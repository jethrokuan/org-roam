;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2021 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0") (magit-section "2.90.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library is an attempt at injecting Roam functionality into Org-mode.
;; This is achieved primarily through building caches for forward links,
;; backward links, and file titles.
;;
;;
;;; Code:
;;;; Dependencies
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'ob-core) ;for org-babel-parse-header-arguments
(require 'ansi-color) ; org-roam--list-files strip ANSI color codes
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'rx)
(require 'seq)
(require 'magit-section)
(eval-when-compile (require 'subr-x))

;;;; Features
(require 'org-roam-migrate)
(require 'org-roam-compat)
(eval-when-compile
  (require 'org-roam-macs)
  (require 'org-macs))
(require 'org-roam-utils)
(require 'org-roam-mode)
(require 'org-roam-completion)
(require 'org-roam-capture)
(require 'org-roam-dailies)
(require 'org-roam-db)

;;; Declarations
;; From org-ref-core.el
(defvar org-ref-cite-types)
(declare-function org-ref-split-and-strip-string "ext:org-ref-utils" (string))
;; From org-id.el
(declare-function org-id-find-id-in-file "ext:org-id" (id file &optional markerp))

(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/org-roam/org-roam")
  :link '(url-link :tag "Online Manual" "https://www.orgroam.com/manual.html"))

(defgroup org-roam-faces nil
  "Faces used by Org-roam."
  :group 'org-roam
  :group 'faces)

;;;; Variables
(defcustom org-roam-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Default path to Org-roam files.
All Org files, at any level of nesting, are considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-file-extensions '("org")
  "Detected file extensions to include in the Org-roam ecosystem.
The first item in the list is used as the default file extension.
While the file extensions may be different, the file format needs
to be an `org-mode' file, and it is the user's responsibility to
ensure that."
  :type '(repeat string)
  :group 'org-roam)

(defcustom org-roam-file-exclude-regexp nil
  "Files matching this regular expression are excluded from the Org-roam."
  :type '(choice
          (string :tag "Regular expression matching files to ignore")
          (const :tag "Include everything" nil))
  :group 'org-roam)

(defcustom org-roam-list-files-commands
  (if (member system-type '(windows-nt ms-dos cygwin))
      nil
    '(find fd fdfind rg))
  "Commands that will be used to find Org-roam files.

It should be a list of symbols or cons cells representing any of the following
supported file search methods.

The commands will be tried in order until an executable for a command is found.
The Elisp implementation is used if no command in the list is found.

  `find'
    Use find as the file search method.
    Example command:
    find /path/to/dir -type f \( -name \"*.org\" -o -name \"*.org.gpg\" \)

  `fd'
    Use fd as the file search method.
    Example command: fd /path/to/dir/ --type file -e \".org\" -e \".org.gpg\"

  `fdfind'
    Same as `fd'. It's an alias that used in some OSes (e.g. Debian, Ubuntu)

  `rg'
    Use ripgrep as the file search method.
    Example command: rg /path/to/dir/ --files -g \"*.org\" -g \"*.org.gpg\"

By default, `executable-find' will be used to look up the path to the
executable. If a custom path is required, it can be specified together with the
method symbol as a cons cell. For example: '(find (rg . \"/path/to/rg\"))."
  :type '(set (const :tag "find" find)
              (const :tag "rg" rg)))

;;;; ID Utilities
(defun org-roam-id-at-point ()
  "Return the ID at point, if any.
Recursively traverses up the headline tree to find the
first encapsulating ID."
  (org-with-wide-buffer
   (org-back-to-heading-or-point-min)
   (while (and (not (org-roam-db-node-p))
               (not (bobp)))
     (org-roam-up-heading-or-point-min))
   (org-id-get)))

;;;; File functions and predicates
(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

(defun org-roam-file-p (&optional file)
  "Return t if FILE is part of Org-roam system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (let* ((path (or file (buffer-file-name (buffer-base-buffer))))
         (ext (org-roam--file-name-extension path))
         (ext (if (string= ext "gpg")
                  (org-roam--file-name-extension (file-name-sans-extension path))
                ext)))
    (save-match-data
      (and
       (member ext org-roam-file-extensions)
       (not (and org-roam-file-exclude-regexp
                 (string-match-p org-roam-file-exclude-regexp path)))
       (f-descendant-of-p path (expand-file-name org-roam-directory))))))

(defun org-roam--shell-command-files (cmd)
  "Run CMD in the shell and return a list of files. If no files are found, an empty list is returned."
  (--> cmd
       (shell-command-to-string it)
       (ansi-color-filter-apply it)
       (split-string it "\n")
       (seq-filter #'s-present? it)))

(defun org-roam--list-files-search-globs (exts)
  "Given EXTS, return a list of search globs.
E.g. (\".org\") => (\"*.org\" \"*.org.gpg\")"
  (cl-loop for e in exts
           append (list (format "\"*.%s\"" e)
                        (format "\"*.%s.gpg\"" e))))

(defun org-roam--list-files-fd (executable dir)
  "Return all Org-roam files located recursively within DIR, using fd, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (extensions (s-join " -e " (mapcar (lambda (glob) (substring glob 2 -1)) globs)))
         (command (s-join " " `(,executable "-L" ,dir "--type file" ,extensions))))
    (org-roam--shell-command-files command)))

(defalias 'org-roam--list-files-fdfind #'org-roam--list-files-fd)

(defun org-roam--list-files-rg (executable dir)
  "Return all Org-roam files located recursively within DIR, using ripgrep, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (command (s-join " " `(,executable "-L" ,dir "--files"
                                            ,@(mapcar (lambda (glob) (concat "-g " glob)) globs)))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-find (executable dir)
  "Return all Org-roam files located recursively within DIR, using find, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (names (s-join " -o " (mapcar (lambda (glob) (concat "-name " glob)) globs)))
         (command (s-join " " `(,executable "-L" ,dir "-type f \\(" ,names "\\)"))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-elisp (dir)
  "Return all Org-roam files located recursively within DIR, using elisp."
  (let ((regex (concat "\\.\\(?:"(mapconcat
                                  #'regexp-quote org-roam-file-extensions
                                  "\\|" )"\\)\\(?:\\.gpg\\)?\\'"))
        result)
    (dolist (file (directory-files-recursively dir regex nil nil t) result)
      (when (and (file-readable-p file)
                 (org-roam-file-p file))
        (push file result)))))

(defun org-roam--list-files (dir)
  "Return all Org-roam files located recursively within DIR.
Use external shell commands if defined in `org-roam-list-files-commands'."
  (let (path exe)
    (cl-dolist (cmd org-roam-list-files-commands)
      (pcase cmd
        (`(,e . ,path)
         (setq path (executable-find path)
               exe  (symbol-name e)))
        ((pred symbolp)
         (setq path (executable-find (symbol-name cmd))
               exe (symbol-name cmd)))
        (wrong-type
         (signal 'wrong-type-argument
                 `((consp symbolp)
                   ,wrong-type))))
      (when path (cl-return)))
    (if-let* ((files (when path
                       (let ((fn (intern (concat "org-roam--list-files-" exe))))
                         (unless (fboundp fn) (user-error "%s is not an implemented search method" fn))
                         (funcall fn path (format "\"%s\"" dir)))))
              (files (seq-filter #'org-roam-file-p files))
              (files (mapcar #'expand-file-name files))) ; canonicalize names
        files
      (org-roam--list-files-elisp dir))))

(defun org-roam--list-all-files ()
  "Return a list of all Org-roam files within `org-roam-directory'."
  (org-roam--list-files (expand-file-name org-roam-directory)))

(defun org-roam--nodes-table ()
  "Return a hash table of node ID to org-roam-nodes."
  (let ((ht (make-hash-table :test #'equal)))
    (pcase-dolist (`(,id ,file ,title)
                   (org-roam-db-query [:select [id file title] :from nodes]))
      (puthash id (org-roam-node-create :file file :id id :title title) ht))
    ht))

(defun org-roam-buffer-p (&optional buffer)
  "Return t if BUFFER is accessing a part of Org-roam system.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        path)
    (with-current-buffer buffer
      (and (derived-mode-p 'org-mode)
           (setq path (buffer-file-name (buffer-base-buffer)))
           (org-roam-file-p path)))))

(defun org-roam-buffer-list ()
  "Return a list of buffers that are Org-roam files."
  (--filter (org-roam-buffer-p it)
            (buffer-list)))

(defun org-roam--get-titles ()
  "Return all distinct titles and aliases in the Org-roam database."
  (mapcar #'car (org-roam-db-query [:select :distinct title :from nodes
                                    :union :select alias :from aliases])))

;;; Org-roam setup and teardown
(defvar org-roam-find-file-hook nil
  "Hook run when an Org-roam file is visited.")

;;;###autoload
(defun org-roam-setup ()
  "Setup Org-roam and initialize its database.
This will install the needed hooks and advices to keep everything
in sync with the connected databases."
  (interactive)
  (add-hook 'find-file-hook #'org-roam--file-setup)
  (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-add 'rename-file :after #'org-roam--rename-file-advice)
  (advice-add 'delete-file :before #'org-roam--delete-file-advice)
  (org-roam-db-sync))

(defun org-roam-teardown ()
  "Teardown Org-roam to completely disable it.
This will remove all the hooks and advices installed by
`org-roam-setup' and close all the database connections made by
Org-roam."
  (interactive)
  (remove-hook 'find-file-hook #'org-roam--file-setup)
  (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-remove 'rename-file #'org-roam--rename-file-advice)
  (advice-remove 'delete-file #'org-roam--delete-file-advice)
  (org-roam-db--close-all)
  ;; Disable local hooks for all org-roam buffers
  (dolist (buf (org-roam-buffer-list))
    (with-current-buffer buf
      (remove-hook 'after-save-hook #'org-roam-db-update-file t))))

;;; Hooks and advices
(defun org-roam--file-setup ()
  "Setup an Org-roam file."
  (when (org-roam-file-p)
    (run-hooks 'org-roam-find-file-hook)))

(defun org-roam--delete-file-advice (file &optional _trash)
  "Maintain cache consistency when file deletes.
FILE is removed from the database."
  (when (and (not (auto-save-file-name-p file))
             (not (backup-file-name-p file))
             (org-roam-file-p file))
    (org-roam-db-clear-file (expand-file-name file))))

(defun org-roam--rename-file-advice (old-file new-file-or-dir &rest _args)
  "Maintain cache consistency of file rename.
OLD-FILE is cleared from the database, and NEW-FILE-OR-DIR is added."
  (let ((new-file (if (directory-name-p new-file-or-dir)
                      (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                    new-file-or-dir)))
    (setq new-file (expand-file-name new-file))
    (setq old-file (expand-file-name old-file))
    (when (and (not (auto-save-file-name-p old-file))
               (not (auto-save-file-name-p new-file))
               (not (backup-file-name-p old-file))
               (not (backup-file-name-p new-file))
               (org-roam-file-p old-file))
      (org-roam-db-clear-file old-file))
    (when (org-roam-file-p new-file)
      (org-roam-db-update-file new-file))))

;;;; Nodes
(cl-defstruct (org-roam-node (:constructor org-roam-node-create)
                             (:copier nil))
  "A heading or top level file with an assigned ID property."
  file file-hash file-atime file-mtime
  id level point todo priority scheduled deadline title properties olp
  tags aliases refs)

(cl-defmethod org-roam-node-slug ((node org-roam-node))
  "Return the slug of NODE."
  (let ((title (org-roam-node-title node))
        (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char)
                                  (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                                       (ucs-normalize-NFC-string
                                        (apply #'string (seq-remove #'nonspacing-mark-p
                                                                    (ucs-normalize-NFD-string s)))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                      ("__*" . "_")  ;; remove sequential underscores
                      ("^_" . "")    ;; remove starting underscore
                      ("_$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

(defvar org-roam-node-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-node-visit)
    map)
  "Keymap for `org-roam-node-section's.")

(defclass org-roam-node-section (magit-section)
  ((keymap :initform 'org-roam-node-map)
   (node :initform nil))
  "A `magit-section' used by `org-roam-mode' to contain heading for NODE.")

(defvar org-roam-preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-preview-visit)
    map)
  "Keymap for `org-roam-preview-section's.")

(defclass org-roam-preview-section (magit-section)
  ((keymap :initform 'org-roam-preview-map)
   (file :initform nil)
   (begin :initform nil)
   (end :initform nil))
  "A `magit-section' used by `org-roam-mode' to contain preview content.
The preview content comes from FILE, between the next locations:
BEGIN and END.")

(cl-defmethod org-roam-populate ((node org-roam-node))
  "Populate NODE from database.
Uses the ID, and fetches remaining details from the database.
This can be quite costly: avoid, unless dealing with very few
nodes."
  (when-let ((node-info (car (org-roam-db-query [:select [file level pos todo priority
                                                          scheduled deadline title properties olp]
                                                 :from nodes
                                                 :where (= id $s1)
                                                 :limit 1]
                                                (org-roam-node-id node)))))
    (pcase-let* ((`(,file ,level ,pos ,todo ,priority ,scheduled ,deadline ,title ,properties ,olp) node-info)
                 (`(,atime ,mtime) (car (org-roam-db-query [:select [atime mtime]
                                                            :from files
                                                            :where (= file $s1)]
                                                           file)))
                 (tag-info (mapcar #'car (org-roam-db-query [:select [tag] :from tags
                                                             :where (= node-id $s1)]
                                                            (org-roam-node-id node))))
                 (alias-info (mapcar #'car (org-roam-db-query [:select [alias] :from aliases
                                                               :where (= node-id $s1)]
                                                              (org-roam-node-id node))))
                 (refs-info (mapcar #'car (org-roam-db-query [:select [ref] :from refs
                                                              :where (= node-id $s1)]
                                                             (org-roam-node-id node)))))
      (setf (org-roam-node-file node) file
            (org-roam-node-file-atime node) atime
            (org-roam-node-file-mtime node) mtime
            (org-roam-node-level node) level
            (org-roam-node-point node) pos
            (org-roam-node-todo node) todo
            (org-roam-node-priority node) priority
            (org-roam-node-scheduled node) scheduled
            (org-roam-node-deadline node) deadline
            (org-roam-node-title node) title
            (org-roam-node-properties node) properties
            (org-roam-node-olp node) olp
            (org-roam-node-tags node) tag-info
            (org-roam-node-refs node) refs-info
            (org-roam-node-aliases node) alias-info)))
  node)

(defcustom org-roam-node-display-template
  "${title:*} ${tags:10}"
  "Configures display formatting for Org-roam node.
Patterns of form \"${field-name:length}\" are interpolated based
on the current node.

Each \"field-name\" is replaced with the return value of each
corresponding accessor function for `org-roam-node', e.g.
\"${title}\" will be interpolated by the result of
`org-roam-node-title'. You can also define custom accessors using
`cl-defmethod'. For example, you can define:

  (cl-defmethod org-roam-node-my-title ((node org-roam-node))
    (concat \"My \" (org-roam-node-title node)))

and then reference it here or in the capture templates as
\"${my-title}\".

\"length\" is an optional specifier and declares how many
characters can be used to display the value of the corresponding
field. If it's not specified, the field will be inserted as is,
i.e. it won't be aligned nor trimmed. If it's an integer, the
field will be aligned accordingly and all the exceeding
characters will be trimmed out. If it's \"*\", the field will use
as many characters as possible and will be aligned accordingly."
  :group 'org-roam
  :type  'string)

(defun org-roam--tags-to-str (tags)
  "Convert list of TAGS into a string."
  (mapconcat (lambda (s) (concat "#" s)) tags " "))

(defun org-roam-node--format-entry (node width)
  "Formats NODE for display in the results list.
WIDTH is the width of the results list.
Uses `org-roam-node-display-template' to format the entry."
  (let ((fmt (org-roam--process-display-format org-roam-node-display-template)))
    (org-roam-format
     (car fmt)
     (lambda (field)
       (let* ((field (split-string field ":"))
              (field-name (car field))
              (field-width (cadr field))
              (getter (intern (concat "org-roam-node-" field-name)))
              (field-value (or (funcall getter node) "")))
         (when (and (equal field-name "tags")
                    field-value)
           (setq field-value (org-roam--tags-to-str field-value)))
         (when (and (equal field-name "file")
                    field-value)
           (setq field-value (file-relative-name field-value org-roam-directory)))
         (when (and (equal field-name "olp")
                    field-value)
           (setq field-value (string-join field-value " > ")))
         (if (not field-width)
             field-value
           (setq field-width (string-to-number field-width))
           (truncate-string-to-width
            field-value
            (if (> field-width 0)
                field-width
              (- width (cdr fmt)))
            0 ?\s)))))))

(defun org-roam-node-preview (file point)
  "Get preview content for FILE at POINT."
  (save-excursion
    (org-roam-with-temp-buffer file
      (goto-char point)
      (let* ((elem (org-element-at-point))
             (begin (org-element-property :begin elem))
             (end (org-element-property :end elem)))
        (list begin end
              (or (string-trim (buffer-substring-no-properties begin end))
                  (org-element-property :raw-value elem)))))))

(defun org-roam-node-at-point (&optional assert)
  "Return the node at point.
If ASSERT, throw an error if there is no node at point.
This function also returns the node if it has yet to be cached in the
database. In this scenario, only expect `:id' and `:point' to be
populated."
  (if-let ((node (magit-section-case
                   (org-roam-node-section (oref it node))
                   (t (org-with-wide-buffer
                       (org-back-to-heading-or-point-min)
                       (while (and (not (org-roam-db-node-p))
                                   (not (bobp)))
                         (org-roam-up-heading-or-point-min))
                       (when-let ((id (org-id-get)))
                         (org-roam-populate
                          (org-roam-node-create
                           :id id
                           :point (point)))))))))
      node
    (when assert
      (user-error "No node at point"))))

(defun org-roam-node--find (node)
  "Navigate to the point for NODE, and return the buffer."
  (unless (org-roam-node-file node)
    (user-error "Node does not have corresponding file"))
  (let ((buf (find-file-noselect (org-roam-node-file node))))
    (with-current-buffer buf
      (goto-char (org-roam-node-point node)))
    buf))

(defun org-roam-node-visit (node &optional other-window)
  "From the current buffer, visit NODE.

Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead."
  (interactive (list (org-roam-node-at-point t) current-prefix-arg))
  (let ((buf (org-roam-node--find node)))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(defun org-roam-node-from-id (id)
  "Return an `org-roam-node' for the node containing ID.
Return nil if a node with ID does not exist."
  (when (> (caar (org-roam-db-query [:select (funcall count) :from nodes
                                     :where (= id $s1)]
                                    id)) 0)
    (org-roam-populate (org-roam-node-create :id id))))

(defun org-roam-node-from-title-or-alias (s)
  "Return an `org-roam-node' for the node with title or alias S.
Return nil if the node does not exist.
Throw an error if multiple choices exist."
  (let ((matches (seq-uniq
                  (append
                   (org-roam-db-query [:select [id] :from nodes
                                       :where (= title $s1)]
                                      s)
                   (org-roam-db-query [:select [node-id] :from aliases
                                       :where (= alias $s1)]
                                      s)))))
    (cond
     ((seq-empty-p matches)
      nil)
     ((= 1 (length matches))
      (org-roam-populate (org-roam-node-create :id (caar matches))))
     (t
      (user-error "Multiple nodes exist with title or alias \"%s\"" s)))))

(defun org-roam-node-list ()
  "Return all nodes stored in the database as a list of `org-roam-node's."
  (let ((rows (org-roam-db-query
               "SELECT
  id,
  file,
  \"level\",
  todo,
  pos,
  priority ,
  scheduled ,
  deadline ,
  title,
  properties ,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  refs
FROM
  (
  SELECT
    id,
    file,
    \"level\",
    todo,
    pos,
    priority ,
    scheduled ,
    deadline ,
    title,
    properties ,
    olp,
    atime,
    mtime,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    refs
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.file as file,
      nodes.\"level\" as \"level\",
      nodes.todo as todo,
      nodes.pos as pos,
      nodes.priority as priority,
      nodes.scheduled as scheduled,
      nodes.deadline as deadline,
      nodes.title as title,
      nodes.properties as properties,
      nodes.olp as olp,
      files.atime as atime,
      files.mtime as mtime,
      tags.tag as tags,
      aliases.alias as aliases,
      '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
    FROM nodes
    LEFT JOIN files ON files.file = nodes.file
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    LEFT JOIN refs ON refs.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,level ,todo ,pos ,priority ,scheduled ,deadline
                                        ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row)
                                 (all-titles (cons title aliases)))
                      (mapcar (lambda (temp-title)
                                (org-roam-node-create :id id
                                                      :file file
                                                      :file-atime atime
                                                      :file-mtime mtime
                                                      :level level
                                                      :point pos
                                                      :todo todo
                                                      :priority priority
                                                      :scheduled scheduled
                                                      :deadline deadline
                                                      :title temp-title
                                                      :properties properties
                                                      :olp olp
                                                      :tags tags
                                                      :refs refs))
                              all-titles)))))

(defun org-roam-node--to-candidate (node)
  "Return a minibuffer completion candidate given NODE."
  (let ((candidate-main (org-roam-node--format-entry node (1- (frame-width))))
        (tag-str (org-roam--tags-to-str (org-roam-node-tags node))))
    (cons (propertize (concat candidate-main
                              (propertize tag-str 'invisible t))
                      'node node)
          node)))

(defun org-roam-node--completions ()
  "Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is the `org-roam-node'.
The displayed title is formatted according to `org-roam-node-display-template'."
  (setq org-roam--cached-display-format nil)
  (let ((nodes (org-roam-node-list)))
    (mapcar #'org-roam-node--to-candidate nodes)))

(defcustom org-roam-node-annotation-function #'org-roam-node--annotation
  "The function used to return annotations in the minibuffer for Org-roam nodes.
This function takes a single argument NODE, which is an `org-roam-node' construct."
  :group 'org-roam
  :type 'function)

(defcustom org-roam-node-default-sort 'file-mtime
  "Default sort order for Org-roam node completions."
  :type 'const
  :group 'org-roam)

(defun org-roam-node-sort-by-file-mtime (completion-a completion-b)
  "Sort files such that files modified more recently are shown first.
COMPLETION-A and COMPLETION-B are items in the form of (node-title org-roam-node-struct)"
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (time-less-p (org-roam-node-file-mtime node-b)
                 (org-roam-node-file-mtime node-a))))

(defun org-roam-node-sort-by-file-atime (completion-a completion-b)
  "Sort files such that files accessed more recently are shown first.
COMPLETION-A and COMPLETION-B are items in the form of (node-title org-roam-node-struct)"
  "Sort completions list by file modification time."
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (time-less-p (org-roam-node-file-atime node-b)
                 (org-roam-node-file-atime node-a))))

(defun org-roam-node-read (&optional initial-input filter-fn sort-fn require-match)
  "Read and return an `org-roam-node'.
INITIAL-INPUT is the initial minibuffer prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
SORT-FN is a function to sort nodes. See `org-roam-node-sort-by-file-mtime'
for an example sort function.
If REQUIRE-MATCH, the minibuffer prompt will require a match."
  (let* ((nodes (org-roam-node--completions))
         (nodes (cl-remove-if-not (lambda (n)
                                    (if filter-fn (funcall filter-fn (cdr n)) t)) nodes))
         (sort-fn (or sort-fn
                      (when org-roam-node-default-sort
                        (intern (concat "org-roam-node-sort-by-" (symbol-name org-roam-node-default-sort))))))
         (_ (when sort-fn (setq nodes (seq-sort sort-fn nodes))))
         (node (completing-read
                "Node: "
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (annotation-function . (lambda (title)
                                                 (funcall org-roam-node-annotation-function
                                                          (get-text-property 0 'node title))))
                        (category . org-roam-node))
                    (complete-with-action action nodes string pred)))
                nil require-match initial-input)))
    (or (cdr (assoc node nodes))
        (org-roam-node-create :title node))))

(defun org-roam-node--annotation (_node)
  "Dummy function.
Returns empty string for annotations."
  "")

(defun org-roam-preview-visit (file point &optional other-window)
  "Visit FILE at POINT.
With prefix argument OTHER-WINDOW, visit the olp in another
window instead."
  (interactive (list (org-roam-file-at-point t)
                     (oref (magit-current-section) begin)
                     current-prefix-arg))
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (goto-char point))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(cl-defun org-roam-node-insert-section (&key source-node point properties)
  "Insert section for a link from SOURCE-NODE to some other node.

SOURCE-NODE is an `org-roam-node' that links or references some
other node. Normally the other node is `org-roam-current-node'
that set in `org-roam-buffer'.

POINT is the position in SOURCE-NODE's file where the link is
located.

PROPERTIES (a plist) contains additional information about the
link.

This section is made out of the next 2 `magit-section's:
1. `org-roam-node-section' for a heading that describes
   SOURCE-NODE.

2. `org-roam-preview-section' for a preview content that comes
   from SOURCE-NODE's file for the link (that references the
   other node) at POINT."
  (magit-insert-section section (org-roam-node-section)
    (let ((outline (if-let ((outline (plist-get properties :outline)))
                       (mapconcat #'org-link-display-format outline " > ")
                     "Top")))
      (insert (concat (propertize (org-roam-node-title source-node)
                                  'font-lock-face 'org-roam-title)
                      (format " (%s)"
                              (propertize outline 'font-lock-face 'org-roam-olp)))))
    (magit-insert-heading)
    (oset section node source-node)
    (magit-insert-section section (org-roam-preview-section)
      (pcase-let ((`(,begin ,end ,s) (org-roam-node-preview (org-roam-node-file source-node)
                                                            point)))
        (insert (org-roam-fontify-like-in-org-mode s) "\n")
        (oset section file (org-roam-node-file source-node))
        (oset section begin begin)
        (oset section end end))
      (insert ?\n))))

;;;###autoload
(defun org-roam-node-find (&optional other-window initial-input filter-fn)
  "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window."
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (org-roam-capture-
       :node node
       :props '(:finalize find-file)))))

;;;###autoload
(defun org-roam-node-insert (&optional filter-fn)
  "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node (org-roam-node-read region-text filter-fn))
               (description (or region-text
                                (org-roam-node-title node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description)))
            (org-roam-capture-
             :node node
             :props (append
                     (when (and beg end)
                       (list :region (cons beg end)))
                     (list :insert-at (point-marker)
                           :link-description description
                           :finalize 'insert-link))))))
    (deactivate-mark)))

;;;###autoload
(defun org-roam-node-random (&optional other-window)
  "Find and open a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead."
  (interactive current-prefix-arg)
  (let ((random-row (seq-random-elt (org-roam-db-query [:select [id file pos] :from nodes]))))
    (org-roam-node-visit (org-roam-node-create :id (nth 0 random-row)
                                               :file (nth 1 random-row)
                                               :point (nth 2 random-row))
                         other-window)))

;;;; Properties
(defun org-roam-add-property (val prop)
  "Add VAL value to PROP property for the node at point.
Both, VAL and PROP are strings."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (lst (if (memq val lst) lst (cons val lst)))
         (lst (seq-uniq lst)))
    (org-set-property prop (combine-and-quote-strings lst))
    val))

(defun org-roam-remove-property (prop &optional val)
  "Remove VAL value from PROP property for the node at point.
Both VAL and PROP are strings.

If VAL is not specified, user is prompted to select a value."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (prop-to-remove (or val (completing-read "Remove: " lst)))
         (lst (delete prop-to-remove lst)))
    (if lst
        (org-set-property prop (combine-and-quote-strings lst))
      (org-delete-property prop))
    prop-to-remove))

(defun org-roam-set-keyword (key value)
  "Set keyword KEY to VALUE.
If the property is already set, it's value is replaced."
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") (point-max) t)
          (if (string-blank-p value)
              (kill-whole-line)
            (replace-match (concat " " value) 'fixedcase nil nil 1))
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" key ": " value "\n")))))

;;;; Tags
(defun org-roam-tag-completions ()
  "Return list of tags for completions within Org-roam."
  (let ((roam-tags (mapcar #'car (org-roam-db-query [:select :distinct [tag] :from tags])))
        (org-tags (cl-loop for tagg in org-tag-alist
                           nconc (pcase tagg
                                   ('(:newline)
                                    nil)
                                   (`(,tag . ,_)
                                    (list tag))
                                   (_ nil)))))
    (seq-uniq (append roam-tags org-tags))))

(defun org-roam-tag-add (tags)
  "Add TAGS to the node at point."
  (interactive
   (list (completing-read-multiple "Tag: " (org-roam-tag-completions))))
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (if (= (org-outline-level) 0)
          (let ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                             (org-collect-keywords '("filetags"))))
                                                ""))))
            (org-roam-set-keyword "filetags" (string-join (seq-uniq (append tags current-tags)) " ")))
        (org-set-tags (seq-uniq (append tags (org-get-tags)))))
      tags)))

(defun org-roam-tag-remove (&optional tags)
  "Remove TAGS from the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (if (= (org-outline-level) 0)
          (let* ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                              (org-collect-keywords '("filetags"))))
                                                 (user-error "No tag to remove"))))
                 (tags (or tags (completing-read-multiple "Tag: " current-tags))))
            (org-roam-set-keyword "filetags"
                                  (string-join (seq-difference current-tags tags #'string-equal) " ")))
        (let* ((current-tags (or (org-get-tags)
                                 (user-error "No tag to remove")))
               (tags (completing-read-multiple "Tag: " current-tags)))
          (org-set-tags (seq-difference current-tags tags #'string-equal))))
      tags)))

;;;; Aliases
(defun org-roam-alias-add (alias)
  "Add ALIAS to the node at point."
  (interactive "sAlias: ")
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-add-property alias "ROAM_ALIASES"))))

(defun org-roam-alias-remove (&optional alias)
  "Remove an ALIAS from the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-remove-property "ROAM_ALIASES" alias))))

;;;; Refs
(defun org-roam-ref-add (ref)
  "Add REF to the node at point."
  (interactive "sRef: ")
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-add-property ref "ROAM_REFS"))))

(defun org-roam-ref-remove (&optional ref)
  "Remove a REF from the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-remove-property "ROAM_REFS" ref))))

(defun org-roam-ref--completions ()
  "Return an alist for ref completion.
The car is the ref, and the cdr is the corresponding node for the ref."
  nil
  (let ((rows (org-roam-db-query
               [:select [id ref type nodes:file pos title]
                :from refs
                :left-join nodes
                :on (= refs:node-id nodes:id)])))
    (cl-loop for row in rows
             collect (pcase-let* ((`(,id ,ref ,type ,file ,pos ,title) row)
                                  (node (org-roam-node-create :id id
                                                              :file file
                                                              :point pos
                                                              :title title)))
                       (cons (propertize ref 'node node 'type type)
                             node)))))

(defun org-roam-ref-read (&optional initial-input filter-fn)
  "Read an Org-roam ref.
Return a string, is propertized in `meta' with additional properties.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
filtered out."
  (let* ((refs (org-roam-ref--completions))
         (refs (cl-remove-if-not (lambda (n)
                                   (if filter-fn (funcall filter-fn (cdr n)) t)) refs))
         (ref (completing-read "Ref: "
                               (lambda (string pred action)
                                 (if (eq action 'metadata)
                                     '(metadata
                                       (annotation-function . org-roam-ref--annotation)
                                       (category . org-roam-ref))
                                   (complete-with-action action refs string pred)))
                               nil t initial-input)))
    (cdr (assoc ref refs))))

(defun org-roam-ref--annotation (ref)
  "Return the annotation for REF.
REF is assumed to be a propertized string."
  (let* ((node (get-text-property 0 'node ref))
         (title (org-roam-node-title node)))
    (when title
      (concat " " title))))

;;;###autoload
(defun org-roam-ref-find (&optional initial-input filter-fn)
  "Find and open an Org-roam node that's dedicated to a specific ref.
INITIAL-INPUT is the initial input to the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive)
  (let* ((node (org-roam-ref-read initial-input filter-fn)))
    (find-file (org-roam-node-file node))
    (goto-char (org-roam-node-point node))))

;;;; roam: link
(defcustom org-roam-link-auto-replace t
  "If non-nil, replace \"roam:\" links to existing nodes with \"id:\" links."
  :group 'org-roam
  :type 'boolean)

;;; the roam: link
(org-link-set-parameters "roam" :follow #'org-roam-link-follow-link)

(defun org-roam-link-replace-at-point (&optional link)
  "Replace \"roam:\" LINK at point with an \"id:\" link."
  (save-excursion
    (save-match-data
      (let* ((link (or link (org-element-context)))
             (type (org-element-property :type link))
             (path (org-element-property :path link))
             node)
        (goto-char (org-element-property :begin link))
        (when (and (org-in-regexp org-link-any-re 1)
                   (string-equal type "roam")
                   (setq node (org-roam-node-from-title-or-alias path)))
          (replace-match (org-link-make-string
                          (concat "id:" (org-roam-node-id node))
                          path)))))))

(defun org-roam-link-replace-all ()
  "Replace all \"roam:\" links in buffer with \"id:\" links."
  (interactive)
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (org-roam-link-replace-at-point))))

(defun org-roam--replace-roam-links-on-save-h ()
  "Run `org-roam-link-replace-all' before buffer is saved to its file."
  (when org-roam-link-auto-replace
    (add-hook 'before-save-hook #'org-roam-link-replace-all nil t)))

(add-hook 'org-roam-find-file-hook #'org-roam--replace-roam-links-on-save-h)

(defun org-roam-link-follow-link (title-or-alias)
  "Navigate \"roam:\" link to find and open the node with TITLE-OR-ALIAS.
Assumes that the cursor was put where the link is."
  (if-let ((node (org-roam-node-from-title-or-alias title-or-alias)))
      (progn
        (when org-roam-link-auto-replace
          (org-roam-link-replace-at-point))
        (org-id-goto (org-roam-node-id node)))
    (org-roam-capture-
     :node (org-roam-node-create :title title-or-alias)
     :props '(:finalize find-file))))

(defun org-roam-open-id-at-point ()
  "Try to navigate \"id:\" link to find and visit node with an assigned ID.
Assumes that the cursor was put where the link is."
  (let* ((context (org-element-context))
         (type (org-element-property :type context))
         (id (org-element-property :path context)))
    (when (string= type "id")
      (let ((node (org-roam-populate (org-roam-node-create :id id))))
        (cond
         ((org-roam-node-file node)
          (org-mark-ring-push)
          (org-roam-node-visit node)
          t)
         (t nil))))))

(defun org-roam-open-id-with-org-roam-db-h ()
  "Try to open \"id:\" links at point by querying them to the database."
  (add-hook 'org-open-at-point-functions #'org-roam-open-id-at-point nil t))

(add-hook 'org-roam-find-file-hook #'org-roam-open-id-with-org-roam-db-h)

;;; Refiling
(defun org-roam-demote-entire-buffer ()
  "Convert an org buffer with any top level content to a single node.

All headings are demoted one level.

The #+TITLE: keyword is converted into a level-1 heading and deleted.
Any tags declared on #+FILETAGS: are transferred to tags on the new top heading.

Any top level properties drawers are incorporated into the new heading."
  (interactive)
  (org-with-point-at 1
    (org-map-entries 'org-do-demote)
    (insert "* "
            (org-roam--file-keyword-get "TITLE")
            "\n")
    (org-back-to-heading)
    (org-set-tags (org-roam--file-keyword-get "FILETAGS"))
    (org-roam--file-keyword-kill "TITLE")
    (org-roam--file-keyword-kill "FILETAGS")))

(defun org-roam-refile ()
  "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (region-start (and regionp (region-beginning)))
         (region-end (and regionp (region-end)))
         (node (org-roam-node-read nil nil nil 'require-match))
         (file (org-roam-node-file node))
         (nbuf (or (find-buffer-visiting file)
                   (find-file-noselect file)))
         level reversed)
    (if regionp
        (progn
          (org-kill-new (buffer-substring region-start region-end))
          (org-save-markers-in-region region-start region-end))
      (progn
        (if (org-before-first-heading-p)
            (org-roam-demote-entire-buffer))
        (org-copy-subtree 1 nil t)))
    (with-current-buffer nbuf
      (org-with-wide-buffer
       (goto-char (org-roam-node-point node))
       (setq level (org-get-valid-level (funcall outline-level) 1)
             reversed (org-notes-order-reversed-p))
       (goto-char
        (if reversed
            (or (outline-next-heading) (point-max))
          (or (save-excursion (org-get-next-sibling))
              (org-end-of-subtree t t)
              (point-max))))
       (unless (bolp) (newline))
       (org-paste-subtree level nil nil t)
       (and org-auto-align-tags
            (let ((org-loop-over-headlines-in-active-region nil))
              (org-align-tags)))
       (when (fboundp 'deactivate-mark) (deactivate-mark))))
    (if regionp
        (delete-region (point) (+ (point) (- region-end region-start)))
      (org-preserve-local-variables
       (delete-region
        (and (org-back-to-heading t) (point))
        (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
    (org-roam--kill-empty-buffer)))

(provide 'org-roam)
;;; org-roam.el ends here
