;;; org-roam-graph.el --- Graphing API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

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
;; This library provides graphing functionality for org-roam.
;;
;;; Code:
(require 'xml) ;xml-escape-string
(require 's)   ;s-truncate, s-replace
(eval-and-compile
  (require 'org-roam-macs))
(require 'org-roam-db)

;;;; Declarations
(defvar org-roam-directory)
(defvar org-roam-mode)
(declare-function org-roam--org-roam-file-p  "org-roam")
(declare-function org-roam--path-to-slug     "org-roam")
(declare-function org-roam-mode              "org-roam")

;;;; Options
(defcustom org-roam-graph-viewer (executable-find "firefox")
  "Method to view the org-roam graph.
It may be one of the following:
  - a string representing the path to the executable for viewing the graph.
  - a function accepting a single argument: the graph file path.
  - nil uses `view-file' to view the graph."
  :type '(choice
          (string   :tag "Path to executable")
          (function :tag "Function to display graph" eww-open-file)
          (const    :tag "view-file"))
  :group 'org-roam)

(defcustom org-roam-graph-executable "dot"
  "Path to graphing executable, or its name."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graph-filetype "svg"
  "File type to generate when producing graphs."
  :type 'string
  :group 'org-roam)


(defcustom org-roam-graph-extra-config nil
  "Extra options passed to graphviz.
Example:
 '((\"rankdir\" . \"LR\"))"
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-node-extra-config
  '(("shape"      . "underline")
    ("style"      . "rounded,filled")
    ("fillcolor"  . "#EEEEEE")
    ("color"      . "#C9C9C9")
    ("fontcolor"  . "#111111"))
  "Extra options for graphviz nodes.
Example:
 '((\"color\" . \"skyblue\"))"
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-edge-extra-config
  '(("color" . "#333333"))
  "Extra options for graphviz edges.
Example:
 '((\"dir\" . \"back\"))"
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-edge-cites-extra-config '(("color" . "red"))
  "Extra options for graphviz edges for citation links.
Example:
 '((\"dir\" . \"back\"))"
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-max-title-length 100
  "Maximum length of titles in graph nodes."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-graph-shorten-titles 'truncate
  "Determines how long titles appear in graph nodes.
Recognized values are the symbols `truncate' and `wrap', in which
cases the title will be truncated or wrapped, respectively, if it
is longer than `org-roam-graph-max-title-length'.

All other values including nil will have no effect."
  :type '(choice
          (const :tag "truncate" truncate)
          (const :tag "wrap" wrap)
          (const :tag "no" nil))
  :group 'org-roam)

(defcustom org-roam-graph-exclude-matcher nil
  "Matcher for excluding nodes from the generated graph.
Any nodes and links for file paths matching this string is
excluded from the graph.

If value is a string, the string is the only matcher.

If value is a list, all file paths matching any of the strings
are excluded."
  :type '(choice
          (string :tag "Matcher")
          (list :tag "Matchers"))
  :group 'org-roam)

;;;; Functions
(defun org-roam-graph--expand-matcher (col &optional negate where)
  "Return the exclusion regexp from `org-roam-graph-exclude-matcher'.
COL is the symbol to be matched against.  if NEGATE, add :not to sql query.
set WHERE to true if WHERE query already exists."
  (let ((matchers (pcase org-roam-graph-exclude-matcher
                    ('nil nil)
                    ((pred stringp) `(,(concat "%" org-roam-graph-exclude-matcher "%")))
                    ((pred listp) (mapcar (lambda (m)
                                            (concat "%" m "%"))
                                          org-roam-graph-exclude-matcher))
                    (_ (error "Invalid org-roam-graph-exclude-matcher"))))
        res)
    (dolist (match matchers)
      (if where
          (push :and res)
        (push :where res)
        (setq where t))
      (push col res)
      (when negate
        (push :not res))
      (push :like res)
      (push match res))
    (nreverse res)))

(defun org-roam-graph--dot-option (option &optional wrap-key wrap-val)
  "Return dot string of form KEY=VAL for OPTION cons.
If WRAP-KEY is non-nil it wraps the KEY.
If WRAP-VAL is non-nil it wraps the VAL."
  (concat wrap-key (car option) wrap-key
          "="
          wrap-val (cdr option) wrap-val))

(defun org-roam-graph--dot (node-query)
  "Build the graphviz dot string for NODE-QUERY.
The Org-roam database titles table is read, to obtain the list of titles.
The links table is then read to obtain all directed links, and formatted
into a digraph."
  (defun ensure-file-ext (file)
    (catch 'pass
      (unless org-roam-graph-url-extension
        (throw 'pass file)))
    (let ((sans-ext (file-name-sans-extension file)))
      (concat sans-ext "." org-roam-graph-url-extension)))
  (defun ensure-edge-ext (edge)
    `(,(ensure-file-ext (car edge)) ,(ensure-file-ext (cadr edge))))
  (org-roam-db--ensure-built)
  (org-roam--with-temp-buffer nil
    (let* ((nodes (org-roam-db-query node-query))
           (edges-query
            `[:with selected :as [:select [file] :from ,node-query]
              :select :distinct [dest source] :from links
              :where (and (in dest selected) (in source selected))])
           (edges-cites-query
            `[:with selected :as [:select [file] :from ,node-query]
              :select :distinct [file source]
              :from links :inner :join refs :on (and (= links:dest refs:ref)
                                                     (= links:type "cite")
                                                     (= refs:type "cite"))
              :where (and (in file selected) (in source selected))])
           (edges       (org-roam-db-query edges-query))
           (edges-cites (org-roam-db-query edges-cites-query)))
      (insert "digraph \"org-roam\" {\n")
      (dolist (option org-roam-graph-extra-config)
        (insert (org-roam-graph--dot-option option) ";\n"))
      (dolist (attribute '("node" "edge"))
        (insert (format " %s [%s];\n" attribute
                        (mapconcat (lambda (var)
                                     (org-roam-graph--dot-option var nil "\""))
                                   (symbol-value
                                    (intern (concat "org-roam-graph-" attribute "-extra-config")))
                                   ","))))
      (dolist (node nodes)
        (let* ((file (ensure-file-ext (car node)))
               (file (xml-escape-string file))
               (title (or (cadr node)
                          (org-roam--path-to-slug file)))
               (shortened-title (pcase org-roam-graph-shorten-titles
                                  (`truncate (s-truncate org-roam-graph-max-title-length title))
                                  (`wrap (s-word-wrap org-roam-graph-max-title-length title))
                                  (_ title)))
               (shortened-title (org-roam-string-quote shortened-title))
               (title (org-roam-string-quote title))
               (url (if org-roam-graph-url-prefix
                        (let* ((prefix org-roam-graph-url-prefix)
                               (roam-dir (expand-file-name org-roam-directory))
                               (file (replace-regexp-in-string roam-dir "" file)))
                          (concat prefix file))
                      (concat "org-protocol://roam-file?file=" (url-hexify-string file))))
               (node-properties
                `(("label"   . ,shortened-title)
                  ("URL"     . ,url)
                  ("tooltip" . ,(xml-escape-string title)))))

          (insert
           (format "  \"%s\" [%s];\n" file
                   (mapconcat (lambda (n)
                                (org-roam-graph--dot-option n nil "\""))
                              node-properties ",")))))
      (dolist (edge edges)
        (insert (apply #'format `("  \"%s\" -> \"%s\";\n"
                                  ,@(mapcar #'xml-escape-string (ensure-edge-ext edge))))))
      (insert (format "  edge [%s];\n"
                      (mapconcat #'org-roam-graph--dot-option
                                 org-roam-graph-edge-cites-extra-config ",")))
      (dolist (edge edges-cites)
        (insert (apply #'format `("  \"%s\" -> \"%s\";\n"
                                  ,@(mapcar #'xml-escape-string (ensure-edge-ext edge))))))
      (insert "}")
      (buffer-string))))

(defun org-roam-graph--build (&optional node-query callback)
  "Generate a graph showing the relations between nodes in NODE-QUERY.
Execute CALLBACK when process exits successfully.
CALLBACK is passed the graph file as its sole argument."
  (unless (stringp org-roam-graph-executable)
    (user-error "`org-roam-graph-executable' is not a string"))
  (unless (executable-find org-roam-graph-executable)
    (user-error (concat "Cannot find executable \"%s\" to generate the graph.  "
                        "Please adjust `org-roam-graph-executable'")
                org-roam-graph-executable))
  (let* ((node-query (or node-query
                         `[:select [file title] :from titles
                           ,@(org-roam-graph--expand-matcher 'file t)
                           :group :by file]))
         (graph      (org-roam-graph--dot node-query))
         (temp-dot   (make-temp-file "graph." nil ".dot" graph))
         (temp-graph (make-temp-file "graph." nil (concat "." org-roam-graph-filetype))))
    (org-roam-message "building graph")
    (make-process
     :name "*org-roam-graph--build-process*"
     :buffer "*org-roam-graph--build-process*"
     :command `(,org-roam-graph-executable ,temp-dot "-T" ,org-roam-graph-filetype "-o" ,temp-graph)
     :sentinel (when callback
                 (lambda (process _event)
                   (when (= 0 (process-exit-status process))
                     (funcall callback temp-graph)))))))

(defun org-roam-graph--open (file)
  "Open FILE using `org-roam-graph-viewer' with `view-file' as a fallback."
  (pcase org-roam-graph-viewer
    ((pred stringp)
     (if (executable-find org-roam-graph-viewer)
         (condition-case err
             (call-process org-roam-graph-viewer nil 0 nil file)
           (error (user-error "Failed to open org-roam graph: %s" err)))
       (user-error "Executable not found: \"%s\"" org-roam-graph-viewer)))
    ((pred functionp) (funcall org-roam-graph-viewer file))
    ('nil (view-file file))
    (_ (signal 'wrong-type-argument `((functionp stringp null) ,org-roam-graph-viewer)))))

(defun org-roam-graph--build-connected-component (file &optional max-distance callback)
  "Build a graph of nodes connected to FILE.
If MAX-DISTANCE is non-nil, limit nodes to MAX-DISTANCE steps.
CALLBACK is passed to `org-roam-graph--build'."
  (let* ((file (expand-file-name file))
         (files (or (if (and max-distance (>= max-distance 0))
                        (org-roam-db--links-with-max-distance file max-distance)
                      (org-roam-db--connected-component file))
                    (list file)))
         (query `[:select [file title]
                  :from titles
                  :where (in file [,@files])]))
    (org-roam-graph--build query callback)))

;;;; Commands
;;;###autoload
(defun org-roam-graph (&optional arg file node-query)
  "Build and possibly display a graph for FILE from NODE-QUERY.
If FILE is nil, default to current buffer's file name.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for FILE.
  - `\\[universal-argument]' N   show the graph for FILE limiting nodes to N steps.
  - `\\[universal-argument] \\[universal-argument]' build the graph.
  - `\\[universal-argument]' -   build the graph for FILE.
  - `\\[universal-argument]' -N  build the graph for FILE limiting nodes to N steps."
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  (let ((file (or file (buffer-file-name (buffer-base-buffer)))))
    (unless (or (not arg) (equal arg '(16)))
      (unless file
        (user-error "Cannot build graph for nil file. Is current buffer visiting a file?"))
      (unless (org-roam--org-roam-file-p file)
        (user-error "\"%s\" is not an org-roam file" file)))
    (pcase arg
      ('nil            (org-roam-graph--build node-query #'org-roam-graph--open))
      ('(4)            (org-roam-graph--build-connected-component file nil #'org-roam-graph--open))
      ((pred integerp) (org-roam-graph--build-connected-component file (abs arg) (when (>= arg 0) #'org-roam-graph--open)))
      ('(16)           (org-roam-graph--build node-query))
      ('-              (org-roam-graph--build-connected-component file))
      (_ (user-error "Unrecognized ARG: %s" arg)))))

(provide 'org-roam-graph)

;;; org-roam-graph.el ends here
