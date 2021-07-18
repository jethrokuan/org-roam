;;; org-roam-dailies.el --- Daily-notes for Org-roam -*- coding: utf-8; lexical-binding: t; -*-
;;;
;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;;      Leo Vivier <leo.vivier+dev@gmail.com>
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
;; This library provides functionality for creating daily-notes. This is a
;; concept borrowed from Roam Research.
;;
;;; Code:
;;; Library Requires
(require 'org-capture)
(require 'org-roam-capture)
(require 'f)

;;;; Declarations
(defvar org-roam-directory)
(defvar org-roam-file-extensions)
(declare-function org-roam-file-p        "org-roam")

;;;; Faces
(defface org-roam-dailies-calendar-note
  '((t :inherit (org-link) :underline nil))
  "Face for dates with a daily-note in the calendar."
  :group 'org-roam-faces)

;;;; Customizable variables
(defcustom org-roam-dailies-directory "daily/"
  "Path to daily-notes.
This path is relative to `org-roam-directory'."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-dailies-find-file-hook nil
  "Hook that is run right after navigating to a daily-note."
  :group 'org-roam
  :type 'hook)

(defcustom org-roam-dailies-capture-templates
  `(("d" "default" entry
     "* %?"
     :if-new (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n")))
  "Capture templates for daily-notes in Org-roam.
See `org-roam-capture-templates' for the template documentation."
  :group 'org-roam
  :type '(repeat
          (choice (list :tag "Multikey description"
                        (string :tag "Keys       ")
                        (string :tag "Description"))
                  (list :tag "Template entry"
                        (string :tag "Keys           ")
                        (string :tag "Description    ")
                        (choice :tag "Capture Type   " :value entry
                                (const :tag "Org entry" entry)
                                (const :tag "Plain list item" item)
                                (const :tag "Checkbox item" checkitem)
                                (const :tag "Plain text" plain)
                                (const :tag "Table line" table-line))
                        (choice :tag "Template       "
                                (string)
                                (list :tag "File"
                                      (const :format "" file)
                                      (file :tag "Template file"))
                                (list :tag "Function"
                                      (const :format "" function)
                                      (function :tag "Template function")))
                        (plist :inline t
                               ;; Give the most common options as checkboxes
                               :options (((const :format "%v " :if-new)
                                          (choice :tag "Node location"
                                                  (list :tag "File"
                                                        (const :format "" file)
                                                        (string :tag "  File"))
                                                  (list :tag "File & Head Content"
                                                        (const :format "" file+head)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content"))
                                                  (list :tag "File & Outline path"
                                                        (const :format "" file+olp)
                                                        (string :tag "  File")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))
                                                  (list :tag "File & Head Content & Outline path"
                                                        (const :format "" file+head+olp)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))))
                                         ((const :format "%v " :prepend) (const t))
                                         ((const :format "%v " :immediate-finish) (const t))
                                         ((const :format "%v " :jump-to-captured) (const t))
                                         ((const :format "%v " :empty-lines) (const 1))
                                         ((const :format "%v " :empty-lines-before) (const 1))
                                         ((const :format "%v " :empty-lines-after) (const 1))
                                         ((const :format "%v " :clock-in) (const t))
                                         ((const :format "%v " :clock-keep) (const t))
                                         ((const :format "%v " :clock-resume) (const t))
                                         ((const :format "%v " :time-prompt) (const t))
                                         ((const :format "%v " :tree-type) (const week))
                                         ((const :format "%v " :unnarrowed) (const t))
                                         ((const :format "%v " :table-line-pos) (string))
                                         ((const :format "%v " :kill-buffer) (const t))))))))

;;;###autoload
(defun org-roam-dailies-find-directory ()
  "Find and open `org-roam-dailies-directory'."
  (interactive)
  (find-file (expand-file-name org-roam-dailies-directory org-roam-directory)))

(defun org-roam-dailies--daily-note-p (&optional file)
  "Return t if FILE is an Org-roam daily-note, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (when-let ((path (expand-file-name
                    (or file
                        (buffer-file-name (buffer-base-buffer)))))
             (directory (expand-file-name org-roam-dailies-directory org-roam-directory)))
    (setq path (expand-file-name path))
    (save-match-data
      (and
       (org-roam-file-p path)
       (f-descendant-of-p path directory)))))

(defun org-roam-dailies--capture (time &optional goto)
  "Capture an entry in a daily-note for TIME, creating it if necessary.
When GOTO is non-nil, go the note without creating an entry."
  (let ((org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory)))
    (org-roam-capture- :goto (when goto '(4))
                       :node (org-roam-node-create)
                       :templates org-roam-dailies-capture-templates
                       :props (list :override-default-time time)))
  (when goto (run-hooks 'org-roam-dailies-find-file-hook)))

;;;; Commands
;;; Today
;;;###autoload
(defun org-roam-dailies-capture-today (&optional goto)
  "Create an entry in the daily-note for today.
When GOTO is non-nil, go the note without creating an entry."
  (interactive "P")
  (org-roam-dailies--capture (current-time) goto))

;;;###autoload
(defun org-roam-dailies-goto-today ()
  "Find the daily-note for today, creating it if necessary."
  (interactive)
  (org-roam-dailies-capture-today t))

;;; Tomorrow
;;;###autoload
(defun org-roam-dailies-capture-tomorrow (n &optional goto)
  "Create an entry in the daily-note for tomorrow.

With numeric argument N, use the daily-note N days in the future.

With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry."
  (interactive "p")
  (org-roam-dailies--capture (time-add (* n 86400) (current-time)) goto))

;;;###autoload
(defun org-roam-dailies-goto-tomorrow (n)
  "Find the daily-note for tomorrow, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future."
  (interactive "p")
  (org-roam-dailies-capture-tomorrow n t))

;;; Yesterday
;;;###autoload
(defun org-roam-dailies-capture-yesterday (n &optional goto)
  "Create an entry in the daily-note for yesteday.

With numeric argument N, use the daily-note N days in the past.

When GOTO is non-nil, go the note without creating an entry."
  (interactive "p")
  (org-roam-dailies-capture-tomorrow (- n) goto))

;;;###autoload
(defun org-roam-dailies-goto-yesterday (n)
  "Find the daily-note for yesterday, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future."
  (interactive "p")
  (org-roam-dailies-capture-tomorrow (- n) t))

;;; Calendar
(defun org-roam-dailies-calendar--file-to-date (&optional file)
  "Convert FILE to date.
Return (MONTH DAY YEAR)."
  (let ((file (or file
                  (buffer-base-buffer (buffer-file-name)))))
    (cl-destructuring-bind (_ _ _ d m y _ _ _)
        (org-parse-time-string
         (file-name-sans-extension
          (file-name-nondirectory file)))
      (list m d y))))

(defun org-roam-dailies-calendar--date-to-time (date)
  "Convert DATE as returned from then calendar (MONTH DAY YEAR) to a time."
  (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))

(defun org-roam-dailies-calendar-mark-entries ()
  "Mark days in the calendar for which a daily-note is present."
  (when (file-exists-p (expand-file-name org-roam-dailies-directory org-roam-directory))
    (dolist (date (mapcar #'org-roam-dailies-calendar--file-to-date
                          (org-roam-dailies--list-files)))
      (when (calendar-date-is-visible-p date)
        (calendar-mark-visible-date date 'org-roam-dailies-calendar-note)))))

;;; Date
;;;###autoload
(defun org-roam-dailies-capture-date (&optional goto prefer-future)
  "Create an entry in the daily-note for a date using the calendar.
Prefer past dates, unless PREFER-FUTURE is non-nil.
With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil (if goto
                                           "Find daily-note: "
                                         "Capture to daily-note: ")))))
    (org-roam-dailies--capture time goto)))

;;;###autoload
(defun org-roam-dailies-goto-date (&optional prefer-future)
  "Find the daily-note for a date using the calendar, creating it if necessary.
Prefer past dates, unless PREFER-FUTURE is non-nil."
  (interactive)
  (org-roam-dailies-capture-date t prefer-future))

;;; Navigation
(defun org-roam-dailies--list-files (&rest extra-files)
  "List all files in `org-roam-dailies-directory'.
EXTRA-FILES can be used to append extra files to the list."
  (let ((dir (expand-file-name org-roam-dailies-directory org-roam-directory))
        (regexp (rx-to-string `(and "." (or ,@org-roam-file-extensions)))))
    (append (--remove (let ((file (file-name-nondirectory it)))
                        (when (or (auto-save-file-name-p file)
                                  (backup-file-name-p file)
                                  (string-match "^\\." file))
                          it))
                      (directory-files-recursively dir regexp))
            extra-files)))

(defun org-roam-dailies-goto-next-note (&optional n)
  "Find next daily-note.

With numeric argument N, find note N days in the future. If N is
negative, find note N days in the past."
  (interactive "p")
  (unless (org-roam-dailies--daily-note-p)
    (user-error "Not in a daily-note"))
  (setq n (or n 1))
  (let* ((dailies (org-roam-dailies--list-files))
         (position
          (cl-position-if (lambda (candidate)
                            (string= (buffer-file-name (buffer-base-buffer)) candidate))
                          dailies))
         note)
    (unless position
      (user-error "Can't find current note file - have you saved it yet?"))
    (pcase n
      ((pred (natnump))
       (when (eq position (- (length dailies) 1))
         (user-error "Already at newest note")))
      ((pred (integerp))
       (when (eq position 0)
         (user-error "Already at oldest note"))))
    (setq note (nth (+ position n) dailies))
    (find-file note)
    (run-hooks 'org-roam-dailies-find-file-hook)))

(defun org-roam-dailies-goto-previous-note (&optional n)
  "Find previous daily-note.

With numeric argument N, find note N days in the past. If N is
negative, find note N days in the future."
  (interactive "p")
  (let ((n (if n (- n) -1)))
    (org-roam-dailies-goto-next-note n)))

(add-hook 'calendar-today-visible-hook #'org-roam-dailies-calendar-mark-entries)
(add-hook 'calendar-today-invisible-hook #'org-roam-dailies-calendar-mark-entries)

;;;; Bindings
(defvar org-roam-dailies-map (make-sparse-keymap)
  "Keymap for `org-roam-dailies'.")

(define-prefix-command 'org-roam-dailies-map)

(define-key org-roam-dailies-map (kbd "d") #'org-roam-dailies-goto-today)
(define-key org-roam-dailies-map (kbd "y") #'org-roam-dailies-goto-yesterday)
(define-key org-roam-dailies-map (kbd "t") #'org-roam-dailies-goto-tomorrow)
(define-key org-roam-dailies-map (kbd "n") #'org-roam-dailies-capture-today)
(define-key org-roam-dailies-map (kbd "f") #'org-roam-dailies-goto-next-note)
(define-key org-roam-dailies-map (kbd "b") #'org-roam-dailies-goto-previous-note)
(define-key org-roam-dailies-map (kbd "c") #'org-roam-dailies-goto-date)
(define-key org-roam-dailies-map (kbd "v") #'org-roam-dailies-capture-date)
(define-key org-roam-dailies-map (kbd ".") #'org-roam-dailies-find-directory)

(provide 'org-roam-dailies)

;;; org-roam-dailies.el ends here
