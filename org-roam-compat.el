;;; org-roam-compat.el --- Compatibility Code -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

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
;; This file contains code needed for backward compatibility with older Emacsen
;; and previous versions of org-roam.
;;
;;; Code:
;;;; Library Requires

;;; Backports
(when (< emacs-major-version 27)
  ;; Emacs 26 does not have FOLLOW-SYMLINKS in `directory-files-recursively'
  (defun directory-files-recursively (dir regexp
                                          &optional include-directories predicate
                                          follow-symlinks)
    "Return list of all files under directory DIR whose names match REGEXP.
This function works recursively.  Files are returned in \"depth
first\" order, and files from each directory are sorted in
alphabetical order.  Each file name appears in the returned list
in its absolute form.

By default, the returned list excludes directories, but if
optional argument INCLUDE-DIRECTORIES is non-nil, they are
included.

PREDICATE can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into).

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
    (let* ((result nil)
           (files nil)
           (dir (directory-file-name dir))
           ;; When DIR is "/", remote file names like "/method:" could
           ;; also be offered.  We shall suppress them.
           (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
      (dolist (file (sort (file-name-all-completions "" dir)
                          'string<))
        (unless (member file '("./" "../"))
          (if (directory-name-p file)
              (let* ((leaf (substring file 0 (1- (length file))))
                     (full-file (concat dir "/" leaf)))
                ;; Don't follow symlinks to other directories.
                (when (and (or (not (file-symlink-p full-file))
                               (and (file-symlink-p full-file)
                                    follow-symlinks))
                           ;; Allow filtering subdirectories.
                           (or (eq predicate nil)
                               (eq predicate t)
                               (funcall predicate full-file)))
                  (let ((sub-files
                         (if (eq predicate t)
                             (condition-case nil
                                 (directory-files-recursively
                                  full-file regexp include-directories
                                  predicate follow-symlinks)
                               (file-error nil))
                           (directory-files-recursively
                            full-file regexp include-directories
                            predicate follow-symlinks))))
                    (setq result (nconc result sub-files))))
                (when (and include-directories
                           (string-match regexp leaf))
                  (setq result (nconc result (list full-file)))))
            (when (string-match regexp file)
              (push (concat dir "/" file) files)))))
      (nconc result (nreverse files)))))

;;; Obsolete aliases (remove after next major release)
;;; Obsolete functions

(provide 'org-roam-compat)

;;; org-roam-compat.el ends here
