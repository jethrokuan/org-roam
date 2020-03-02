;;; org-roam-protocol.el --- Protocol handler for roam:// links  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Author: Jethro Kuan <jethrokuan95@gmail.com>

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
;; We extend org-protocol, adding custom Org-roam handlers. The setup
;; instructions for `org-protocol' can be found in org-protocol.el.
;;
;;; Code:

(require 'org-protocol)
(require 'org-roam)

(declare-function org-roam-find-ref "org-roam" (&optional info))
(declare-function org-roam--capture-get-point "org-roam" ())

(defvar org-roam-ref-capture-templates
  '(("r" "ref" plain (function org-roam--capture-get-point)
     ""
     :file-name "${slug}"
     :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}\n"
     :unnarrowed t)))

(defun org-roam-protocol-open-ref (info)
  "Process an org-protocol://roam-ref?ref= style url with INFO.

The sub-protocol used to reach this function is set in
`org-protocol-protocol-alist'.

This function decodes a ref.

  javascript:location.href = \\='org-protocol://roam-ref?template=rref=\\='+ \\
        encodeURIComponent(location.href) + \\='&title=\\=' \\
        encodeURIComponent(document.title) + \\='&body=\\=' + \\
        encodeURIComponent(window.getSelection())"
  (when-let* ((alist (org-roam--plist-to-alist info))
              (decoded-alist (mapcar (lambda (k.v)
                                       (let ((key (car k.v))
                                             (val (cdr k.v)))
                                         (cons key (org-link-decode val)))) alist)))
    (unless (assoc 'ref decoded-alist)
      (error "No ref key provided."))
    (let* ((template (cdr (assoc 'template decoded-alist)))
           (org-roam-capture-templates org-roam-ref-capture-templates)
           (org-roam--capture-context 'ref)
           (org-roam--capture-info decoded-alist))
      (raise-frame)
      (org-roam-capture nil template)
      (message "Item captured.")))
  nil)

(defun org-roam-protocol-open-file (info)
  "This handler simply opens the file with emacsclient.

  Example protocol string:

org-protocol://roam-file?file=/path/to/file.org"
  (when-let ((file (plist-get info :file)))
    (raise-frame)
    (find-file file))
  nil)

(push '("org-roam-ref"  :protocol "roam-ref"   :function org-roam-protocol-open-ref)
      org-protocol-protocol-alist)
(push '("org-roam-file"  :protocol "roam-file"   :function org-roam-protocol-open-file)
      org-protocol-protocol-alist)

(provide 'org-roam-protocol)

;;; org-roam-protocol.el ends here
