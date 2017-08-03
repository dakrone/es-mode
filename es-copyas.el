;;; es-copyas.el --- Copy ES queries as other things

;; Copyright (C) 2017 Matthew Lee Hinman

;; Author: Lee Hinman <lee@writequit.org>
;; URL: http://www.github.com/dakrone/es-mode
;; Version: 1.0.0
;; Keywords: elasticsearch
;; Package-Requires: ((dash "2.11.0") (cl-lib "0.5") (spark "1.0") (s 1.11.0))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides the ability to copy es-mode snippets as `curl' or other command
;; invocations

;;; Usage:

;; Fill me in

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

(require 'es-mode)

(defcustom es-copy-as-fn 'es-copy-as-curl
  "Default type when copying request to a different tool's
  invocation. Defaults to `es-copy-as-curl' which copies the
  request as a `curl' invocation."
  :group 'es)

(defcustom es-copy-as-single-line nil
  "Whether the copied request should use a single line, or
whether it can span multiple lines. Defaults to `nil' meaning the
request can span multiple (quoted) lines."
  :group 'es)

(defun es-copy-as-curl ()
  (let* ((body (es-get-request-body))
         ;; this *should* fix the issue if someone uses single quotes in their request
         (fixed-body (replace-regexp-in-string "'" "\\\\u0027" body))
         (params (or (es--find-params)
                     `(,(es-get-request-method) . ,(es-get-url))))
         (url (es--munge-url (cdr params)))
         (url-request-method (car params)))
    (kill-new
     (format "curl -H \"Content-Type: application/json\" -X%s \"%s\" -d'%s'"
             url-request-method url
             (if es-copy-as-single-line
                 (replace-regexp-in-string "\n" "" fixed-body)
               fixed-body)))))

(defun es-copy-as-wget ()
  (error "Not implemented yet!"))

(defun es-copy-as-powershell ()
  (error "Not implemented yet!"))

;;;###autoload
(defun es-copy-as ()
  (interactive)
  (es-save-everything
   (funcall es-copy-as-fn)
   (message "Request copied to kill ring with [%s]" es-copy-as-fn)))

(provide 'es-copyas)

;;; es-copyas.el ends here
