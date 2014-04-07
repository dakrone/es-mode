;;; ob-elasticsearch.el --- org-babel functions for Elasticsearch queries

;; Copyright (C) 2014 Bjarte Johansen
;; Copyright (C) 2014 Matthew Lee Hinman

;; Author: Bjarte Johansen
;; Keywords: literate programming, reproducible research
;; Homepage: http://www.github.com/dakrone/es-mode
;; Version: 0.0.1

;;; License:

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

;; Provides a way to evaluate Elasticsearch queries in org-mode.

;;; Code:
(require 'ob)
(require 'es-mode)

(defvar org-babel-default-header-args:es
  `((:url . ,es-default-url)
    (:method . ,es-default-request-method))
  "Default arguments for evaluating an elasticsearch query
block.")

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("es" . "es"))

(defun org-babel-expand-body:es (body params)
  "This command is used by org-tangle to create a file with the
source code of the elasticsearch block. If :tangle specifies a
file with the .sh extension a curl-request is created instead of
just a normal .es file that contains the body of the block.."
  (let ((ext (file-name-extension
              (cdr (assoc :tangle params)))))
    (if (not (equal "sh" ext))
        body
      (let ((method (cdr (assoc :method params)))
            (url (cdr (assoc :url params))))
        (format "curl -X%s %s -d %S\n"
                (upcase method)
                url
                body)))))

(defun es-org-execute (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (when (string-match "^.* 20[0-9] OK$" (or (thing-at-point 'line) ""))
      (search-forward "\n\n")
      (delete-region (point-min) (point))
      (mark-whole-buffer)
      (es-indent-line))
    (buffer-string)))

(defun es-org-babel-execute-with-params (body params)
  "Executes the body of the request, use the org :header
parameters for the method and URL specification."
  (let ((endpoint-url (cdr (assoc :url params)))
        (url-request-method (upcase (or (cdr (assoc :method params))
                                        es-default-request-method)))
        (url-request-data body)
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (when (es--warn-on-delete-yes-or-no-p)
      (es-org-execute (es--fix-url endpoint-url)))))

(defun es-org-execute-request-with-body-params ()
  "Executes a request with parameters that are above the request.
Does not move the point."
  (interactive)
  ;; If we're currently on a parameter declaration, go forward a line and a
  ;; character to place us into the sexp {}
  (save-excursion
    (when (or (eq 1 (point)) (es--at-current-header-p))
      (es-mark-request-body))
    (let* ((params (es--find-params))
           (url-request-method (car params))
           (url (cdr params))
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded")))
           (url-request-data (buffer-substring-no-properties
                              (region-beginning)
                              (region-end))))
      (es-org-execute url))))

(defun org-babel-execute:es (body params)
  "Execute a block containing an Elasticsearch query with
org-babel.  This function is called by
`org-babel-execute-src-block'. If `es-warn-on-delete-query' is
set to true, this function will also ask if the user really wants
to do that."
  ;; If we can find parameters in the body itself, use those
  (if (save-excursion
        (string-match-p (concat "^" (regexp-opt es-http-builtins) " .*$") body))
      (with-temp-buffer
        (setq buffer-read-only nil)
        (insert body)
        (beginning-of-buffer)
        (setq output (es-org-execute-request-with-body-params))
        (while (es-goto-next-request)
          (let ((new-output
                 (concat output "\n"
                         (es-org-execute-request-with-body-params))))
            (setq output new-output)))
        output)
    (es-org-babel-execute-with-params body params)))

(provide 'ob-elasticsearch)
;;; ob-elasticsearch.el ends here
