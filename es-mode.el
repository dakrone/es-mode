;;; es-mode.el --- A major mode for editing Elasticsearch queries

;; Copyright (C) 2014 Matthew Lee Hinman
;; Copyright (C) 2014 Bjarte Johansen

;; Author: Lee Hinman <lee@writequit.org>
;; URL: http://www.github.com/dakrone/es-mode
;; Version: 2.0.0
;; Keywords: elasticsearch

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a major mode for editing queries and sending them to an
;; Elasticsearch endpoint.

;;; Usage:

;; Add to your Emacs config:
;;  (add-to-list 'load-path "/path/to/es-mode-dir")
;;  (autoload 'es-mode "es-mode.el"
;;    "Major mode for editing Elasticsearch queries" t)
;;  (add-to-list 'auto-mode-alist '("\\.es$" . es-mode))

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

;;; Code:

(require 'js)
(require 'url)
(require 'url-util)

(defgroup es nil
  "Major mode for editing Elasticsearch queries."
  :group 'languages)

(defcustom es-indent-offset 2
  "Indentation offset for `es-mode'."
  :group 'es
  :type 'integer)

(defcustom es-default-url "http://localhost:9200/_search?pretty=true"
  "The default URL of the Elasticsearch endpoint."
  :group 'es
  :type 'string)

(defcustom es-prompt-url nil
  "Non-nil means prompt user for requested URL on each query
  evaluation."
  :group 'es
  :type 'boolean)

(defvar es-endpoint-url nil
  "The current URL used as the Elasticsearch endpoint.")

(defvar es-endpoint-url-history (list es-default-url)
  "The history over used URLs.")

(defcustom es-default-request-method "POST"
  "The default request method used for queries."
  :group 'es
  :type '(choice
          (const "POST")
          (const "GET")
          (const "PUT")
          (const "PATCH")
          (const "OPTIONS")
          (const  "DELETE")
          (string :tag "Custom")))

(defcustom es-prompt-request-method nil
  "Non-nil means prompt user for the request method on each query
evaluation."
  :group 'es
  :type 'boolean)

(defvar es-request-method nil
  "The current request method used for this buffer.")

(defvar es-request-method-history
  '("POST"
    "GET"
    "PUT"
    "PATCH"
    "OPTIONS"
    "DELETE"))

(defcustom es-warn-on-delete-query t
  "If `es-warn-on-delete-query' is set to true, es-mode prompts
the user on DELETE requests."
  :group 'es
  :type 'boolean)

(defvar es-results-buffer nil
  "Buffer local variable pointing to the buffer containing the
  results from the most recent query.")

(defvar es-result-response nil
  "The variable containing the response header from the result in
  a result buffer.")

(defvar es-top-level-fields
  '("aggregations" "aggs" "facets" "filter"
    "post_filter" "query")
  "Top-level query and filter containers")

(defvar es-keywords
  '("fields" "from" "size" "highlight")
  "Top-level fields supported by all queries")

(defvar es-warnings
  '("DELETE")
  "HTTP methods that should be highlighted as warnings")

(defvar es-http-builtins
  '("GET" "OPTIONS" "PATCH" "POST" "PUT")
  "HTTP methods used by curl")

(defvar es-parent-types
  '("and" "bool" "filtered" "not" "or" "properties" "mappings" "settings")
  "Compound queries that always contain additional queries or filters")

(defvar es-query-types
  '("boosting" "common" "constant_score" "custom_boost_factor"
    "custom_filters_score" "custom_score" "dismax" "function_score" "fuzzy"
    "fuzzy_like_this" "fuzzy_like_this_field" "geo_shape" "has_child"
    "has_parent" "ids" "indices" "match" "match_all" "match_phrase"
    "match_phrase_prefix" "more_like_this" "more_like_this_field"
    "multi_match" "nested" "prefix" "query_string" "range" "regexp"
    "simple_query_string" "span_first" "span_multi_term" "span_near"
    "span_not" "span_or" "span_term" "term" "terms" "text" "top_children"
    "wildcard" "geo_distance" "geo_bbox")
  "Various leaf-type queries and filters")

(defvar es-facet-types
  '("date_histogram" "geo_distance" "histogram" "statistical" "terms_stats"
    "min" "max" "sum" "avg" "stats" "extended_stats" "value_count" "missing"
    "geohash_grid" "script")
  "Leaf-type facets")

(defun es-set-endpoint-url (new-url)
  "`new-url' is the url that you want the queries to be sent
  to."
  (interactive
   (let ((current-url (or es-endpoint-url es-default-url)))
     (list (read-string (format "ES URL (%s): " current-url)
                        nil
                        'es-endpoint-url-history
                        current-url))))
  (setq es-endpoint-url
        (if (string= "" new-url)
            (or es-endpoint-url es-default-url)
          (add-to-list 'es-endpoint-url-history new-url)
          new-url)))

(defun es-get-url ()
  "Returns the URL for the ES queries in this buffer unless it
has not been set, in which case it prompts the user."
  (or (and (not es-prompt-url) es-endpoint-url)
      (command-execute 'es-set-endpoint-url)))

(defun es-set-request-method (new-request-method)
  "Set the request method to be used for the buffer."
  (interactive
   (let ((current-request-method (or es-request-method
                                     es-default-request-method)))
     (list (read-string (format "Method (%s): " current-request-method)
                        nil
                        'es-request-method-history
                        current-request-method))))
  (setq es-request-method
        (if (string= "" new-request-method)
            (or es-request-method es-default-request-method)
          (add-to-list 'es-request-method-history new-request-method)
          new-request-method)))

(defun es-get-request-method ()
  "Returns the current request-method unless it has not been set,
in which case it prompts the user."
  (or (and (not es-prompt-request-method) es-request-method)
      (command-execute 'es-set-request-method)))

(defun es-company-backend (command &optional arg &rest ign)
  "A `company-backend' for es-queries and facets."
  (case command
    ('prefix (let ((sym (company-grab-symbol)))
               (if (string-match "\"\\(.*\\)\"?" sym)
                   (match-string 1 sym)
                 sym)))
    ('candidates
     (all-completions
      arg
      (append es-top-level-fields es-query-types es-facet-types
              es-parent-types es-keywords)))))

(defun es-result--handle-response (status &optional results-buffer)
  "Handles the response from the server returns after sending a
query. "
  (let ((http-results-buffer (current-buffer)))
    (set-buffer results-buffer)
    (let ((buffer-read-only nil))
      (insert-buffer-substring http-results-buffer)
      (kill-buffer http-results-buffer)
      (if (zerop (buffer-size))
          (insert "Error: could not open connection to server.")
        (goto-char (point-min))
        (when (string-match "^.* 20[0-9] OK$" (thing-at-point 'line))
          (search-forward "\n\n")
          (setq es-result-response
                (buffer-substring (point-min) (point)))
          (delete-region (point-min) (point)))
        (setq mode-name "ES[finished]")))))

(defun es--warn-on-delete-yes-or-no-p ()
  (or (not (string= "DELETE" (upcase url-request-method)))
      (not es-warn-on-delete-query)
      (yes-or-no-p
       ;; This will not font-lock if `yes-or-no-p' is aliased to
       ;; `y-or-n-p'.
       (propertize
        "Do you really want to send a DELETE request?"
        'font-lock-face 'font-lock-warning-face))))

(defun es-query-region ()
  "Submits the active region as a query to the specified
endpoint. If the region is not active, the whole buffer is used."
  (interactive)
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url (es-get-url))
         (url-request-method (es-get-request-method))
         (url-request-data (buffer-substring beg end)))
    (when (es--warn-on-delete-yes-or-no-p)
      (unless (buffer-live-p es-results-buffer)
        (setq es-results-buffer
              (generate-new-buffer
               (format "*ES: %s*" (buffer-name))))
        (with-current-buffer es-results-buffer
          (setq buffer-read-only nil)
          (es-result-mode)))
      (with-current-buffer es-results-buffer
        (let ((buffer-read-only nil))
          (delete-region (point-min) (point-max))))
      (url-retrieve url 'es-result--handle-response (list es-results-buffer))
      (view-buffer-other-window es-results-buffer)
      (other-window -1))))

(defun es-result-show-response ()
  "Shows the header of the response from the server in the
  minibuffer."
  (interactive)
  (message es-result-response))

(defvar es-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'es-result-show-response)
    map)
  "Keymap for `es-result-mode'")

(define-derived-mode es-result-mode text-mode "ES[waiting]"
  "Major mode to hold the result from a query to elastic search end point.
\\{es-result-mode-map}"
  ;; Use es-mode syntax-table
  (set-syntax-table es-mode-syntax-table)
  ;; Use es-mode font-lock
  (setq font-lock-defaults '(es-font-lock-keywords))
  (make-local-variable 'es-result-response))

(defun es-indent-line ()
  "Indent current line as ES code. Uses the same indention as js-mode."
  (interactive)
  (beginning-of-line)
  ;; Dynamically bind js-indent-level so we can have our own indent
  ;; offset if we want to.
  (let ((js-indent-level es-indent-offset))
    (js-indent-line))
  (when (bobp)
    (indent-line-to 0)))

(defconst es-font-lock-keywords
  (eval-when-compile
    `(;; Booleans
      (,(regexp-opt '("true" "false") 'word) . font-lock-constant-face)
      ;; top-level fields containing leaf nodes
      (,(concat "\"\\(" (regexp-opt es-top-level-fields) "\\)\"")
       (1 font-lock-constant-face t))
      ;; repair font-locking for http://... turning into comment.
      ("http:\\(//[^\"\n]+\\)" (1 font-lock-string-face t))
      ;; builtins for warnings
      (,(concat "^\\s-*\\("
                (regexp-opt es-warnings)
                "\\)\\s-+\\(http://[^[:space:]\n]+\\)")
       (1 font-lock-warning-face t)
       (2 font-lock-variable-name-face t))
      ;; builtins for REST
      (,(concat "^\\s-*\\("
                (regexp-opt es-http-builtins)
                "\\)\\s-+\\(http://[^[:space:]\n]+\\)")
       (1 font-lock-builtin-face t)
       (2 font-lock-variable-name-face t))
      ;; keywords for fields usually specified
      (,(concat "\"\\(" (regexp-opt es-keywords) "\\)\"")
       (1 font-lock-keyword-face t))
      ;; types (parent queries containing sub queries)
      (,(concat "\"\\(" (regexp-opt es-parent-types) "\\)\"")
       (1 font-lock-type-face t))
      ;; query types (leaf nodes)
      (,(concat "\"\\(" (regexp-opt es-query-types) "\\)\"")
       (1 font-lock-function-name-face t))
      ;; facet types (leaf nodes)
      (,(concat "\"\\(" (regexp-opt es-facet-types) "\\)\"")
       (1 font-lock-function-name-face t))))
  "Highlighting expressions for ES mode")

(defvar es-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Set _ to a word character so it can be used inside words.
    (modify-syntax-entry ?_ "w" st)
    ;; Set " and ' as string delimiters.
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)
    ;; / is a punctuation character and is the first and second
    ;; character of a two letter comment starter.
    (modify-syntax-entry ?/ ". 12" st)
    ;; newline is the end of a comment.
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for ES mode.")

(defvar es-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'es-query-region)
    (define-key map (kbd "C-c C-u") 'es-set-endpoint-url)
    (define-key map (kbd "C-c C-f") 'es-set-request-method)
    map)
  "Keymap for `es-mode'.")

;; Compatibility with Emacs < 24
(defalias 'es-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode es-mode es-parent-mode "ES"
  "Major mode for editing Elasticsearch queries.
\\{es-mode-map}"
  ;; Font lock and indent
  (setq-local font-lock-defaults '(es-font-lock-keywords))
  (setq-local indent-line-function 'es-indent-line)

  ;; Comment dwim
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+[\t ]*")

  ;; Local buffer for results
  (make-local-variable 'es-results-buffer)

  (make-local-variable 'es-endpoint-url)
  (make-local-variable 'es-request-method)

  ;; If we have company-mode we use it.
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'es-company-backend t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.es\\'" . es-mode))

(provide 'es-mode)

;;; es-mode.el ends here
