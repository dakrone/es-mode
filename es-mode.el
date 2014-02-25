;;; es-mode.el --- A major mode for Elasticsearch curl/http scripts

;; Copyright (C) 2014 Matthew Lee Hinman

;; Author: Lee Hinman <lee@writequit.org>
;; URL: http://www.github.com/dakrone/es-mode
;; Version: 1.0.0
;; Keywords: elasticsearch

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a major mode for editing Elasticsearch examples. A mix between
;; sh-mode and js-mode that highlights queries, filters and builtins for
;; writing ES shell scripts.

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

(defvar es-mode-hook nil)

(defvar es-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for ES major mode")

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

(defconst es-font-lock-keywords
  (eval-when-compile
    `(("'\\(.+?\\)'" . font-lock-string-face)
      ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
      ("\\<\\(#.*\\)\\>" . font-lock-comment-face)
      ;; top-level fields containing leaf nodes
      (,(concat "\"\\(" (regexp-opt es-top-level-fields) "\\)\"")
       (1 font-lock-constant-face t))
      ;; keywords for fields usually specified
      (,(concat "\"\\(" (regexp-opt es-keywords) "\\)\"")
       (1 font-lock-keyword-face t))
      ;; builtins for warnings
      (,(concat " -.*X\\(" (regexp-opt es-warnings) "\\)")
       (1 font-lock-warning-face t))
      ;; builtins for REST
      (,(concat " -.*X\\(" (regexp-opt es-http-builtins) "\\)")
       (1 font-lock-builtin-face t))
      ;; types (parent queries containing sub queries)
      (,(concat "\"\\(" (regexp-opt es-parent-types) "\\)\"")
       (1 font-lock-type-face t))
      ;; query types (leaf nodes)
      (,(concat "\"\\(" (regexp-opt es-query-types) "\\)\"")
       (1 font-lock-function-name-face t))
      ;; facet types (leaf nodes)
      (,(concat "\"\\(" (regexp-opt es-facet-types) "\\)\"")
       (1 font-lock-function-name-face t))
      ;; Highlight shell variables
      ("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
       (2 font-lock-variable-name-face))
      ))
  "Highlighting expressions for ES mode")

(defun es-indent-line ()
  "Indent current line as ES code. Uses the same indention as js-mode."
  (interactive)
  (beginning-of-line)
  (js-indent-line)
  (when (bobp)
    (indent-line-to 0)))

(defvar es-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for ES mode.")

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

;;;###autoload
(define-derived-mode es-mode prog-mode "ES"
  "Major mode for editing curl ES scripts, similar to both sh-mode and js-mode."
  (kill-all-local-variables)
  (set-syntax-table es-mode-syntax-table)
  (use-local-map es-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(es-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'es-indent-line)
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'es-company-backend t))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq mode-name "ES")
  (setq major-mode 'es-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.es\\'" . es-mode))

(provide 'es-mode)

;;; es-mode.el ends here
