;;; es-mode.el --- elasticsearch mode

;; Copyright (C) 2014 Matthew Lee Hinman

;; Author: Lee Hinman <lee@writequit.org>
;; URL: http://www.github.com/dakrone/es-mode
;; Version: 1.0.0
;; Keywords: elasticsearch

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a major mode for editing Elasticsearch examples. Inherits the
;; indention from js-mode.

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

(defconst es-top-level-fields '("facets" "filter" "post_filter" "query"))
(defconst es-keywords '("fields" "from" "size"))
(defconst es-http-builtins '("DELETE" "GET" "POST" "PUT"))
(defconst es-parent-types '("and" "bool" "filtered" "must"
                            "must_not" "not" "or" "should"))
(defconst es-query-types '("boosting" "common" "constant_score"
                           "custom_boost_factor" "custom_filters_score"
                           "custom_score" "dismax" "function_score" "fuzzy"
                           "fuzzy_like_this" "fuzzy_like_this_field"
                           "geo_shape" "has_child" "has_parent" "ids"
                           "indices" "match" "match_all" "match_phrase"
                           "match_phrase_prefix" "more_like_this"
                           "more_like_this_field" "multi_match" "nested"
                           "prefix" "query_string" "range" "regexp"
                           "simple_query_string" "span_first"
                           "span_multi_term" "span_near" "span_not" "span_or"
                           "span_term" "term" "terms" "text" "top_children"
                           "wildcard"))
(defconst es-facet-types '("date_histogram" "geo_distance" "histogram"
                           "statistical" "terms_stats"))

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
      ;; builtins for REST
      (,(concat "-X\\(" (regexp-opt es-http-builtins) "\\)")
       (1 font-lock-builtin-face t))
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

(defun es-indent-line ()
  "Indent current line as ES code"
  (interactive)
  (beginning-of-line)
  (js-indent-line)
  (if (bobp)
      (indent-line-to 0)))

(defvar es-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for ES mode.")

(define-derived-mode es-mode prog-mode "ES"
  "Major mode for editing curl ES scripts, similar to both sh-mode and js-mode."
  (kill-all-local-variables)
  (set-syntax-table es-mode-syntax-table)
  (use-local-map es-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(es-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'es-indent-line)
  (setq major-mode 'es-mode)
  (setq mode-name "ES")
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*")
  (run-hooks 'es-mode-hook))

;; This is a terrible terrible hack right now
(defun org-babel-execute:es (body params)
  "Execute a block of ES code with org-babel."
  (message "executing ES source code block")
  (require 'org-install)
  (org-babel-eval "/bin/sh" body))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.es\\'" . es-mode))

(provide 'es-mode)

;;; es-mode.el ends here
