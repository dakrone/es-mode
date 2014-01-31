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

(defconst es-font-lock-keywords
  (list
   '("'\\(.+?\\)'" . font-lock-string-face)
   '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
   '("\\<\\(#.*\\)\\>" . font-lock-comment-face))
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

;; Constants, top-level fields
(font-lock-add-keywords 'es-mode '(("\"\\(facets\\)\"" 1 'font-lock-constant-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(filter\\)\"" 1 'font-lock-constant-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(post_filter\\)\"" 1 'font-lock-constant-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(query\\)\"" 1 'font-lock-constant-face t)))

;; Keywords
(font-lock-add-keywords 'es-mode '(("\"\\(fields\\)\"" 1 'font-lock-keyword-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(from\\)\"" 1 'font-lock-keyword-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(size\\)\"" 1 'font-lock-keyword-face t)))


;; Builtins
(font-lock-add-keywords 'es-mode '(("-X\\(DELETE\\)" 1 'font-lock-builtin-face t)))
(font-lock-add-keywords 'es-mode '(("-X\\(GET\\)" 1 'font-lock-builtin-face t)))
(font-lock-add-keywords 'es-mode '(("-X\\(POST\\)" 1 'font-lock-builtin-face t)))
(font-lock-add-keywords 'es-mode '(("-X\\(PUT\\)" 1 'font-lock-builtin-face t)))

;; Types (parent nodes)
(font-lock-add-keywords 'es-mode '(("\"\\(and\\)\"" 1 'font-lock-type-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(bool\\)\"" 1 'font-lock-type-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(filtered\\)\"" 1 'font-lock-type-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(must\\)\"" 1 'font-lock-type-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(must_not\\)\"" 1 'font-lock-type-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(not\\)\"" 1 'font-lock-type-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(or\\)\"" 1 'font-lock-type-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(should\\)\"" 1 'font-lock-type-face t)))

;; Functions (query types)
(font-lock-add-keywords 'es-mode '(("\"\\(boosting\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(common\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(constant_score\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(custom_boost_factor\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(custom_filters_score\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(custom_score\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(dismax\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(function_score\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(fuzzy\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(fuzzy_like_this\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(fuzzy_like_this_field\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(geo_shape\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(has_child\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(has_parent\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(ids\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(indices\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(match\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(match_all\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(match_phrase\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(match_phrase_prefix\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(more_like_this\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(more_like_this_field\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(multi_match\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(nested\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(prefix\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(query_string\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(range\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(regexp\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(simple_query_string\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(span_first\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(span_multi_term\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(span_near\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(span_not\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(span_or\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(span_term\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(term\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(terms\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(text\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(top_children\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(wildcard\\)\"" 1 'font-lock-function-name-face t)))
;; Functions (facet types)
(font-lock-add-keywords 'es-mode '(("\"\\(date_histogram\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(geo_distance\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(histogram\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(statistical\\)\"" 1 'font-lock-function-name-face t)))
(font-lock-add-keywords 'es-mode '(("\"\\(terms_stats\\)\"" 1 'font-lock-function-name-face t)))

(defun es-mode ()
  "Major mode for editing ES files"
  (interactive)
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
