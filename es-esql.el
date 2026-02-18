;;; es-esql.el --- A sql interactive mode for ESQL -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2026 Matthew Lee Hinman
;; Copyright (C) 2014 Bjarte Johansen

;; Author: Lee Hinman <lee@writequit.org>
;; URL: http://www.github.com/dakrone/es-mode
;; Version: 1.0.0
;; Keywords: elasticsearch
;; Package-Requires: ((dash "2.11.0") (cl-lib "0.5") (spark "1.0") (s "1.11.0") (request "0.3.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; TODO: fill me in

;;; Usage:

;; TODO: fill me in

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
(require 'sql)

(sql-add-product 'esql "Elasticsearch ES|QL"
                 '(:free-software nil))

(defvar es-esql-mode-font-lock-keywords
  '(("\\b\\(from\\|select\\|limit\\)\\b"
     . font-lock-keyword-face))
  "Elasticsearch ES|QL keywords used by font-lock.")

(defvar es-esql-mode-font-lock-keywords
  (eval-when-compile
    (list
     ;; Passthrough commands
     '("^[.].*$" . font-lock-doc-face)
     ;; Comments starting with //
     '("[//].*$" . font-lock-comment-face)
     '("/\*.*\*/" . font-lock-comment-face)

     ;; ES|QL Keywords
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
                                     "change_point" "completion" "dissect" "drop" "enrich" "eval"
                                     "fork" "from" "fuse" "grok" "inline stats" "keep" "limit"
                                     "lookup join" "mv_expand" "rename" "rerank" "row" "sample"
                                     "set" "show" "sort" "stats" "ts" "uri_parts" "where"
                                     )
     ;; ES|QL Data types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
                                     "integer" "double" "float" "boolean" "date"
                                     )
     ;; ES|QL Functions
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
                                     "abs" "absent" "absent_over_time" "acos" "acosh" "asin" "asinh"
                                     "atan" "atan2" "atanh" "avg" "avg_over_time" "bit_length" "bucket"
                                     "byte_length" "case" "categorize" "cbrt" "ceil" "chunk" "cidr_match"
                                     "clamp" "clamp_max" "clamp_min" "coalesce" "concat" "contains"
                                     "copy_sign" "cos" "cosh" "count" "count_distinct"
                                     "count_distinct_over_time" "count_over_time" "date_diff"
                                     "date_extract" "date_format" "date_parse" "date_trunc" "day_name"
                                     "decay" "delta" "deriv" "e" "ends_with" "exp" "first"
                                     "first_over_time" "floor" "from_base64" "greatest" "hash" "hypot"
                                     "idelta" "increase" "ip_prefix" "irate" "knn" "kql" "last"
                                     "last_over_time" "least" "left" "length" "locate" "log" "log10"
                                     "ltrim" "match" "match_phrase" "max" "max_over_time" "md5" "median"
                                     "median_absolute_deviation" "min" "min_over_time" "month_name"
                                     "mv_append" "mv_avg" "mv_concat" "mv_contains" "mv_count"
                                     "mv_dedupe" "mv_first" "mv_intersection" "mv_intersects" "mv_last"
                                     "mv_max" "mv_median" "mv_median_absolute_deviation" "mv_min"
                                     "mv_percentile" "mv_pseries_weighted_sum" "mv_slice" "mv_sort"
                                     "mv_sum" "mv_union" "mv_zip" "now" "percentile"
                                     "percentile_over_time" "pi" "pow" "present" "present_over_time"
                                     "qstr" "rate" "repeat" "replace" "reverse" "right" "round"
                                     "round_to" "rtrim" "sample" "scalb" "score" "sha1" "sha256"
                                     "signum" "sin" "sinh" "space" "split" "sqrt" "starts_with"
                                     "stddev_over_time" "std_dev" "st_centroid_agg" "st_contains"
                                     "st_disjoint" "st_distance" "st_envelope" "st_extent_agg"
                                     "st_geohash" "st_geohex" "st_geotile" "st_intersects" "st_npoints"
                                     "st_simplify" "st_within" "st_x" "st_xmax" "st_xmin" "st_y"
                                     "st_ymax" "st_ymin" "substring" "sum" "sum_over_time" "tan" "tanh"
                                     "tau" "tbucket" "text_embedding" "top" "top_snippets"
                                     "to_aggregate_metric_double" "to_base64" "to_boolean"
                                     "to_cartesianpoint" "to_cartesianshape" "to_dateperiod"
                                     "to_datetime" "to_date_nanos" "to_degrees" "to_dense_vector"
                                     "to_double" "to_geohash" "to_geohex" "to_geopoint" "to_geoshape"
                                     "to_geotile" "to_integer" "to_ip" "to_long" "to_lower"
                                     "to_radians" "to_string" "to_timeduration" "to_unsigned_long"
                                     "to_upper" "to_version" "trange" "trim" "url_decode" "url_encode"
                                     "url_encode_component" "values" "variance" "variance_over_time"
                                     "v_cosine" "v_dot_product" "v_hamming" "v_l1_norm" "v_l2_norm"
                                     "weighted_avg")))

  "Elasticsearch ES|QL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-sqlite-font-lock-keywords'.")

(sql-set-product-feature 'esql
                         :font-lock
                         'es-esql-mode-font-lock-keywords)

(sql-set-product-feature 'esql
                         :syntax-alist '((?# . "_")))

(defcustom es-esql-program "esql"
  "Command to start interactive ES|QL commands for Elasticsearch"
  :type 'file
  :group 'SQL)

(sql-set-product-feature 'esql
                         :sqli-program 'es-esql-program)

(sql-set-product-feature 'esql
                         :prompt-regexp "^esql> ")

(sql-set-product-feature 'esql
                         :prompt-length 6)

(defcustom es-esql-login-params '(user password database)
  "Login parameters needed to connect to Elasticsearch"
  :type 'sql-login-params
  :group 'SQL)

(sql-set-product-feature 'esql
                         :sqli-login 'es-esql-login-params)

(defcustom es-esql-cli-options '()
  "List of additional options for `es-esql-program'."
  :type '(repeat string)
  :group 'SQL)

(sql-set-product-feature 'esql
                         :sqli-options 'es-esql-cli-options)

(defun es-esql-comint-esql (product options &optional buf-name)
  "Create comint buffer and connect to Elasticsearch."
  ;; Do something with `sql-user', `sql-password',
  ;; and `sql-server'.
  (let ((params
         (append
          (if (not (string= "" sql-user))
              (list "-U" sql-user))
          (if (not (string= "" sql-password))
              (list "-P" sql-password))
          (if (not (string= "" sql-database))
              (list "-S" sql-database))
          options)))
    (message "Connecting to ESQL %s" params)
    (sql-comint product params buf-name)))

(sql-set-product-feature 'esql
                         :sqli-comint-func 'es-esql-comint-esql)

(defun es-esql (&optional buffer)
  "Run Elasitcsearch ESQL as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.
"
  (interactive "P")
  (sql-product-interactive 'esql buffer))
