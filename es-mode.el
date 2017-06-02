;;; es-mode.el --- A major mode for editing Elasticsearch queries -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2014 Matthew Lee Hinman
;; Copyright (C) 2014 Bjarte Johansen

;; Author: Lee Hinman <lee@writequit.org>
;; URL: http://www.github.com/dakrone/es-mode
;; Version: 4.3.0
;; Keywords: elasticsearch
;; Package-Requires: ((dash "2.11.0") (cl-lib "0.5") (spark "1.0") (s "1.11.0") (request "0.3.0"))

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

(require 'cl-lib)
(require 'js)
(require 'url)
(require 'url-handlers)
(require 'url-parse)
(require 'url-util)
(require 'request)

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

(defcustom es-default-headers
  '(("Content-Type" . "application/json; charset=UTF-8"))
  "The default request headers. Note that removing the
`Content-Type' header from this list may cause Elasticsearch to
throw exceptions, it is recommended that you only *add* to this
list"
  :group 'es
  :type 'list)

(defcustom es-prompt-url nil
  "Non-nil means prompt user for requested URL on each query
  evaluation."
  :group 'es
  :type 'boolean)

(defvar es-endpoint-url nil
  "The current URL used as the Elasticsearch endpoint.")

(defvar es-endpoint-url-history (list es-default-url)
  "The history over used URLs.")

(defvar es-response-success-functions nil
  "Abnormal hook called with the Elasticsearch 2xx
  response. Functions in this list take 3 arguments: the response
  status (as an integer), the Content-Type header (i.e,
  text/html), and the buffer containing the response data.")

(defvar es-response-failure-functions nil
  "Abnormal hook called with the Elasticsearch non-2xx
  response. Functions in this list take 3 arguments: the response
  status (as an integer), the Content-Type header (i.e,
  text/html), and the buffer containing the response data.")

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

(defcustom es-always-pretty-print nil
  "If set to `t', results will always be pretty printed without
having to add the `?pretty' flag to the URL manually."
  :group 'es-mode
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

(eval-and-compile
  (defvar es-top-level-fields
    '("aggregations" "aggs" "facets" "filter" "post_filter" "query" "highlight"
      "rescore" "scroll" "scroll_id" "indices_boost" "min_score")
    "Top-level query and filter containers"))

(eval-and-compile
  (defvar es-keywords
    '("fields" "from" "size" "highlight" "_name" "_cache" "_cache_key"
      "_source" "script_fields" "docvalue_fields" "explain" "version" "collapse"
      "search_after" "profile")
    "Top-level fields supported by all queries")

  (defvar es-warnings
    '("DELETE")
    "HTTP methods that should be highlighted as warnings")

  (defvar es-http-builtins
    '("GET" "OPTIONS" "PATCH" "POST" "PUT")
    "HTTP methods used by curl")

  (defvar es-http-builtins-all
    (cons "DELETE" es-http-builtins)
    "HTTP methods, including `DELETE'")

  (defconst es-vars
    '( ;;; Parent types (combiners)
      #("bool" 0 1
        (:type "parent" :summary "Parent combining multiple filters/queries"))
      #("filtered" 0 1
        (:type "parent" :summary "Parent query combining a filter and a query"))
      #("and" 0 1
        (:type "parent" :summary "Parent combining multiple filters/queries, prefer <bool>"))
      #("or" 0 1
        (:type "parent" :summary "Parent combining multiple filters/queries, prefer <bool>"))
      #("not" 0 1
        (:type "parent" :summary "Parent combining multiple filters/queries, prefer <bool>"))
      #("constant_score" 0 1
        (:type "parent" :summary "Parent query wrapping a filter returning a constant score value"))

;;; Both queries and filter
      #("term" 0 1
        (:type "both" :summary "Query or filter that does not analyze the text"))
      #("match_all" 0 1
        (:type "both" :summary "Query or filter matching every document"))
      #("has_child" 0 1
        (:type "both" :summary "Query or filter for parent documents with matching children"))
      #("has_parent" 0 1
        (:type "both" :summary "Query or filter for child documents with matching parents"))
      #("nested" 0 1
        (:type "both" :summary "Query or filter for surrounding documents with matching nested docs"))
      #("prefix" 0 1
        (:type "both" :summary "Query or filter for terms with a given prefix"))
      #("regexp" 0 1
        (:type "both" :summary "Query or filter for terms matching a given regular expression"))

;;; Queries
      #("match" 0 1
        (:type "query" :summary "Query that analyzes the search term according to the field's analyzer"))
      #("multi_match" 0 1
        (:type "query" :summary "Query similar to `match' query for multiple fields"))
      #("boosting" 0 1
        (:type "query" :summary "Query promoting or demoting results matching a query"))
      #("common" 0 1
        (:type "query" :summary "Query with cutoff for common terms"))
      #("dis_max" 0 1
        (:type "query" :summary "Query for disjuntive max of multiple queries"))
      #("fuzzy_like_this" 0 1
        (:type "query" :summary "Query for other documents like the query text"))
      #("fuzzy_like_this_field" 0 1
        (:type "query" :summary "Query for other documents like the query text using a certain field"))
      #("function_score" 0 1
        (:type "query" :summary "Query with custom scoring functions"))
      #("fuzzy" 0 1
        (:type "query" :summary "Query for matching terms using Levenshtein distance"))
      #("more_like_this" 0 1
        (:type "query" :summary "Query for other documents like a particular document"))
      #("more_like_this_field" 0 1
        (:type "query" :summary "Query for other documents like a particular document using a certain field"))
      #("query_string" 0 1
        (:type "query" :summary "Query for documents with Lucene's powerful but error-prone query string syntax"))
      #("simple_query_string" 0 1
        (:type "query" :summary "Query for documents with the simple query string syntax"))
      #("span_first" 0 1
        (:type "query" :summary "Matches spans near the beginning of a field"))
      #("span_multi" 0 1
        (:type "query" :summary "Wrap a multi term query as a span query"))
      #("span_near" 0 1
        (:type "query" :summary "Matches spans which are near one another"))
      #("span_not" 0 1
        (:type "query" :summary "Removes matches which overlap with another span query"))
      #("span_or" 0 1
        (:type "query" :summary "Matches the union of its span clauses"))
      #("span_term" 0 1
        (:type "query" :summary "Matches spans containing a term"))
      #("wildcard" 0 1
        (:type "query" :summary "Query matching documents that have fields matching a wildcard expression (not analyzed)"))
      #("inner_hits" 0 1
        (:type "query" :summary "Return the matched child or nested documents"))
      #("top_children" 0 1
        (:type "filter" :summary "Execute a child query, and out of the hit docs, aggregates it into parent docs"))

;;; Filters
      #("range" 0 1
        (:type "filter" ;; technically both, but only should be used as a filter
               :summary "Filter between two numeric or lexographic values"))
      #("geoshape" 0 1
        (:type "filter" :summary "Filter documents inside shape"))
      #("ids" 0 1
        (:type "filter" :summary "Filter documents by id"))
      #("indices" 0 1
        (:type "filter" :summary "Filter documents differently depending on matching or not matching a document"))
      #("terms" 0 1
        (:type "filter" :summary "Filter documents with an array of terms"))
      #("exists" 0 1
        (:type "filter" :summary "Filter documents where a specific field has a value in them"))
      #("geo_bbox" 0 1
        (:type "filter" :summary "Filter documents inside of a geographical bounding box"))
      #("geo_distance" 0 1
        (:type "filter" :summary "Filter documents within the distance from a point"))
      #("geo_distance_range" 0 1
        (:type "filter" :summary "Filter documents inside a distance range from a point"))
      #("geo_polygon" 0 1
        (:type "filter" :summary "Filter documents falling inside a geographic polygon"))
      #("geoshape" 0 1
        (:type "filter" :summary "Filter documents falling inside a geoshape"))
      #("geohash_cell" 0 1
        (:type "filter" :summary "Filter documents falling inside a geohash cell"))
      #("limit" 0 1
        (:type "filter" :summary "Filter limiting the number of documents (per shard) to execute on"))
      #("missing" 0 1
        (:type "filter" :summary "Filter documents missing a specific field"))
      #("script" 0 1
        (:type "filter" :summary "Filter with an arbitrary script"))
      #("type" 0 1
        (:type "filter" :summary "Filter based on document type"))

;;; Metric Aggregations
      #("min" 0 1
        (:type "metric agg" :summary "Aggregation for minimum value"))
      #("max" 0 1
        (:type "metric agg" :summary "Aggregation for maximum value"))
      #("sum" 0 1
        (:type "metric agg" :summary "Aggregation for sum of values"))
      #("avg" 0 1
        (:type "metric agg" :summary "Aggregation for average of values"))
      #("stats" 0 1
        (:type "metric agg" :summary "Aggregation calculating statistics of numeric values"))
      #("extended_stats" 0 1
        (:type "metric agg" :summary "Aggregation calculating extended statistics of numeric values"))
      #("value_count" 0 1
        (:type "metric agg" :summary "Aggregation counting number of values extracted from field"))
      #("percentiles" 0 1
        (:type "metric agg" :summary "Aggregation calculating percentiles of numeric values"))
      #("percentile_ranks" 0 1
        (:type "metric agg" :summary "Aggregation calculating percentile rank of numeric values"))
      #("cardinality" 0 1
        (:type "metric agg" :summary "Aggregation calculating cardinality of a field"))
      #("geo_bounds" 0 1
        (:type "metric agg" :summary "Aggregation within geo bounding box"))
      #("top_hits" 0 1
        (:type "metric agg" :summary "Aggregation of results within a bucket (join)"))
      #("significant_terms" 0 1
        (:type "metric agg" :summary "Aggregation returning interesting or unusual occurrences of terms in a set"))

;;; Bucket Aggregations
      #("global" 0 1
        (:type "bucket agg" :summary "Aggregation returning all results regardless of scope"))
      #("terms" 0 1
        (:type "bucket agg" :summary "Aggregation calculating most or least common terms"))
      #("reverse_nested" 0 1
        (:type "bucket agg" :summary "Aggregation for reverse nested documents"))
      #("range" 0 1
        (:type "bucket agg" :summary "Aggregation of documents within ranges"))
      #("date_range" 0 1
        (:type "bucket agg" :summary "Aggregation of documents within a date range"))
      #("ip_range" 0 1
        (:type "bucket agg" :summary "Aggregation of documents within an IP address range"))
      #("geohash_grid" 0 1
        (:type "bucket agg" :summary "Aggregation that works on geo_point fields and groups points into buckets that represent cells in a grid"))
      #("missing" 0 1
        (:type "bucket agg" :summary "Aggregation of documents missing a field value"))
      #("histogram" 0 1
        (:type "bucket agg" :summary "Aggregation of documents within numeric slices"))
      #("date_histogram" 0 1
        (:type "bucket agg" :summary "Aggregation of documents within date slices"))
      #("filters" 0 1
        (:type "bucket agg" :summary "Aggregation bucketing documents into buckets defined by filters"))
      #("sampler" 0 1
        (:type "bucket agg" :summary "Aggregation used to limit any sub aggregations' processing to a sample of the top-scoring documents"))
      #("diversified_sampler" 0 1
        (:type "bucket agg" :summary "Aggregation used to limit any sub aggregations' processing to a sample of the top-scoring documents"))
      #("children" 0 1
        (:type "bucket agg" :summary "Aggregation that enables aggregating from buckets on parent document types to buckets on child documents"))

;;; Matrix Aggregations
      #("adjacency_matrix" 0 1
        (:type "matrix agg" :summary "A matrix aggregation returning a form of adjacency matrix"))
      #("matrix_stats" 0 1
        (:type "matrix agg" :summary "A matrix aggregation calculating numeric aggregation over a set of document fields"))

;;; Pipeline Aggregations
      #("avg_bucket" 0 1
        (:type "pipeline agg" :summary "Calculates the (mean) average value of a specified metric in a sibling aggregation"))
      #("derivative" 0 1
        (:type "pipeline agg" :summary "Calculates the derivative of a specified metric in a parent histogram (or date_histogram) aggregation"))
      #("max_bucket" 0 1
        (:type "pipeline agg" :summary "Identifies the bucket(s) with the maximum value of a specified metric in a sibling aggregation"))
      #("min_bucket" 0 1
        (:type "pipeline agg" :summary "Identifies the bucket(s) with the minimum value of a specified metric in a sibling aggregation"))
      #("sum_bucket" 0 1
        (:type "pipeline agg" :summary "Calculates the sum across all bucket of a specified metric in a sibling aggregation"))
      #("stats_bucket" 0 1
        (:type "pipeline agg" :summary "Calculates a variety of stats across all bucket of a specified metric in a sibling aggregation"))
      #("extended_stats_bucket" 0 1
        (:type "pipeline agg" :summary "Calculates a variety of stats (more than regular stats) across all bucket of a specified metric in a sibling aggregation"))
      #("percentiles_bucket" 0 1
        (:type "pipeline agg" :summary "Calculates percentiles across all buckets of a specified metric in a sibling aggregation"))
      #("moving_avg" 0 1
        (:type "pipeline agg" :summary "Given an ordered series of data, will slide a window across the data and emit the average value of that window"))
      #("cumulative_sum" 0 1
        (:type "pipeline agg" :summary "Calculates the cumulative sum of a specified metric in a parent histogram (or date_histogram) aggregation"))
      #("bucket_script" 0 1
        (:type "pipeline agg" :summary "Executes a script which can perform per bucket computations on specified metrics in the parent aggregation"))
      #("bucket_selector" 0 1
        (:type "pipeline agg" :summary "Executes a script which determines whether the current bucket will be retained in the parent multi-bucket aggregation"))
      #("serial_diff" 0 1
        (:type "pipeline agg" :summary "A technique where values in a time series are subtracted from itself at different time lags or periods"))
      )
    "Vars used for query and filter completion"))

(eval-and-compile
  (defun es-extract-type-raw (s)
    "Extract the type of operation from the var, without formatting"
    (get-text-property 0 :type s)))

(defun es-extract-type (s)
  "Extract the type of operation from the var"
  (format " [%s]" (get-text-property 0 :type s)))

(defun es-extract-summary (s)
  "Extract the summary of the operation from the var"
  (get-text-property 0 :summary s))

(eval-and-compile
  (defvar es-facet-types
    (cl-remove-if-not (lambda (c) (string= "agg" (es-extract-type-raw c))) es-vars)
    "Facets/Aggregations")

  (defvar es-parent-types
    (cl-remove-if-not (lambda (c) (string= "parent" (es-extract-type-raw c))) es-vars)
    "Compound queries that always contain additional queries or filters")

  (defvar es-query-types
    (cl-remove-if-not (lambda (c) (or (string= "filter" (es-extract-type-raw c))
                                      (string= "query" (es-extract-type-raw c))
                                      (string= "both" (es-extract-type-raw c))))
                      es-vars)
    "Various leaf-type queries and filters"))

(defconst es--method-url-regexp
  (concat "^\\("
          (regexp-opt es-http-builtins-all)
          "\\) \\(.*\\)$")
  "A regex to get the method and url from a line.")

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
  (let ((url (or (and (not es-prompt-url) es-endpoint-url)
                 (command-execute 'es-set-endpoint-url))))

    (if (and (not (string-prefix-p "http://" url t))
             (not (string-prefix-p "https://" url t)))
        (concat "http://" url)
      url)))

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

(defun es--fix-url (url)
  "Transforms the URL so that we can use it to send a request."
  (cond ((not (string-prefix-p "http://" url))
         (let ((url (if (string-prefix-p "/" url)
                        url
                      (concat "/" url))))
           (let ((base (url-generic-parse-url
                        (let ((es-default-url
                               (url-generic-parse-url
                                (or es-endpoint-url es-default-url))))
                          (setf (url-filename es-default-url) url)
                          (setq es-default-url
                                (url-recreate-url es-default-url))
                          (es-get-url)))))
             (setf (url-filename base)
                   (if (string-prefix-p "/" url)
                       url
                     (concat "/" url)))
             (url-recreate-url base))))
        (t url)))

(defun es--find-params ()
  "Search backwards to find text like \"POST /_search\",
  returning a list of method and full URL, prepending
  `es-default-base' to the URL. Returns `false' if no parameters
  are found."
  (save-excursion
    (if (search-backward-regexp es--method-url-regexp nil t)
        (let ((method (match-string-no-properties 1))
              (uri (match-string-no-properties 2)))
          `(,method . ,(es--fix-url uri)))
      (message "Could not find <method> <url> parameters!")
      nil)))

(defun es-old-company-backend (command &optional arg &rest ign)
  "The old `company-backend' for es-queries and facets."
  (cl-case command
    (prefix (let ((sym (company-grab-symbol)))
              (if (string-match "\"\\(.*\\)\"?" sym)
                  (match-string-no-properties 1 sym)
                sym)))
    (candidates
     (all-completions
      arg
      (append es-top-level-fields es-query-types es-facet-types
              es-parent-types es-keywords)))))

(defun es-company-backend (command &optional arg &rest ignored)
  "A `company-backend' for es-queries and facets. Displays metadata about the
 completion, if available."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'es-company-backend))
    (prefix (and (derived-mode-p 'es-mode)
                 (let ((sym (company-grab-symbol)))
                   (if (string-match "\"\\(.*\\)\"?" sym)
                       (match-string-no-properties 1 sym)
                     sym))))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      es-vars))
    (annotation (es-extract-type arg))
    (meta (es-extract-summary arg))))

(defun es-extract-warnings ()
  "Extract the warning headers from the current buffer. Assumes
  the buffer contains the result of a `url-retrieve' request."
  (goto-char (point-min))
  (when (re-search-forward "Warning: " (point-max) t)
    (let ((start (point))
          (end (progn (end-of-line) (point))))
      (buffer-substring-no-properties start end))))

(defun es-result--handle-response (data response error-thrown)
  "Handles the response from the server after sending a request."
  (let ((buffer-read-only nil)
        (http-warnings (request-response-header response "warning"))
        (http-content-type (request-response-header response "content-type"))
        (http-content-length (request-response-header response "content-length"))
        (http-status-code (request-response-status-code response)))
    (message "Response: Status: %S Content-Type: %S (%s bytes)"
             http-status-code http-content-type http-content-length)
    (erase-buffer)
    (if error-thrown
        (if data
            (insert data)
          (insert "ERROR: Could not connect to server."))
      (when http-warnings
        (insert "// Warning: " http-warnings "\n"))
      (insert data))
    (es-result-mode)
    (cond
     ((and (>= http-status-code 200) (<= http-status-code 299))
      (run-hook-with-args 'es-response-success-functions
                          http-status-code
                          http-content-type
                          (current-buffer)))
     (t
      (run-hook-with-args 'es-response-failure-functions
                          http-status-code
                          http-content-type
                          (current-buffer))))
    (if error-thrown
        (setq mode-name "ES[failed]")
      (setq mode-name "ES[finished]"))))

(defun es--warn-on-delete-yes-or-no-p (request-method)
  (or (not (string= "DELETE" (upcase request-method)))
      (not es-warn-on-delete-query)
      (yes-or-no-p
       ;; This will not font-lock if `yes-or-no-p' is aliased to
       ;; `y-or-n-p'.
       (propertize
        "Do you really want to send a DELETE request?"
        'font-lock-face 'font-lock-warning-face))))

(defun es--munge-url (url)
  "Do whatever is necessary to mutate the ES `url' before the
  HTTP request is sent. Currently only checks
  `es-always-pretty-print' and adds the `pretty' parameter as
  needed."
  (if (and es-always-pretty-print
           ;; If a user has already specified a 'pretty' option, don't modify
           ;; url so it can be overridden
           (not (string-match (regexp-opt '("?pretty" "&pretty")) url)))
      (if (string-match "\\?" url)
          (concat url "&pretty=true")
        (concat url "?pretty=true"))
    url))

(defvar es--query-number 0)

(defun es--execute-region ()
  "Submits the active region as a query to the specified
endpoint. If the region is not active, the whole buffer is
used. Uses the params if it can find them or alternativly the
vars."
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (params (or (es--find-params)
                     `(,(es-get-request-method) . ,(es-get-url))))
         (url (es--munge-url (cdr params)))
         (url-request-method (car params))
         (request-data (buffer-substring-no-properties beg end)))
    (let ((result-buffer-name (if (zerop es--query-number)
                                          (format "*ES: %s*" (buffer-name))
                                        (format "*ES: %s [%d]*"
                                                (buffer-name)
                                                es--query-number))))
      (when (es--warn-on-delete-yes-or-no-p url-request-method)
        (message "Issuing %s against %s" url-request-method url)
        (request
         url
         :type url-request-method
         :parser 'buffer-string
         :headers es-default-headers
         :data (encode-coding-string request-data 'utf-8)
         :timeout 600 ;; timeout of 10 minutes
         :complete (cl-function
                    (lambda (&key data response error-thrown &allow-other-keys)
                      (with-current-buffer (get-buffer-create result-buffer-name)
                        (es-result--handle-response data response error-thrown)))))
        (setq es-results-buffer (get-buffer result-buffer-name))
        (save-selected-window
          ;; We want 2 buffers next to each other if it's not already visible, so
          ;; delete other buffers
          (when (and es-results-buffer
                     (not (get-buffer-window es-results-buffer)))
            (delete-other-windows)
            (view-buffer-other-window es-results-buffer)))))))

(defun es--at-current-header-p ()
  "Returns t if at on a header line, nil otherwise."
  (looking-at (concat "^" (regexp-opt es-http-builtins-all) " .*$")))

(defun es-mark-request-body ()
  "Sets point to the beginning of the request body and mark at
the end. May modify the current point."
  (interactive)
  (let (startpoint
        endpoint)
    (beginning-of-line)
    (forward-line 1)
    (es-goto-previous-request)
    (forward-line 1)
    (setq startpoint (point))
    (if (es-goto-next-request)
        (forward-line -1)
      (goto-char (point-max)))
    (setq endpoint (point))
    (goto-char startpoint)
    (set-mark endpoint)))

(defun es-goto-previous-request ()
  "Advance the point to the previous parameter declaration, if
available. Returns truthy if one was found, nil otherwise."
  (interactive)
  (re-search-backward es--method-url-regexp (point-min) t))

(defun es-goto-next-request ()
  "Advance the point to the next parameter declaration, if
available. Returns truthy if one was found, nil otherwise."
  (interactive)
  ;; Go forward 1 char so being at a request doesn't keep the cursor at the
  ;; request permanently.
  (when (not (eobp))
    (forward-char 1)
    (prog1
        (re-search-forward es--method-url-regexp (point-max) t)
      (beginning-of-line))))

(defmacro es-save-everything (&rest args)
  `(,(if (fboundp 'save-mark-and-excursion)
         'save-mark-and-excursion
       'save-excursion)
    ,@args))

(defun es-execute-request-dwim (prefix)
  "Executes a request with parameters if found, otherwises
assumes that the user wants to be prompted for a method/url to
send the region as a request to/use the predefined vars. Does not
move the point. If a prefix, `C-u', is given, all the requests in
the buffer is executed from top to bottom."
  (interactive "P")
  (es-save-everything
   (when prefix
     (goto-char (point-min))
     (setq es--query-number 1))
   (es-mark-request-body)
   (es--execute-region)
   (when prefix
     (while (es-goto-next-request)
       (setq es--query-number (1+ es--query-number))
       (es-mark-request-body)
       (es--execute-region))
     (setq es--query-number 0))))

(defun es-result-show-response ()
  "Shows the header of the response from the server in the
  minibuffer."
  (interactive)
  (message es-result-response))

(defvar es-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'es-result-show-response)
    (define-key map (kbd "q") 'bury-buffer)
    map)
  "Keymap for `es-result-mode'")

(define-derived-mode es-result-mode text-mode "ES[waiting]"
  "Major mode to hold the result from a query to elastic search end point.
\\{es-result-mode-map}"
  ;; Use es-mode syntax-table
  (set-syntax-table es-mode-syntax-table)
  ;; Use es-mode font-lock
  (setq font-lock-defaults '(es-font-lock-keywords))
  (make-local-variable 'es-result-response)
  ;; Also set comment chars in results mode
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+[\t ]*"))

(defun es-indent-line ()
  "Indent current line as ES code. Uses the same indention as js-mode."
  (interactive)
  (let ((empty-slash (save-excursion
                       (beginning-of-line)
                       (forward-char -2)
                       (looking-at-p "/$"))))
    (if empty-slash
        (indent-line-to 0)
      ;; Dynamically bind js-indent-level so we can have our own indent offset
      ;; if we want to.
      (let ((js-indent-level es-indent-offset))
        (js-indent-line))))
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
      (,(concat "^\\("
                (regexp-opt es-http-builtins-all)
                "\\) \\([^[:space:]\n]*\\)")
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
    (define-key map (kbd "C-c C-p") 'es-goto-previous-request)
    (define-key map (kbd "C-c C-n") 'es-goto-next-request)
    (define-key map (kbd "C-c C-c") 'es-execute-request-dwim)
    (define-key map (kbd "C-c C-u") 'es-set-endpoint-url)
    (define-key map (kbd "C-c C-m") 'es-set-request-method)
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

  ;; Required to make bulk requests without a training newline work
  (setq-local require-final-newline t)

  ;; If we have company-mode we use it.
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'es-company-backend)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.es\\'" . es-mode))

(provide 'es-mode)

;;; es-mode.el ends here
