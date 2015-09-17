;;; es-parse.el --- Parse Elasticsearch JSON into useful things

;; Copyright (C) 2015 Matthew Lee Hinman

;;; Commentary:

;; Provides helpers for parsing ES responses into useful elisp

;;; Usage:

;; Add to your Emacs config:
;;  (add-to-list 'load-path "/path/to/es-mode-dir")
;;  (require 'es-parse)

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

(require 'json)
(require 'dash)

(defun es-bucket-to-string (bucket)
  (format "| %s | %s "
          (gethash "key" bucket)
          (gethash "doc_count" bucket)))

(defun es-parse-histogram-to-table (agg-string agg-name)
  (let* ((json-object-type 'hash-table)
         (data (json-read-from-string agg-string))
         (aggs (gethash "aggregations" data))
         (buckets (->> aggs                       
                       (gethash agg-name)
                       (gethash "buckets")))
         (lines (mapcar 'es-bucket-to-string buckets)))
    (concat " key | document count \n|--+--|\n"
     (mapconcat 'identity lines "\n"))))


(defvar test-resp "{\"took\":1,\"timed_out\":false,\"_shards\":{\"total\":5,\"successful\":5,\"failed\":0},\"hits\":{\"total\":10,\"max_score\":0.0,\"hits\":[]},\"aggregations\":{\"prices\":{\"buckets\":[{\"key\":0,\"doc_count\":2},{\"key\":50,\"doc_count\":4},{\"key\":100,\"doc_count\":0},{\"key\":150,\"doc_count\":3}]}}}\n")

(provide 'es-parse)
;;; es-parse.el ends here
