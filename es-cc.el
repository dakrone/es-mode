;;; es-cc.el --- The Elasticsearch command center

;; Copyright (C) 2016 Matthew Lee Hinman

;; Author: Lee Hinman <lee@writequit.org>
;; URL: http://www.github.com/dakrone/es-mode
;; Version: 1.0.0
;; Keywords: elasticsearch
;; Package-Requires: ((dash "2.11.0") (cl-lib "0.5") (spark "1.0") (s 1.11.0))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a command center for monitoring Elasticsearch clusters

;;; Usage:

;; Invoke M-x es-command-center to open the command center, you can then hit 'g'
;; to refresh the buffer manually

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
(require 'spark)
(require 'url)
(require 'url-handlers)
(require 'url-parse)
(require 'url-util)
(require 'dash)

(defgroup es-cc nil
  "Elasticsearch command center"
  :group 'monitoring)

(defcustom es-cc-endpoint "http://localhost:9200/"
  "The endpoint to be monitored"
  :group 'es-cc
  :type 'string)

(defcustom es-cc-metric-history 55
  "Number of historical metrics to keep for graphs"
  :group 'es-cc
  :type 'integer)

(defcustom es-cc-refresh-interval 5
  "Refresh the command center buffer in this many seconds."
  :group 'es-cc
  :type 'integer)

(defun es-cc-get-nodes-stats-endpoint ()
  "Return the nodes stats API endpoint"
  (concat es-cc-endpoint
          (if (s-ends-with-p "/" es-cc-endpoint)
              ""
            "/")
          "_nodes/stats"))

(defun es-cc-get-health-endpoint ()
  "Return the health API endpoint"
  (concat
   es-cc-endpoint
   (if (s-ends-with-p "/" es-cc-endpoint)
       ""
     "/")
   "_cat/health?v&h=cluster,status,node.total,node.data,"
   "shards,unassign,active_shards_percent"))

(defun es-cc-get-indices-endpoint ()
  "Return the indices API endpoint"
  (concat
   es-cc-endpoint
   (if (s-ends-with-p "/" es-cc-endpoint)
       ""
     "/")
   "_cat/indices?v&h=index,health,pri,rep,docs.count,store.size"))

(defun es-cc-get-shards-endpoint ()
  "Return the indices API endpoint"
  (concat es-cc-endpoint
          (if (s-ends-with-p "/" es-cc-endpoint)
              ""
            "/")
          "_cat/shards?v"))

(defvar es-cc--bounds-for-metric
  '(:mem
    (:min 0 :max 100 :title "Memory Usage (%)")
    :cpu
    (:min 0 :max 100 :title "CPU Usage (%)")
    :load
    (:min 0 :max :auto :title "Load Average (1m)"))
  "Bounds for spark line for different metric names")

(defvar es-cc--refresh-timer nil
  "Timer value for automatic refresh")

(defvar es-cc--node-history '()
  "Var used to store node cpu/mem/load history")

(defvar es-cc--cluster-health-string
  "Loading cluster health..."
  "Var used to store cluster health output")

(defvar es-cc--indices-health-string
  "Loading indices health..."
  "Var used to store indices health output")

(defvar es-cc--shards-health-string
  "Loading shard health..."
  "Var used to store shard health output")

(defun es-cc--get-node-readable-id (node-id nodes-plist)
  "Return a string suitable for a label for the node."
  (-> nodes-plist
      (plist-get node-id)
      (plist-get :name)))

(defun es-cc--drop-colon (symbol)
  "Take a symbol like `:foo' and return a string like \"foo\"."
  (substring-no-properties (symbol-name symbol) 1))

(defun es-cc--get-node-pretty-string (node-id nodes-plist)
  "Given a `node-id' and the nodes stats plist, return a nice
multi-line string for the node."
  (let* ((node-info (plist-get nodes-plist node-id))
         (name (plist-get node-info :name))
         (host (plist-get node-info :host)))
    (format "name: %s\nhost: %s\n  id: %s\n"
            name host (es-cc--drop-colon node-id))))

(defun es-cc--spark-v-for-metric (info-plist metric)
  "Given a `metric' keyword and info, return the spark-v string
for all the nodes for that metric."
  (let* ((node-ids (-map 'first (-partition 2 info-plist)))
         (metric-of-nodes (-map (lambda (id)
                                  (-> (plist-get info-plist id)
                                      (plist-get metric)))
                                node-ids))
         (bounds (plist-get es-cc--bounds-for-metric metric))
         (min (plist-get bounds :min))
         (min-val (cond
                   ((numberp min)
                    min)
                   ((eq min :auto)
                    (-min metric-of-nodes))
                   (t 0)))
         (max (plist-get bounds :max))
         (max-val (cond
                   ((numberp max)
                    max)
                   ((eq max :auto)
                    (-max metric-of-nodes))
                   (t 10)))
         (labels (-map (lambda (id)
                         (es-cc--get-node-readable-id id info-plist))
                       node-ids))
         (title (plist-get bounds :title)))
    (spark-v metric-of-nodes
             :min min-val
             :max max-val
             :labels labels
             :title title
             :size 78)))

(defun es-cc--spark-h-for-historical-metric (max-len node-id info-plist metric)
  (let ((stats (-> es-cc--node-history
                   (plist-get node-id)
                   (plist-get metric)))
        (format-str (concat "%0" (number-to-string max-len) "s %s")))
    (format format-str
            (es-cc--get-node-readable-id node-id info-plist)
            (if (sequencep stats)
                (spark (reverse stats)
                       :min 0
                       :max 100)
              "unable to load historical information"))))

(defun es-cc--plist-merge (plist-a &rest plist-b)
  "Merge multiple plists into a single plist"
  (-reduce-from
   (lambda (plist-a plist-b)
     (->> (-partition 2 plist-b)
          (-reduce-from
           (lambda (acc it)
             (let ((key (first it))
                   (val (second it)))
               (plist-put acc key val)))
           plist-a)))
   plist-a
   plist-b))

(defun es-cc--node-to-info (node-tuple)
  "Returns plist of node name to plist of metrics"
  (let* ((node-id (first node-tuple))
         (node-stat (second node-tuple))
         (name (plist-get node-stat :name))
         (host (plist-get node-stat :host))
         (mem-pct (-> node-stat
                      (plist-get :jvm)
                      (plist-get :mem)
                      (plist-get :heap_used_percent)))
         (cpu-stats (-> node-stat
                        (plist-get :os)
                        (plist-get :cpu)))
         (cpu-pct (or (-> node-stat
                          (plist-get :os)
                          (plist-get :cpu_percent))
                      (-> cpu-stats (plist-get :percent))
                      ;; 1.7.x support
                      (- 100 (-> cpu-stats (plist-get :idle)))))
         (maybe-load-avg (or (-> node-stat
                                 (plist-get :os)
                                 (plist-get :load_average))
                             (-> cpu-stats
                                 (plist-get :load_average)
                                 (plist-get :1m))))
         (load-avg (if (sequencep maybe-load-avg)
                       ;; 1.7.x support
                       (elt maybe-load-avg 0)
                     maybe-load-avg)))
    ;; (message "NAME: %s, CPU: %s, MEM: %s, LOAD %s"
    ;;          name cpu-pct mem-pct load-avg)
    (plist-put '() node-id
               (-> '()
                   (plist-put :name name)
                   (plist-put :host host)
                   (plist-put :cpu cpu-pct)
                   (plist-put :mem mem-pct)
                   (plist-put :load load-avg)))))

(defun es-cc--build-map-from-nodes-stats (stats-json)
  (let* ((json-object-type 'plist)
         (data (json-read-from-string stats-json))
         (cluster-name (plist-get data :cluster_name))
         (nodes (plist-get data :nodes))
         (node-names (->> nodes (-partition 2) (-map 'first)))
         (node-infos (->> nodes (-partition 2) (-map 'es-cc--node-to-info))))
    (-reduce 'es-cc--plist-merge node-infos)))

;; TODO: use this
(defvar es-cc--history-tracked-stats '(:mem :cpu :load))

(defun es-cc--update-history-from-stats-for-single-node (node-id new-info)
  (let* ((current-history (plist-get es-cc--node-history node-id))
         (old-mem (-take es-cc-metric-history
                         (plist-get current-history :mem)))
         (new-mem (cons (plist-get new-info :mem) old-mem))
         (old-cpu (-take es-cc-metric-history
                         (plist-get current-history :cpu)))
         (new-cpu (cons (plist-get new-info :cpu) old-cpu))
         (old-load (-take es-cc-metric-history
                          (plist-get current-history :load)))
         (new-load (cons (plist-get new-info :load) old-load))
         (new-history (-> nil
                          (plist-put :mem new-mem)
                          (plist-put :cpu new-cpu)
                          (plist-put :load new-load))))
    new-history))

(defun es-cc--update-history-from-stats (nodes-infos-plist)
  "Given the new stats, update the history with the new information"
  (let* ((node-names (->> nodes-infos-plist (-partition 2) (-map 'first)))
         (histories
          (-map
           (lambda (id)
             (plist-put nil
                        id
                        (es-cc--update-history-from-stats-for-single-node
                         id (plist-get nodes-infos-plist id))))
           node-names))
         (new-history (-reduce 'es-cc--plist-merge histories)))
    ;; Actually update history
    (setq es-cc--node-history new-history)))

(defun es-cc--process-nodes-stats (status &optional results-buffer)
  (let* ((http-results-buffer (current-buffer))
         (http-status-code url-http-response-status)
         (http-content-type url-http-content-type)
         (http-content-length url-http-content-length))
    (set-buffer
     (get-buffer-create results-buffer))
    ;; (message "Response: Status: %S Content-Type: %S (%s bytes)"
    ;;          http-status-code http-content-type http-content-length)
    (let ((buffer-read-only nil)
          (current-point (point))
          (temp-url es-cc-endpoint))
      ;; Clear everything
      (delete-region (point-min) (point-max))
      (fundamental-mode)
      ;; Turn on ES-CC mode
      (es-command-center-mode)
      (if (or (equal 'connection-failed (cl-cadadr status))
              (not (numberp http-status-code)))
          (insert "ERROR: Could not connect to server.")
        ;; Set a local var for the URL
        (setq-local es-cc-endpoint temp-url)
        ;; Insert the new stats
        (let* ((body-string (with-temp-buffer
                              (url-insert http-results-buffer)
                              (goto-char (point-min))
                              (search-forward "{" (point-max))
                              (buffer-substring-no-properties
                               (- (point) 1) (point-max))))
               (stats (es-cc--build-map-from-nodes-stats body-string))
               (node-ids (-map 'first (-partition 2 stats)))
               (max-node-len
                (-reduce 'max
                         (-map (lambda (id)
                                 (length
                                  (es-cc--get-node-readable-id id stats)))
                               node-ids))))
          (es-cc--update-history-from-stats stats)
          (insert
           (propertize "* Information" 'face 'outline-1)
           "\n"
           (format "URL: <%s>\n" (propertize es-cc-endpoint 'face 'underline))
           (format-time-string "Last Updated: [%FT%T%z]\n")
           "\n"
           (propertize "* Cluster Information" 'face 'outline-1)
           "\n"
           es-cc--cluster-health-string
           "\n"
           (propertize "* Node Information" 'face 'outline-1)
           "\n"
           (-reduce (lambda (x y) (concat x "\n" y))
                    (-map (lambda (tuple)
                            (es-cc--get-node-pretty-string
                             (first tuple) stats))
                          (-partition 2 stats)))
           "\n"
           (propertize "* Node Memory" 'face 'outline-1)
           (es-cc--spark-v-for-metric stats :mem)
           (-reduce (lambda (x y) (concat x "\n" y))
                    (-map (lambda (tuple)
                            (es-cc--spark-h-for-historical-metric
                             max-node-len
                             (first tuple) stats :mem))
                          (-partition 2 stats)))
           "\n\n"
           (propertize "* Node CPU" 'face 'outline-1)
           (es-cc--spark-v-for-metric stats :cpu)
           (-reduce (lambda (x y) (concat x "\n" y))
                    (-map (lambda (tuple)
                            (es-cc--spark-h-for-historical-metric
                             max-node-len
                             (first tuple) stats :cpu))
                          (-partition 2 stats)))
           "\n\n"
           (propertize "* Node Load" 'face 'outline-1)
           (es-cc--spark-v-for-metric stats :load)
           (-reduce (lambda (x y) (concat x "\n" y))
                    (-map (lambda (tuple)
                            (es-cc--spark-h-for-historical-metric
                             max-node-len
                             (first tuple) stats :load))
                          (-partition 2 stats)))
           "\n\n"
           (propertize "* Index Information" 'face 'outline-1)
           "\n"
           es-cc--indices-health-string
           "\n"
           (propertize "* Shard Information" 'face 'outline-1)
           "\n"
           es-cc--shards-health-string)))
      (goto-char current-point))
    (read-only-mode 1)))

(defun es-cc--process-cluster-health (status &optional results-buffer)
  (let* ((http-results-buffer (current-buffer))
         (body-string (with-temp-buffer
                        (url-insert http-results-buffer)
                        (buffer-substring-no-properties
                         (point-min) (point-max))))
         (http-status-code url-http-response-status))
    (if (or (equal 'connection-failed (cl-cadadr status))
            (not (numberp http-status-code)))
        (setq es-cc--cluster-health-string
              "ERROR: Could not connect to server.\n")
      (setq es-cc--cluster-health-string body-string))))

(defun es-cc--process-indices-health (status &optional results-buffer)
  (let* ((http-results-buffer (current-buffer))
         (body-string (with-temp-buffer
                        (url-insert http-results-buffer)
                        (buffer-substring-no-properties
                         (point-min) (point-max))))
         (http-status-code url-http-response-status))
    (if (or (equal 'connection-failed (cl-cadadr status))
            (not (numberp http-status-code)))
        (setq es-cc--indices-health-string
              "ERROR: Could not connect to server.\n")
      (setq es-cc--indices-health-string body-string))))

(defun es-cc--process-shards-health (status &optional results-buffer)
  (let* ((http-results-buffer (current-buffer))
         (body-string (with-temp-buffer
                        (url-insert http-results-buffer)
                        (buffer-substring-no-properties
                         (point-min) (point-max))))
         (http-status-code url-http-response-status))
    (if (or (equal 'connection-failed (cl-cadadr status))
            (not (numberp http-status-code)))
        (setq es-cc--shards-health-string
              "ERROR: Could not connect to server.\n")
      (setq es-cc--shards-health-string body-string))))

(defun es-cc-get-cluster-health (buffer-name)
  (url-retrieve (es-cc-get-health-endpoint)
                'es-cc--process-cluster-health
                (list buffer-name)
                t t))

(defun es-cc-get-indices-health (buffer-name)
  (url-retrieve (es-cc-get-indices-endpoint)
                'es-cc--process-indices-health
                (list buffer-name)
                t t))

(defun es-cc-get-shards-health (buffer-name)
  (url-retrieve (es-cc-get-shards-endpoint)
                'es-cc--process-shards-health
                (list buffer-name)
                t t))

(defun es-cc-get-nodes-stats (buffer-name)
  (url-retrieve (es-cc-get-nodes-stats-endpoint)
                'es-cc--process-nodes-stats
                (list buffer-name)
                t t))

(defun es-cc-refresh ()
  "Refresh the stats for the current buffer"
  (interactive)
  (es-cc-get-cluster-health (buffer-name))
  (es-cc-get-indices-health (buffer-name))
  (es-cc-get-shards-health (buffer-name))
  (es-cc-get-nodes-stats (buffer-name)))

(defun es-cc--periodic-refresh (buffer-name)
  "Return a function for periodic refresh of command center
  buffer"
  (with-current-buffer buffer-name
    (save-window-excursion
      (save-excursion
        (es-cc-refresh)))))

(defvar es-command-center-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'es-cc-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    map)
  "Keymap used for `es-command-center-mode'.")

(define-minor-mode es-command-center-mode
  "Docstring"
  :init-value nil
  :lighter "ES-CC"
  :group 'es-cc
  :keymap es-command-center-mode-map)

;;;###autoload
(defun es-command-center ()
  "Open the Elasticsearch Command Center"
  (interactive)
  (let ((buffer-name (format "*ES-CC: %s*" es-cc-endpoint)))
    (switch-to-buffer
     (get-buffer-create buffer-name))
    (make-local-variable 'es-cc-endpoint)
    (make-local-variable 'es-cc--node-history)
    (make-local-variable 'es-cc--cluster-health-string)
    (make-local-variable 'es-cc--indices-health-string)
    (make-local-variable 'es-cc--shards-health-string)
    (make-local-variable 'es-cc--refresh-timer)
    ;; Clear everything
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (insert (format "Fetching stats [%s]..." es-cc-endpoint)))
    ;; (set-window-buffer nil buffer-name)
    (setq es-cc--refresh-timer
          (run-at-time nil es-cc-refresh-interval
                        'es-cc--periodic-refresh buffer-name))))

(provide 'es-cc)
;;; es-cc.el ends here
