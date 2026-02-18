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
