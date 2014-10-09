;; Ensure tunnelier is up and running to redirect ports

;; Handy SQL queries
;;   Display all tables in user space
;;   select table_name from user_tables order by table_name;
;;

(setq sql-oracle-program "sqlplus")
(setq password-cache t)
(setq password-cache-expiry 60)

(setq sql-oracle-login-params
      '((user     :default "sales")
        (password :default (password-read "Password:" "webtst"))
        (database :default "localhost:21521/webtst")))

;;==============================
;; sqlplus package provides formatted query results
(require 'sqlplus)

;; use an org table to store sql connection parameters
;; connection string format is:
;; user/pwd@sid
;; e.g. sales/paSSW0rd@localhost:21521/webtst

(require 'org-table)
;; don't use adjacent tables, present results as expected
(setq sqlplus-multi-output-tables-default-flag t)

;; adjust the table faces to lighten the background up a bit
;; default was -20 and -30
;; note, required changing the source code sqlplus.el to work, commented out lines 3385-3398
;; will send a change proposal to the author
(set-face-background 'sqlplus-table-even-rows-face (sqlplus-shine-color (face-background 'default) -10))
(set-face-background 'sqlplus-table-odd-rows-face (sqlplus-shine-color (face-background 'default) -20))

;; Use org table to store connection info.
;; See the password.gpg file for current connections
(defvar sqlplus-x-columns '(sqlplus-x-service sqlplus-x-user sqlplus-x-pwd))
(defun sqlplus-x-connect ()
  "Build a connection string and make a connection. The point must be in an org-mode table.
Columns of the table must correspond to the `sqlplus-x-columns' variable.
Default table format is

   | Service (sid)          | user  | pwd       |
   |------------------------+-------+-----------|
   | localhost:21521/webtst | sales | <password>|"
  
  (interactive)
  (org-table-force-dataline)
  (let
      ((cur-row (nth (org-table-current-dline) (org-table-to-lisp)))
       (is-user-selected (= (org-table-current-column) (+ 1 (position 'sqlplus-x-user sqlplus-x-columns)))))
    (sqlplus
     (format
      "%s/%s@%s"
      (if is-user-selected
          (thing-at-point 'symbol)
        (nth (position 'sqlplus-x-user sqlplus-x-columns) cur-row))
      (nth (position 'sqlplus-x-pwd sqlplus-x-columns) cur-row)
      (nth (position 'sqlplus-x-service sqlplus-x-columns) cur-row))
     (concat (nth (position 'sqlplus-x-service sqlplus-x-columns) cur-row) ".sqp"))))

(global-set-key [f4] 'sqlplus-x-connect)

