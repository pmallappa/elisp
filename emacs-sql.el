;; Ensure tunnelier is up and running to redirect ports

(setq sql-oracle-program "sqlplus")
(setq sql-oracle-login-params
      '((user     :default "sales")
        (password :default (password-read "Password:" "webtest"))
        (database :default "localhost:21521/webtst")))

;; to start a sql session, open a file or buffer and put into sql mode
;; then from that buffer, execute the commands
;;   sql-set-product <ret> oracle
;;   sql-product-interactive
;; to open an interactive oracle sql buffer
;;
;; type your commands in the original *SQL* buffer, and use the following
;; commands to send to the active oracle process
;;
;; C-c C-b         sql-send-buffer
;; C-c C-c         sql-send-paragraph
;; C-c TAB         sql-product-interactive
;; C-c C-l         Prefix Command
;; C-c C-r         sql-send-region
;; C-c C-s         sql-send-string
;; 
;; C-M-q           prog-indent-sexp
;; 
;; C-c C-l a       sql-list-all
;; C-c C-l t       sql-list-table

;; Handy SQL queries
;;   select table_name from user_tables order by table_name;
;;   desc <tablename>
;;   set pagesize 9999
;;   set linesize 32767;
;;   column portfolio_name format a40;
;;   column port_solution_name format a40;
;;   column prnbr format a15;
;;   column date_modified format a12;
;;   column modified_by format a15;

;;==============================
;; sqlplus package provides formatted query results
(require 'sqlplus)

;; format display results side-by-side
(setq sqlplus-multi-output-tables-default-flag nil)

;; adjust the table faces to lighten the background up a bit
;; default was -20 and -30
;; note, required changing the source code sqlplus.el to work, commented out lines 3385-3398
;; will send a change proposal to the author
(set-face-background 'sqlplus-table-even-rows-face (sqlplus-shine-color (face-background 'default) -10))
(set-face-background 'sqlplus-table-odd-rows-face  (sqlplus-shine-color (face-background 'default) -20))
(set-face-background 'sqlplus-table-head-face      (sqlplus-shine-color (face-background 'default) -30))

;;==============================
;; use an org table to store sql connection parameters
;; connection string format is:
;; user/pwd@sid
;; e.g. sales/paSSW0rd@localhost:21521/webtst
;; See the password.gpg file for current connections

(require 'org-table)

(defvar sqlplus-x-columns '(sp-service sp-user sp-pwd sp-key))
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
       (is-user-selected (= (org-table-current-column) (+ 1 (position 'sp-user sqlplus-x-columns)))))
    (sqlplus
     (format
      "%s/%s@%s"
      (if is-user-selected
          (thing-at-point 'symbol)
        (nth (position 'sp-user sqlplus-x-columns) cur-row))
      (nth (position 'sp-pwd sqlplus-x-columns) cur-row)
      (nth (position 'sp-service sqlplus-x-columns) cur-row))
     (concat (nth (position 'sp-service sqlplus-x-columns) cur-row) ".sqp"))
    (password-cache-add
     (nth (position 'sp-key sqlplus-x-columns) cur-row)
     (nth (position 'sp-pwd sqlplus-x-columns) cur-row))))

(global-set-key [f4] 'sqlplus-x-connect)

(provide 'emacs-sql)
