;; place the name of the function in scope within the mode-line
(which-function-mode)
(add-to-list 'which-function-mode 'emacs-lisp-mode)
(setq which-func-unknown "n/a")

;;; Show the current function name in the header line instead of the modeline
;(setq-default header-line-format
;              '((which-func-mode ("" which-func-format " "))))
;(setq mode-line-misc-info
;            ;; We remove Which Function Mode from the mode line, because it's mostly
;            ;; invisible here anyway.
;            (assq-delete-all 'which-func-mode mode-line-misc-info))

;; define keys to helpfule commands (C-h prefix for each)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; elisp slime nave provides a good way to browse elisp sources
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
(add-hook hook 'elisp-slime-nav-mode))


;;======================================================================
;; Look up a word under the point in an online dictionary
;; From: Xah Lee <xah@xahlee.org>
;; Newsgroups: gnu.emacs.help
;; Date: Fri, 11 Apr 2008 13:25:31 -0700 (PDT)
(defun word-definition-lookup ()
"Look up the word under cursor in a browser."
 (interactive)
 (w3m-goto-url
  (concat
   "http://www.answers.com/main/ntquery?s="
   (thing-at-point 'word))))

;;======================================================================
;; Find a function in the elisp manual
;; From: lawrence mitchell <wence@gmx.li>
;; Find the function under the point in the elisp manual
;;
;; C-h TAB runs the command info-lookup-symbol
;;    which is an interactive autoloaded Lisp function in `info-look'.
;; [Arg list not available until function definition is loaded.]
;;
;; Display the definition of SYMBOL, as found in the relevant manual.
;; When this command is called interactively, it reads SYMBOL from the minibuffer.
;; In the minibuffer, use M-n to yank the default argument value
;; into the minibuffer so you can edit it.
;; The default symbol is the one found at point.
;;
;; With prefix arg a query for the symbol help mode is offered.
(defun find-function-in-elisp-manual (function)
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val
	   (completing-read
	    (if fn
		(format "Find function (default %s): " fn)
	      "Find function: ")
	    obarray 'fboundp t nil nil (symbol-name fn)))
     (list (if (equal val "")
	       fn
	     val))))
  (Info-goto-node "(elisp)Index")
  (condition-case err
      (progn
	(search-forward (concat "* "function":"))
	(Info-follow-nearest-node))
    (error (message "`%s' not found" function))))


(provide 'emacs-help)

