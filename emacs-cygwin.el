;;;======================================================================
(require 'cygwin-mount)
(cygwin-mount-activate)

;; This assumes that Cygwin is installed in C:\cygwin (the
;; default) and that C:\cygwin\bin is not already in your
;; Windows Path (it generally should not be).

(setq exec-path (cons "C:/cygwin/bin" exec-path))
(setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))

;;   LOGNAME and USER are expected in many Emacs packages
;;   Check these environment variables.

(if (and (null (getenv "USER"))
	 ;; Windows includes variable USERNAME, which is copied to
	 ;; LOGNAME and USER respectively.
	 (getenv "USERNAME"))
    (setenv "USER" (getenv "USERNAME")))

(if (and (getenv "LOGNAME")
	 ;;  Bash shell defines only LOGNAME
	 (null (getenv "USER")))
    (setenv "USER" (getenv "LOGNAME")))

(if (and (getenv "USER")
	 (null (getenv "LOGNAME")))
    (setenv "LOGNAME" (getenv "USER")))

;;============================================================
;; (A) M-x shell: This change M-x shell permanently
;; Would call Windows command interpreter. Change it.

(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

;; Remove C-m (^M) characters that appear in output
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)


(provide 'emacs-cygwin)
