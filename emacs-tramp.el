;; to get tramp-2.2.6 (comes with emacs-24.3) to work correctly, recompile
;; the file emacs/lisp/net/tramp-sh.el

;; To get the pling method working, add the location of plink.exe (part of
;; the putty package), to your windows path, then execute the pageant
;; application (the putty authentication agent)

;; This all assumes you have set up the public and private keys on each
;; system.

(require 'tramp)

(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))
(if (eq system-type 'darwin)
    (setq tramp-default-method "ssh"))

(defun tramp-set-auto-save ()
  (auto-save-mode -1))

(provide 'emacs-tramp)
