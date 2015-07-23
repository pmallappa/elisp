;;======================================================================
;; Setting for Emacs-Eclim, the Emacs/Eclipse integration daemon

(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

(setq eclimd-default-workspace "c:/workspace/")
(setq eclim-executable "c:/eclipse/plugins/org.eclim_2.4.1/bin/eclim")
(setq eclimd-executable nil)

;; start the eclimd daemon with m-x start-eclimd

;;; company mode for pop-up auto-completion
;(require 'company)
;(require 'company-emacs-eclim)
;(company-emacs-eclim-setup)
;(global-company-mode t)
;(setq company-emacs-eclim-ignore-case t)

