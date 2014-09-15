;;;_. -*-mode: emacs-lisp -*-
;; set to t for debugging
(setq debug-on-error nil)

;; =====================================================================
;; .emacs --- Emacs configuration file

;; set up the environment (moved to .emacs)
(if (eq system-type 'darwin)
    (defconst HOME_DIR  (concat "/Users/" (getenv "USER"))))
(if (eq system-type 'windows-nt)
    (defconst HOME_DIR  (concat "c:/cygwin/home/" (getenv "USERNAME"))))
(if (eq system-type 'cygwin)
    (defconst HOME_DIR (concat "/home/" (getenv "USERNAME"))))

;; Set the command key to act as the meta key for OS-X
(if (eq system-type 'darwin)
    (progn
      (setq ns-use-srgb-colorspace t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
      (setq ns-function-modifier 'hyper)))

;; basic setup
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default-font "Bitstream Vera Sans Mono-13")

;; set the customization file
(setq custom-file "~/elisp/emacs-custom.el")

;; Load the package library prior to loading configs
(load-file "~/elisp/emacs-package.el")

;; load the actual configuration file
(load-file "~/elisp/emacs.el")

;; go to the home directory
(cd "~/")
