;;;_. -*-mode: emacs-lisp -*-
;; set to t for debugging
(setq debug-on-error nil)

;; =====================================================================
;; .emacs --- Emacs configuration file

;; Set the command key to act as the meta key for OS-X
(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
      (setq ns-function-modifier 'hyper)))

;; basic setup
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; set up the home directory
(if (eq system-type 'darwin)
    (defconst HOME_DIR  (concat "/Users/" (getenv "USER"))))
(if (eq system-type 'windows-nt)
    (defconst HOME_DIR  (concat "c:/cygwin/home/" (getenv "USERNAME"))))
(if (eq system-type 'cygwin)
    (defconst HOME_DIR (concat "/home/" (getenv "USERNAME"))))

;; set the customization file
(setq custom-file (concat HOME_DIR "/elisp/emacs-custom.el"))

;; Load the package library prior to loading configs
(load-file (concat HOME_DIR "/elisp/emacs-package.el"))

;; load the actual configuration file
(load-file (concat HOME_DIR "/elisp/emacs.el"))

;; go to the home directory
(cd HOME_DIR)
