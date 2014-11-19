;; -*-mode: emacs-lisp -*-
;; This is my .emacs file. There are many like it but this one is
;; mine. My .emacs file is my best friend. It is my life. I must
;; master it as I must master my life. Without me, my .emacs file is
;; useless. Without my .emacs file I am useless. I must code my
;; .emacs file true. I must code faster than my enemy, who is trying
;; to kill(1) me. I must kill(1) him before he kill(1)s me. I
;; will. Before God I swear this creed: my .emacs file and myself
;; are defenders of my country, we are the masters of my enemy, we
;; are the saviours of my life. So be it, until there is no enemy,
;; but peace.
;; 
;; Amen.

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
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; set up the home directory
(if (eq system-type 'darwin)
    (defconst HOME_DIR  (concat "/Users/" (getenv "USER"))))
(if (eq system-type 'windows-nt)
    (defconst HOME_DIR  (concat "c:/cygwin/home/" (getenv "USERNAME"))))
(if (eq system-type 'cygwin)
    (defconst HOME_DIR (concat "/home/" (getenv "USERNAME"))))

(setenv "HOME" HOME_DIR)

;; set the customization file
(setq custom-file (concat HOME_DIR "/elisp/emacs-custom.el"))

;; Load the package library prior to loading configs
(load-file (concat HOME_DIR "/elisp/emacs-package.el"))

;; load the actual configuration file
(load-file (concat HOME_DIR "/elisp/emacs.el"))

;; go to the home directory
(cd HOME_DIR)

;;=====================================================================
;; start the emacsserver that listens to emacsclient
(when (display-graphic-p)
  (if (not (server-running-p))
      (server-start)))

