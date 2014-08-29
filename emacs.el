;; -*-mode: emacs-lisp -*-
;;=====================================================================
;; .emacs --- Emacs configuration file

;; Author: Chris McMahan <cmcmahan@gmail.com>
;; Time-stamp: 2014-05-24 Sat 21:35
;; Emacsen Compatibility: Emacs23
;; OS Compatibility: Win32 (with Cygwin utils) / OS X

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of version 2 of the GNU General Public License
;; as published by the Free Software Foundation.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; I'm sure you already have many copies of the GPL on your machine.
;; If you're using GNU Emacs, try typing C-h C-c to bring it up. If
;; you're using XEmacs, C-h C-l does this.

;;=====================================================================
;;; set up the environment (moved to .emacs)
;(if (eq system-type 'darwin)
;    (defconst HOME_DIR  (concat "/Users/" (getenv "USER"))))
;(if (eq system-type 'windows-nt)
;    (defconst HOME_DIR  (concat "c:/cygwin/home/" (getenv "USERNAME"))))
;(if (eq system-type 'cygwin)
;    (defconst HOME_DIR (concat "/home/" (getenv "USERNAME"))))
;;; set up the home directory
;(setenv "HOME" HOME_DIR)

(defconst EMACS_CONFIGS (concat HOME_DIR "/elisp")
  "Directory for the emacs pkgs and configuration files.
 Default uses `HOME_DIR' as a prefix")

(defconst MY_TRYOUT_DIR (concat HOME_DIR "/downloads/tryout")
  "Directory for extracting files")

(defconst MY_HTTP_PROXY (getenv "http_proxy")
  "http proxy, taken from http_proxy env variable")

(defconst MY_PROXY_USER (getenv "user")
  "http proxy username, taken from user env variable")

(defvar MY_DEFAULT_WIDTH 130)

;;=====================================================================
;; select your preferred programs for html and media here
; Mac OS X
(if (eq system-type 'darwin)
    (progn
      (defconst MEDIA_PLAYER "/Applications/VLC.app/"
        "Media Player program, video and streaming audio")
      (defconst FIREFOX_PRG "/Applications/Firefox.app"
        "points to the Mozilla Firefox location")
      (defconst CHROME_PRG "/Applications/Google\\ Chrome.app"
        "points to the Google Chrome browser location")
      (defconst BROWSER FIREFOX_PRG
        "set the default browser to use within emacs")))
; Windows
(if (eq system-type 'windows-nt)
  (progn
    (defconst MEDIA_PLAYER "c:/Program Files/Windows Media Player/wmplayer.exe"
        "Media Player program, video and streaming audio")
    (defconst IEPRG "c:/Program Files/Internet Explorer/iexplore.exe"
        "points to the Mozilla Firefox location")
    (defconst FIREFOX_PRG "c:/Program Files (x86)/Mozilla Firefox/firefox.exe"
        "points to the Mozilla Firefox location")
    (defconst CHROME_PRG "c:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
        "points to the Google Chrome browser location")
    (defconst BROWSER CHROME_PRG
      "set the default browser to use within emacs")))
(if (eq system-type 'cygwin)
  (progn
    (defconst MEDIA_PLAYER "/c/Program Files/Windows Media Player/wmplayer.exe"
        "Media Player program, video and streaming audio")
    (defconst IEPRG "/c/Program Files/Internet Explorer/iexplore.exe"
        "points to the Mozilla Firefox location")
    (defconst FIREFOX_PRG "/c/Program Files (x86)/Mozilla Firefox/firefox.exe"
        "points to the Mozilla Firefox location")
    (defconst CHROME_PRG "/c/Program Files (x86)/Google/Chrome/Application/chrome.exe"
        "points to the Google Chrome browser location")
    (defconst BROWSER CHROME_PRG
      "set the default browser to use within emacs")))

; set the location of firefox for the browse-url-package
(setq browse-url-firefox-program FIREFOX_PRG)
(setq browse-url-firefox-new-window-is-tab 1)

;;=====================================================================
;; various startup settings

;; prevent new frames in emacsclient on darwin
(if (eq system-type 'darwin)
    (setq ns-popup-frames nil))

(defvar my_email_address "cmcmahan@gmail.com"
  "email address to use in emacs configuration")

;;==============================
;; size, colors and fonts
;; abcedfghijklmnopqrstuvwxyz
;; ABCEDFGHIJKLMNOPQRSTUVWXYZ
;; 0123456789
(setq default-frame-alist
      '((menu-bar-lines . 0)
	(tool-bar-lines . 0)
        (height . 60)
        (width . 130)))

(setq initial-frame-alist
      '((menu-bar-lines . 0)
	(tool-bar-lines . 0)
        (height . 60)
        (width . 130)
        (alpha 100 100)))     ; focus background

;;======================================================================
;; Set the fonts
;; the default font is set in .emacs, the fixed-font should match this
(load-theme 'zenburn t)
(if (eq system-type 'darwin)
    (progn
      (set-face-font 'default "Bitstream Vera Sans Mono-13")
      (set-face-font 'variable-pitch "Verdana-13")
      (copy-face 'default 'fixed-pitch)))
(if (or (eq system-type 'cygwin) (eq system-type 'windows-nt))
    (progn
      (set-face-font 'default "Lucida Sans Typewriter-10")
      (set-face-font 'variable-pitch "Lucida Sans-10")
;      (set-face-font 'default "Lucida Sans Typewriter-8")
;      (set-face-font 'variable-pitch "Lucida Sans-8")
      (copy-face 'default 'fixed-pitch)))

;; Fix the UI
(setq visible-bell nil)
(setq ring-bell-visible nil)

(set-scroll-bar-mode nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(global-hl-line-mode)
(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (modify-frame-parameters frame
              '((vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil)))))

;; enable buffer-face mode to provide buffer-local fonts
(buffer-face-mode)

;; These require fixed-pitch fonts to format correctly
(add-hook 'text-mode-hook 'fixed-pitch-mode)
(add-hook 'dired-mode-hook 'fixed-pitch-mode)
(add-hook 'calendar-mode-hook 'fixed-pitch-mode)
(add-hook 'org-agenda-mode-hook 'fixed-pitch-mode)
(add-hook 'shell-mode-hook 'fixed-pitch-mode)
(add-hook 'eshell-mode-hook 'fixed-pitch-mode)

;;======================================================================
;; Eye candy

;;==============================
;; Enable linumbers in the left margin
;; use M-x linum-mode to toggle
(load-library "linum")

(defalias 'toggle-line-numbers
  (read-kbd-macro "M-x linum-mode"))

;; set the fringe background to match the default background color
(set-face-background 'fringe (face-attribute 'default :background))

;; I don't like bold/underline fonts
(set-face-bold-p 'bold nil)
(set-face-underline-p 'underline nil)
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil))
  (face-list))

; increase the space between lines
(setq-default line-spacing 0)

; Syntax highlighting
(global-font-lock-mode t)

; Lets us see col # at the bottom. very handy.
(column-number-mode 1)

; buffer-name completion for C-x b; makes life much easier.
(iswitchb-mode 1)

;;=====================================================================
;; load path for various single-file packages
(setq load-path
      (append
       (list (concat EMACS_CONFIGS "")
             (concat EMACS_CONFIGS "/misc"))
       load-path))

;; Specify where backup files are stored
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))

;;=====================================================================
;; Set the environment (OSX or Cygwin)
(if (eq system-type 'darwin)
  (progn
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

(if (eq system-type 'windows-nt)
      (require 'emacs-cygwin))

;;=====================================================================
;; Load the customize configurations files
(require 'emacs-macros)       ; various macros and functions
(require 'emacs-org)          ; emacs org mode settings
(require 'emacs-bm)           ; bookmark enhancements
(require 'emacs-bs)           ; buffer switch setting
(require 'emacs-browse)       ; external browser convenience functions
(require 'emacs-autocomplete) ; autocompletion goodness
(require 'emacs-eshell)       ; customized eshell settings
(require 'emacs-frame)        ; customized frame functions
(require 'emacs-git)          ; emacs git integration
(require 'emacs-misc)         ; various settings
(require 'emacs-info)         ; setting up info
(require 'emacs-w3m)          ; w3m web browser settings
(require 'emacs-webjump)      ; webjump settings
(require 'emacs-calendar)     ; calendar settings
(require 'emacs-dired)        ; dired settings
(require 'emacs-csv)          ; comma-separated-value editing package
(require 'emacs-eshell)       ; emacs eshell settings
;(require 'emacs-javascript)   ; javascript development and REPL
(require 'emacs-smartparens)  ; better parenthesis highlighting and navigating
(require 'emacs-help)         ; keybindings for help functions

;;=====================================================================
;; start the emacsserver that listens to emacsclient
(server-start)

;;=====================================================================
;; some reference stuff
;; Flash a message in the echo area. This works well for debugging an
;; .emacs file by placing various messages throughout
;(message "Hello, this is .emacs speaking")
;(sit-for 3) ; 3 seconds
;
;; set a break point in elisp by adding this:
;(debug)

;;======================================================================
;; Local variables

;Local Variables:
;indent-tabs-mode: nil
;allout-layout: (-1 : 0)
;End:


(provide 'emacs)
