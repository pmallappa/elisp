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
;; set up the environment
(defconst HOME_DIR
  (if (eq system-type 'darwin)
      (concat "/Users/" (getenv "USER"))
    (concat "c:/cygwin/home/" (getenv "USERNAME")))
  "Home directory. I could rely on the HOME environment variable,
  but I'm being retentive.")

(defconst EMACS_CONFIGS (concat HOME_DIR "/elisp")
  "Directory for the emacs pkgs and configuration files.
 Default uses `HOME_DIR' as a prefix")

(defconst MY_TRYOUT_DIR (concat HOME_DIR "/Downloads/tryout")
  "Directory for extracting files")

(defconst MY_HTTP_PROXY (getenv "http_proxy")
  "http proxy, taken from http_proxy env variable")

(defconst MY_PROXY_USER (getenv "user")
  "http proxy username, taken from user env variable")

(defvar MY_DEFAULT_WIDTH 130)

(defconst MY_FG_COLOR "#a9b7b0")
(defconst MY_BG_COLOR "gray17")
(defconst MY_HL_LINE_COLOR "gray24")
(defconst MY_REGION_COLOR "DarkSlateGray")

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
        "points to the Mozilla Firefox location")
      (defconst BROWSER FIREFOX_PRG
        "set the default browser to use within emacs")))
; Windows
(if (eq system-type 'windows-nt)
  (progn
    (defconst MEDIA_PLAYER "c:/Program Files/Windows Media Player/wmplayer.exe"
        "Media Player program, video and streaming audio")
    (defconst IEPRG "c:/Program Files/Internet Explorer/iexplore.exe"
        "points to the Mozilla Firefox location")
    (defconst FIREFOX_PRG "c:/Program Files/Mozilla Firefox/firefox.exe"
        "points to the Mozilla Firefox location")
    (defconst BROWSER IEPRG
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

;; Fix the UI
(set-scroll-bar-mode nil)
(put 'narrow-to-region 'disabled nil)
(global-hl-line-mode)

(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (modify-frame-parameters frame
              '((vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil)))))

;;==============================
;; Enable linumbers in the left margin
;; use M-x linum-mode to toggle
(load-library "linum")

(defalias 'toggle-line-numbers
  (read-kbd-macro "M-x linum-mode"))

;;==============================
;; size, colors and fonts
;; abcedfghijklmnopqrstuvwxyz
;; ABCEDFGHIJKLMNOPQRSTUVWXYZ
;; 0123456789
(setq default-frame-alist
      `((menu-bar-lines . 0)
	(tool-bar-lines . 0)
        (font . "Bitstream Vera Sans Mono-13")))

; copy to the initial frame alist
(setq initial-frame-alist default-frame-alist)

; now modify the initial frame sizes
(add-to-list 'initial-frame-alist '(top    .   8))
(add-to-list 'initial-frame-alist '(left   .  -5))
(add-to-list 'initial-frame-alist '(width  . 132))
(add-to-list 'initial-frame-alist '(height .  70))
(add-to-list 'initial-frame-alist '(alpha 100 90)) ; alpha focus non-focus

;; enable buffer-face mode to provide buffer-local fonts
;; sets the font to the value of buffer-face-mode-face
(set-face-font 'variable-pitch "Verdana-13")
(set-face-font 'fixed-pitch "Bitstream Vera Sans Mono-13")
(buffer-face-mode)


;; good for experimenting with faces
;(set-face-font 'fixed-pitch "Bitstream Vera Sans Mono-13")
;(set-face-font 'variable-pitch "Verdana-13")

;; These require fixed-pitch fonts to format correctly
(add-hook 'text-mode-hook 'fixed-pitch-mode)
(add-hook 'dired-mode-hook 'fixed-pitch-mode)
(add-hook 'calendar-mode-hook 'fixed-pitch-mode)
(add-hook 'org-agenda-mode-hook 'fixed-pitch-mode)
(add-hook 'shell-mode-hook 'fixed-pitch-mode)
(add-hook 'eshell-mode-hook 'fixed-pitch-mode)

; increase the space between lines
(setq-default line-spacing 0)

; Syntax highlighting
(global-font-lock-mode t)

; Lets us see col # at the bottom. very handy.
(column-number-mode 1)

; buffer-name completion for C-x b; makes life much easier.
(iswitchb-mode 1)

; set default colors (theme may override)
(set-face-foreground 'default MY_FG_COLOR)
(set-face-background 'default MY_BG_COLOR)
(set-face-background 'fringe  MY_BG_COLOR)
(set-face-background 'region  MY_REGION_COLOR)
(set-face-background 'hl-line  MY_HL_LINE_COLOR)

(set-face-attribute  'mode-line
                     nil
                     :foreground "gray90"
                     :background "gray30"
                     :box '(:line-width 1 :style released-button))

(set-face-attribute  'mode-line-inactive
                     nil
                     :foreground "gray50"
                     :background MY_BG_COLOR
                     :box '(:line-width 1 :style nil))

;; I don't like bold/underline fonts
(set-face-bold-p 'bold nil)
(set-face-underline-p 'underline nil)
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil))
  (face-list))


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
(require 'emacs-clojure)      ; clojure development using cider
(require 'emacs-eshell)       ; customized eshell settings
(require 'emacs-frame)        ; customized frame functions
(require 'emacs-git)          ; emacs git integration
(require 'emacs-misc)         ; various settings
(require 'emacs-w3m)          ; w3m web browser settings
(require 'emacs-webjump)      ; webjump settings
(require 'emacs-calendar)     ; calendar settings
(require 'emacs-dired)        ; dired settings
(require 'emacs-csv)          ; comma-separated-value editing package
(require 'emacs-eshell)       ; emacs eshell settings

;; jump to a function definition
(global-set-key (kbd "C-h C-f") 'find-function)

;;=====================================================================
;; start the emacsserver that listens to emacsclient
(server-start)

;;======================================================================
;; Use emacs built-in color theme capabilities
;(load-theme 'noctilux)
;(load-theme 'heroku t)
;(load-theme 'zenburn t)
(load-theme 'nzenburn t)

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
