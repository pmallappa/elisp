;; -*-mode: emacs-lisp -*-
;;=====================================================================
;; .emacs --- Emacs configuration file

;; Author: Chris McMahan <cmcmahan@gmail.com>
;; Time-stamp: 2011-03-17 Thu 11:15
;; Emacsen Compatibility: Emacs23
;; OS Compatibility:      Win32 (with Cygwin utils) / OS X

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
;; set debugging flag
(setq debug-on-error nil)

;;=====================================================================
;; set the location of various directories
;(defconst CYGWIN_DIR
;  (if (eq system-type 'darwin)
;      ""
;    "c:/cygwin/home"))

(defconst HOME_DIR 
  (if (eq system-type 'darwin)
      (concat "/Users/" (getenv "USER"))
    (concat "c:/cygwin/home/" (getenv "USERNAME")))
  "Home directory. I could rely on the HOME environment variable,
  but I'm being retentive.")

(defconst EMACS_PKGS (concat HOME_DIR "/emacs-pkgs")
  "Directory for the emacs pkgs and configuration files.
 Default uses `HOME_DIR' as a prefix")

(defconst MY_TRYOUT_DIR (concat HOME_DIR "/downloads/tryout")
  "Directory for extracting files")

(defconst MY_HTTP_PROXY (getenv "http_proxy")
  "http proxy, taken from http_proxy env variable")

(defconst MY_PROXY_USER (getenv "user")
  "http proxy username, taken from user env variable")

(defvar MY_DEFAULT_WIDTH 132)

;;=====================================================================
;; select your preferred programs for html and media here
; Mac OS X
(if (eq system-type 'darwin)
    (progn
      (defconst MPPRG "/Applications/VLC.app/"
        "Media Player program, video and streaming audio")
      (defconst FRFXPRG "/Applications/Firefox.app/"
        "points to the Mozilla Firefox location")
      (defconst CHRMPRG "/Applications/Google\\ Chrome.app"
        "points to the Mozilla Firefox location")
      (defconst BRWSR CHRMPRG
        "set the default browser to use within emacs")))
; Windows
(if (eq system-type 'windows-nt)
  (progn
    (defconst MPPRG "c:/Program Files/Windows Media Player/wmplayer.exe"
      "Media Player program, video and streaming audio")
    (defconst IEPRG "c:/Program Files/Internet Explorer/iexplore.exe"
      "points to the Internet Explorer location")
   (defconst FRFXPRG "c:/Program Files/Mozilla Firefox/firefox.exe"
      "points to the Mozilla Firefox location")
    (defconst BRWSR IEPRG
      "set the default browser to use within emacs")))

; set the location of firefox for the browse-url-package
(setq browse-url-firefox-program FRFXPRG)

;;=====================================================================
;; various startup settings

;; Toggle whether or not the selected frame should auto-raise.
(auto-raise-mode 1)

;; Set the command key to act as the meta key for OS-X
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

;; isolate customize settings
(setq custom-file (concat EMACS_PKGS "/emacs-custom.el"))

(defvar my_email_address "cmcmahan@gmail.com"
  "email address to use in emacs configuration")

;; turn off the scrollbar
(set-scroll-bar-mode nil)

;; allow narrow-to-region commands
(put 'narrow-to-region 'disabled nil)

;; highlight the current line
(global-hl-line-mode)

;;==============================
;; size, colors and fonts
(setq default-frame-alist
      `((menu-bar-lines . 0)
	(tool-bar-lines . 0)))

(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(font . "Bitstream Vera Sans Mono-12"))
  (add-to-list 'default-frame-alist '(font . "Consolas-10")))

; copy to the default frame alist
(setq initial-frame-alist default-frame-alist)

; now modify the initial frame sizes
(add-to-list 'initial-frame-alist '(top    .   5))
(add-to-list 'initial-frame-alist '(left   .  -5))
(add-to-list 'initial-frame-alist '(width  . 132))
(add-to-list 'initial-frame-alist '(height .  80))

;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; in emacs-frame.el, you can use C-c t to toggle transparency
(set-frame-parameter (selected-frame) 'alpha '(95 50))
(add-to-list 'default-frame-alist '(alpha 95 50))

; Syntax highlighting
(global-font-lock-mode t)

; Lets us see col # at the bottom. very handy.
(column-number-mode 1)

; buffer-name completion for C-x b; makes life much easier.
(iswitchb-mode 1)

; Removes gui elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))  ; no gui scrollbare
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))      ; no toolbar!
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))      ; no menubar

; set colors
(defconst MY_BG_COLOR "gray15")
(set-face-foreground 'default "NavajoWhite3")
(set-face-background 'default MY_BG_COLOR)

(set-face-background 'fringe  MY_BG_COLOR)
(set-face-background 'region  "gray35")
(set-face-background 'hl-line "gray12")

(set-face-foreground 'mode-line "gray75")
(set-face-background 'mode-line "gray25")
(set-face-attribute  'mode-line nil :box "gray60")

(set-face-attribute  'mode-line
                     nil :background "gray30" :box '(:line-width 1 :style released-button))
(set-face-attribute  'mode-line-inactive nil
                     :foreground "gray45" :background MY_BG_COLOR :box '(:line-width 1))

;;=====================================================================
;; load path for various single-file packages
(setq load-path 
      (append 
       (list (concat EMACS_PKGS "")
             (concat EMACS_PKGS "/misc"))
       load-path))

;; Specify where backup files are stored
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))

;;=====================================================================
;; Load the customize configurations files
(require 'emacs-bbdb)      ; bbdb address book
(require 'emacs-bm)        ; bookmark enhancements
(require 'emacs-bs)        ; buffer switch setting
(require 'emacs-calendar)  ; calendar settings
(require 'emacs-dired)     ; dired settings
(require 'emacs-epa)       ; emacs gpg encryption settings
(require 'emacs-frame)     ; customized frame functions
(require 'emacs-git)       ; emacs gpg encryption settings
(require 'emacs-info)      ; add info directories to emacs
(require 'emacs-macros)    ; various macros and functions
(require 'emacs-misc)      ; various settings
(require 'emacs-org)       ; emacs org mode settings
(require 'emacs-undo)      ; Tree-based undo visualizetions
(require 'emacs-w3m)       ; w3m web browser settings
(require 'emacs-webjump)   ; webjump settings
;(require 'emacs-yasnippet) ; yasnippet settings

(cond ((eq system-type 'darwin)
      (require 'emacs-todochiku))) ; notification using growl
(cond ((eq system-type 'windows-nt)
      (require 'emacs-cygwin)))    ; emacs/cygwin integration

;;=====================================================================
;; start the emacsserver that listens to emacsclient
(server-start)

;;=====================================================================
;; some reference stuff
;; Flash a message in the echo area. This works well for debugging an
;; .emacs file by placing various messages throughout
;(message "Hello, this is .emacs speaking")
;(sit-for 3) ; 3 seconds

;;======================================================================
;; Local variables

;Local Variables:
;indent-tabs-mode: nil
;allout-layout: (-1 : 0)
;End:

(provide 'emacs)