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
;; load path for various single-file packages
(setq load-path
      (append
       (list (concat EMACS_CONFIGS "")
             (concat EMACS_CONFIGS "/misc"))
       load-path))

;; Specify where backup files are stored
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))

;;=====================================================================
;; set up a http proxy if necessary
;(setq url-proxy-services '(("no_proxy" . "siemens\\.com")
;                           ("http" . "http://127.0.0.1:3124")))
;(setq url-using-proxy "http://127.0.0.1:3124")
;;(setq url-using-proxy "http://mil-fsprx.net.plm.eds.com:3128")

;;=====================================================================
;; Set the environment (OSX or Cygwin)
(if (eq system-type 'darwin)
  (progn
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

(if (eq system-type 'windows-nt)
    (require 'emacs-cygwin))

;;=====================================================================
;; select your preferred programs for html and media here
;; Mac OS X
(if (eq system-type 'darwin)
    (progn
      (defconst MEDIA_PLAYER "/Applications/VLC.app/"
        "Media Player program, video and streaming audio")
      (defconst FIREFOX_PRG "/Applications/Firefox.app"
        "points to the Mozilla Firefox location")
      (defconst SAF_PRG "/Applications/Safari/Safari.app"
        "points to the Internet Explorer Mozilla Firefox location")
      (defconst CHROME_PRG "/Applications/Google\\ Chrome.app"
        "points to the Google Chrome browser location")
      (defconst BROWSER FIREFOX_PRG
        "set the default browser to use within emacs")))
;; Windows
(if (eq system-type 'windows-nt)
  (progn
    (defconst MEDIA_PLAYER "c:/Program Files/Windows Media Player/wmplayer.exe"
        "Media Player program, video and streaming audio")
    (defconst IE_PRG "c:/Program Files/Internet Explorer/iexplore.exe"
        "points to the Internet Explorer location")
    (defconst FIREFOX_PRG "c:/Program Files (x86)/Mozilla Firefox/firefox.exe"
        "points to the Mozilla Firefox location")
    (defconst CHROME_PRG "c:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
        "points to the Google Chrome browser location")
    (defconst BROWSER CHROME_PRG
      "set the default browser to use within emacs")))
;; cygwin emacs
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

;; load an updated rect.el that provides preview of string-rectangle
(load-file (concat HOME_DIR "/elisp/misc/rect.el"))

;; prevent new frames in emacsclient on darwin
(if (eq system-type 'darwin)
    (setq ns-popup-frames nil))

;;==============================
;; size, colors and fonts
;; abcedfghijklmnopqrstuvwxyz
;; ABCEDFGHIJKLMNOPQRSTUVWXYZ
;; 0123456789
(setq default-frame-alist
      '((menu-bar-lines . 0)
	(tool-bar-lines . 0)
        (width . 130)
        (height . 60)))

(setq initial-frame-alist
      '((menu-bar-lines . 0)
	(tool-bar-lines . 0)
        (width . 130)
        (height. 60)
        (alpha 100 100)))     ; focus background

;;=====================================================================
;; easypg settings
;; prevents symmetric-encrypted files from repeatedly prompting for
;; passwords
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; use the internal password cache
(require 'password-cache)
(setq password-cache t)
(setq password-cache-expiry 360)

;;=====================================================================
;; Load the customize configurations files
(require 'emacs-macros)       ; various macros and functions
(require 'emacs-helm)         ; helm integration
(require 'emacs-org)          ; emacs org mode settings
(require 'emacs-bm)           ; bookmark enhancements
(require 'emacs-bs)           ; buffer switch setting
(require 'emacs-browse)       ; external browser convenience functions
(require 'emacs-autocomplete) ; autocompletion goodness
(require 'emacs-eshell)       ; customized eshell settings
(require 'emacs-shell)        ; customized shell settings
(require 'emacs-frame)        ; customized frame functions
(require 'emacs-git)          ; emacs git integration
(require 'emacs-hydra)        ; multiple commands strung together
(require 'emacs-misc)         ; various settings
(require 'emacs-info)         ; setting up info
(require 'emacs-tramp)        ; remote file access
(require 'emacs-w3m)          ; w3m web browser settings
(require 'emacs-webjump)      ; webjump settings
(require 'emacs-calendar)     ; calendar settings
(require 'emacs-dired)        ; dired settings
(require 'emacs-csv)          ; comma-separated-value editing package
(require 'emacs-java)         ; java development settings
(require 'emacs-web)          ; web/html development settings
(require 'emacs-smartparens)  ; better parenthesis highlighting and navigating
(require 'emacs-help)         ; keybindings for help functions
(require 'emacs-sql)          ; database interaction
(require 'emacs-ui)           ; theme, fonts, modeline and eye candy

;;=====================================================================
;; some reference stuff
;; Flash a message in the echo area. This works well for debugging an
;; .emacs file by placing various messages throughout
;;(message "Hello, this is .emacs speaking")
;;(sit-for 3) ; 3 seconds
;;
;; set a break point in elisp by adding this:
;;(debug)

;;======================================================================
;; Local variables

;Local Variables:
;indent-tabs-mode: nil
;allout-layout: (-1 : 0)
;End:

(provide 'emacs)
