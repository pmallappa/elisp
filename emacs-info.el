;;; emacs-info.el --- 

(require 'info)

;;;_.======================================================================
;;;_. Info mode settings
;; point the packages to the appropriate info directory
;(setq Info-default-directory-list 
;      (list
;       (concat EMACS_PKGS "/info")
;       "/Applications/Emacs.app/Contents/Resources/info"
;       (concat EMACS_PKGS "/info")
;       (concat EMACS_PKGS "/info/elisp")
;       (concat EMACS_PKGS "/info/gnus")
;       (concat EMACS_DIR  "/info")
;       (concat CYGWIN_DIR "/usr/share/info")
;       (concat CYGWIN_DIR "/usr/info")
;       ))

(add-to-list  'Info-default-directory-list (concat EMACS_PKGS "/info"))
(cond ((eq system-type 'windows-nt)
      (add-to-list  'Info-default-directory-list "c:/cygwin/usr/share/info")))
(cond ((eq system-type 'darwin)
      (add-to-list  'Info-default-directory-list "/usr/share/info")))

(setq Info-directory-list Info-default-directory-list)

(provide 'emacs-info)
