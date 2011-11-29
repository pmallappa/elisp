;;; while emacs21.1 can handle colors, only load it if in windows
;;; mode. If you are in TTY and you wand colors, you can load it
;;; manually

;;;======================================================================
;;; load the color theme package
;;;======================================================================
(add-to-list 'load-path
              (concat EMACS_PKGS "/color-theme"))
(add-to-list 'load-path
              (concat EMACS_PKGS "/color-theme/zenburn-theme"))


;;;; color theme...use zenburn ;;;;
(require 'color-theme)

;; load the custom color themes. Each theme adds itself to the color-themes list
;; you only need to load the file for the initial theme you want to
;; use on starting up emacs. Any other theme or library located in the
;; color-theme/themes directory will automatically be loaded
(require 'color-theme-zenburn)
(color-theme-zenburn)

;;======================================================================
; now use this color theme at startup
;======================================================================
; ensure that color theme is run at the end of the init, so it
; overrides any applications that may load after this file and set
; face defaults
;(add-hook 'after-init-hook
;          (lambda ()
;            (if window-system
;;                (color-theme-zenburn)
;;                (color-theme-darkgray)
;	      nil)))

;;;_.======================================================================
;;;_. Change all face weights to 'normal
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal))
  (face-list))

(provide 'emacs-colors)
