;;======================================================================
;; Set the colors
;; the default font is set in .emacs, the fixed-font should match this
;;======================================================================
;; Set the fonts
(if (eq system-type 'darwin)
    (progn
      (set-face-font 'default "Bitstream Vera Sans Mono-12")
      (set-face-font 'variable-pitch "Verdana-12")
      (copy-face 'default 'fixed-pitch)))

(if (or (eq system-type 'cygwin) (eq system-type 'windows-nt))
    (progn
      (set-face-font 'default "Consolas-10")
      (set-face-font 'variable-pitch "Lucida Sans Unicode-8")
      (copy-face 'default 'fixed-pitch)))

;; Fix the UI
(setq visible-bell nil)
(setq ring-bell-visible nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(set-scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; enable buffer-face mode to provide buffer-local fonts
(buffer-face-mode)

;; These require fixed-pitch fonts to format correctly
(add-hook 'text-mode-hook 'fixed-pitch-mode)
(add-hook 'dired-mode-hook 'fixed-pitch-mode)
(add-hook 'calendar-mode-hook 'fixed-pitch-mode)
(add-hook 'org-agenda-mode-hook 'fixed-pitch-mode)
(add-hook 'shell-mode-hook 'fixed-pitch-mode)
(add-hook 'eshell-mode-hook 'fixed-pitch-mode)
(add-hook 'bs-mode-hook 'fixed-pitch-mode)

;;======================================================================
;; Eye candy

(load-library "linum")

(defalias 'toggle-line-numbers
  (read-kbd-macro "M-x linum-mode"))

;;; I don't like bold/underline fonts
;(set-face-bold-p 'bold nil)
;;;(set-face-underline-p 'underline nil)
;(mapc
;  (lambda (face)
;    (set-face-attribute face nil :weight 'normal :underline nil))
;  (face-list))

; increase the space between lines
(setq-default line-spacing 0)

; Lets us see col # at the bottom. very handy.
(column-number-mode 1)

;;======================================================================
;; Syntax highlighting

(global-font-lock-mode t)
(global-hl-line-mode -1)
(setq global-hl-line-sticky-flag nil)

;; highlight the entire s-expression under point
(hl-sexp-mode 1)

;; highlight symbol shows the current symbol across the entire buffer
(highlight-symbol-mode 1)


;;======================================================================
;; mode-line modifications

;; set the modeline to display path and filename
(defun long-file-name ()
  "Display the full file path and name in the modeline"
  (interactive "*")
  (setq-default mode-line-buffer-identification
    '("%S:"(buffer-file-name "%f"))))

;; set the modeline to display filename only
(defun short-file-name ()
  "Display the file name without path in the modeline"
  (interactive "*")
  (setq-default mode-line-buffer-identification '("%12b")))

;; turn on column line mode
(setq column-number-mode nil)

;; time and date
(setq display-time-format " %a %m/%d %H:%M ")     ;;Fri 08/19 15:26
(setq display-time-day-and-date t)
(display-time)

;; format for the filename in the modeline
;; The default, "%12b", just displays the filename.
;; You can find the complete path by invoking the macro 'M-x path',
;; defined in .emacs-macros.el
(setq-default mode-line-buffer-identification '("%12b"))

;; format for the title on the titlebar
(setq frame-title-format
      (concat invocation-name " on "
              system-name
	      " -- %f"))

;; minimize extraneous info
;(require 'diminish)
;(diminish 'abbrev-mode)
;(diminish 'elisp-slime-nav-mode)
;(diminish 'magit-auto-revert-mode)
;(diminish 'smartparens-mode)
;(diminish 'auto-complete-mode)

;; display the function the point is in within the modeline if any
(setq which-func-unknown "")
(which-function-mode)

;;;======================================================================
;;; auto-dim-other-buffers
;;; changes the default background color when buffer is not in focus
;(add-hook
; 'after-init-hook
; (lambda ()
;   (when (fboundp 'auto-dim-other-buffers-mode)
;     (progn (auto-dim-other-buffers-mode t)
;            (set-face-background
;             'auto-dim-other-buffers-face
;             (cm-adjust-color (face-background 'default) -2))))))

;;======================================================================
;; Color Themes
;; Zenburn theme with some tweaks
;(load-theme 'hc-zenburn t nil)
;(set-face-background 'default (cm-adjust-color (face-background 'default) +10))
;(set-face-background 'region (cm-adjust-color (face-background 'default) +15))
;(set-face-background 'isearch (cm-adjust-color (face-background 'default) -20))
;(set-face-background 'lazy-highlight (cm-adjust-color (face-background 'isearch-fail) -5))

;; A better Solarized theme with some adjustments
(load-theme 'sanityinc-solarized-light t nil)
;(set-face-background 'default "#e0e1dc")
(set-face-foreground 'mode-line (cm-adjust-color (face-foreground 'default) -15))
(set-face-background 'mode-line (cm-adjust-color (face-background 'default) -10))
(set-face-background 'mode-line-inactive (cm-adjust-color (face-background 'default) -2))

;; regardless of the theme, match the fringe the default background
(set-face-background 'fringe (face-attribute 'default :background))

;;(load-theme 'base16-chalk t nil)
;;(load-theme 'base16-default t nil)
;;(load-theme 'base16-eighties t nil)
;;(load-theme 'base16-greenscreen t nil)
;;(load-theme 'base16-mocha t nil)
;;(load-theme 'base16-ocean t nil)
;;(load-theme 'base16-tomorrow t nil)


(provide 'emacs-ui)
