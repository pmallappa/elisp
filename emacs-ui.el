;;======================================================================
;; Set the colors
;; the default font is set in .emacs, the fixed-font should match this
(global-hl-line-mode)

;;======================================================================
;; Set the fonts
(if (eq system-type 'darwin)
    (progn
      (set-face-font 'default "Bitstream Vera Sans Mono-14")
      (set-face-font 'variable-pitch "Verdana-14")
      (copy-face 'default 'fixed-pitch)))
(if (or (eq system-type 'cygwin) (eq system-type 'windows-nt))
    (progn
;      (set-face-font 'default "Lucida Sans Typewriter-10")
;      (set-face-font 'variable-pitch "Lucida Sans-10")
      (set-face-font 'default "Consolas-9")
      (set-face-font 'variable-pitch "Lucida Sans-8")
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

; Syntax highlighting
(global-font-lock-mode t)

; Lets us see col # at the bottom. very handy.
(column-number-mode 1)

;;======================================================================
;; Theme

; solarized light theme
(load-theme 'solarized-light t nil)
(set-face-background 'default "#f8f4e9")
(set-face-background 'fringe (face-attribute 'default :background))
(set-face-background 'mode-line-inactive (face-attribute 'mode-line :background))
(set-face-foreground 'mode-line "gray30")
(set-face-background 'mode-line "#e0dcd1")
(set-face-background 'hl-line "#f0ece1")
(set-face-foreground 'ediff-fine-diff-A "gray30")
(set-face-foreground 'ediff-fine-diff-B "gray30")

;; zenburn theme
;; tweak the theme. See the variable `zenburn-colors-alist' for color pallet
;(load-theme 'zenburn t nil)
;(set-face-foreground 'default "#d0d0c0")
;(set-face-background 'region  "#656555")
;(set-face-foreground 'isearch "#ffffef")
;(set-face-background 'isearch "#d0bf8f")
;(set-face-background 'lazy-highlight "#656555")

;; set the fringe background to match the default background color
(set-face-background 'fringe (face-attribute 'default :background))

(provide 'emacs-ui)
