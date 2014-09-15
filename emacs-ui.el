;;======================================================================
;; Set the colors
;; the default font is set in .emacs, the fixed-font should match this
(global-hl-line-mode)

;;======================================================================
;; Set the fonts
(if (eq system-type 'darwin)
    (progn
      (set-face-font 'default "Bitstream Vera Sans Mono-12")
      (set-face-font 'variable-pitch "Verdana-12")
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
;; Color Theme
(load-theme 'sanityinc-solarized-light t nil)

(if (eq system-type 'darwin)
    (set-face-background 'default "cornsilk2")
  (set-face-background 'default "ivory2"))
;(set-face-background 'fringe (face-attribute 'default :background))
;(set-face-foreground 'eshell-prompt-face "IndianRed4")
;(set-face-background 'eshell-prompt (face-attribute 'default :background))
;(set-face-background 'hl-line "cornsilk2")
;(set-face-foreground 'ediff-fine-diff-A "gray30")
;(set-face-foreground 'ediff-fine-diff-B "gray30")
;
;(set-face-attribute 'org-agenda-date nil
;                    :box '(:color "ivory2")
;                    :background (face-attribute 'default :background))
;
;(set-face-attribute 'org-agenda-structure nil
;                    :box (face-attribute 'org-agenda-date :box)
;                    :box (face-attribute 'default :background))
;
;(set-face-attribute 'mode-line nil
;                    :box '(:line-width 1 :style released-button)
;                    :background "cornsilk1")
;
;(set-face-attribute 'mode-line-inactive nil
;                    :box '(:line-width 1 :style released-button)
;                    :background "ivory3")
;
;;; set the fringe background to match the default background color
(set-face-background 'fringe (face-attribute 'default :background))

(provide 'emacs-ui)
