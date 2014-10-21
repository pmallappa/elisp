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

;; blatantly stolen from sqlplus-shine-color
(defun cm-adjust-color (color percent)
  "Return the hex numeric value of the color provided adjusted by
the percent specified, An example of usage to adjust the color of
a face would be:

(set-face-foreground 'mode-line (cm-adjust-color (face-foreground 'default) -20))"

  (when (equal color "unspecified-bg")
    (setq color (if (< percent 0) "white" "black")))
  (apply 'format "#%02x%02x%02x" 
         (mapcar (lambda (value)
                   (min 65535 (max 0 (* (+ (/ value 650) percent) 650))))
                 (color-values color))))

(global-font-lock-mode t)
(global-hl-line-mode -1)
(setq global-hl-line-sticky-flag nil)

;; highlight the entire s-expression under point
(hl-sexp-mode 1)

;; highlight symbol shows the current symbol across the entire buffer
(highlight-symbol-mode 1)


;;======================================================================
;; auto dim background buffers
;(add-hook 'after-init-hook (lambda ()
;  (when (fboundp 'auto-dim-other-buffers-mode)
;    (auto-dim-other-buffers-mode t))))
;
;(set-face-background 'auto-dim-other-buffers-face
;                     (cm-adjust-color (face-background 'default) -1.5))


;;======================================================================
;; Color Themes
;; Zenburn theme with some tweaks
;(load-theme 'hc-zenburn t nil)
;(set-face-background 'default (cm-adjust-color (face-background 'default) +10))
;(set-face-background 'region (cm-adjust-color (face-background 'default) +15))
;(set-face-background 'lazy-highlight (cm-adjust-color (face-background 'isearch-fail) -5))

;; A better Solarized theme with some adjustments
(load-theme 'sanityinc-solarized-light t nil)
(set-face-foreground 'mode-line (cm-adjust-color (face-foreground 'default) -20))
;(set-face-background 'mode-line (cm-adjust-color (face-background 'default) -5))
(set-face-background 'mode-line-inactive (cm-adjust-color (face-background 'default) -1))

;;; set the fringe background to match the default background color
;(set-face-background 'default (face-attribute 'fringe :background))
(set-face-background 'fringe (face-attribute 'default :background))

(provide 'emacs-ui)
