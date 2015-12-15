;;======================================================================
;; Set the colors
;; the default font is set in .emacs, the fixed-font should match this
;;======================================================================
;; Set the fonts
;; you can get the 'Source Code Pro' font at
;; https://github.com/adobe-fonts/source-code-pro
;; a copy has also been place in my dropbox account
;;
;; abcedfghijklmnopqrstuvwxyz
;; ABCEDFGHIJKLMNOPQRSTUVWXYZ
;; 0123456789

(if (eq system-type 'darwin)
    (progn
      (set-face-font 'default "Consolas-14")
      (set-face-font 'variable-pitch "Verdana-13")))
(if (or (eq system-type 'cygwin) (eq system-type 'windows-nt))
    (progn
      (set-face-font 'default "Source Code Pro Medium-9")
      (set-face-font 'variable-pitch "MS Reference Sans Serif-8")))

(copy-face 'default 'fixed-pitch)

;; (set-face-font 'default "NK57 Monospace Sc Rg-9")
;; (set-face-font 'default "Consolas-10")
;; (set-face-font 'default "Source Code Pro Semibold-9")
;; (set-face-font 'default "Source Code Pro Medium-9")
;; (set-face-font 'default "Courier New-9")
;; (set-face-font 'default "Lucida Console-9")
;; (set-face-font 'default "Lucida Sans Typewriter-8")
;; (set-face-font 'default "InputMono-8")
;; (set-face-font 'default "Hack-9")
;; (set-face-font 'default "Menlo-13")

;; (set-face-font 'variable-pitch "Calibri-10")
;; (set-face-font 'variable-pitch "Verdana-12")
;; (set-face-font 'variable-pitch "Arial-13")
;; (set-face-font 'variable-pitch "Segoe UI Semibold-12")
;; (set-face-font 'variable-pitch "Segoe UI Semibold-9")
;; (set-face-font 'variable-pitch "MS Reference Sans Serif-8")
;; (set-face-font 'variable-pitch "InputSans-12")
;; (set-face-font 'variable-pitch "Source Sans Pro-10")

;; Fix the UI
(setq visible-bell nil) 
(setq ring-bell-function 'ignore)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(set-scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;;======================================================================
;; Eye candy

(load-library "linum")
(defun toggle-line-numbers()
  "Easy to remember shortcut to M-x linum-mode"
  (interactive)
  (if linum-mode
      (linum-mode -1)
    (linum-mode 1)))

; Lets us see col # at the bottom. very handy.
(column-number-mode 1)

;;======================================================================
;; Syntax highlighting

(global-font-lock-mode t)
(global-hl-line-mode 1)
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

;; turn on column and line numbers
;(setq line-number-mode t)
;(setq column-number-mode t)

;; time and date
(setq display-time-format " %a %m/%d %H:%M ")     ;;Fri 08/19 15:26
(setq display-time-day-and-date t)
(display-time)

;; format for the filename in the modeline
;; The default, "%12b", just displays the filename.
;; You can find the complete path by invoking the macro 'M-x path',
;; defined in .emacs-macros.el
(setq-default mode-line-buffer-identification '("%12b"))

;; minimize extraneous info
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'smartparens-mode)
(diminish 'auto-complete-mode)

;; Titlebar text
(setq frame-title-format
      (concat invocation-name " on "
              system-name
	      " -- %f"))

;;=====================================================================
;; frame transparency
(defvar alpha-focused 93
  "When transparency is enabled, the value between
0 (transparent) and 100 (opaque) to use when emacs has the
focus")
(defvar alpha-background 85
  "When transparency is enabled, the value between
0 (transparent) and 100 (opaque) to use when emacs does not have
the focus")

(setq default-alpha-focused alpha-focused
      default-alpha-background alpha-background)

(defun toggle-transparency ()
  "Toggle between transparent and opaque background. 
Uses the values for `alpha-focused' and `alpha-background'"
  (interactive)
  (if (/= (cadr (frame-parameter nil 'alpha)) 100)
        (transparent 100 100)
    (transparent alpha-focused alpha-background)))

(defun reset-transparency()
  (interactive)
  (transparent default-alpha-focused default-alpha-background))
    
;; Set transparency of emacs
(defun transparent(alpha-f alpha-b)
 "Set the focus and background transparency"
 (interactive "nFocus level (0-100): \nnBackground level (0-100): ")
 (set-frame-parameter (selected-frame) 'alpha (list alpha-f alpha-b))
 (add-to-list 'default-frame-alist `(alpha ,alpha-f)))

;;======================================================================
;; Split horizontally vice vertically when buffer exceeds this width
(setq split-width-threshold 160)

;;======================================================================
;; Color Themes

;;------------------------------
;; A better Solarized theme with some adjustments
;; here's the canonical colors as defined in the solarized source site
;; http://ethanschoonover.com/solarized
;; See org/reference.org for an org-table version
;;
;;
;; | COLOR   | HEX     |         RGB |         HSB |   XTERM/HEX |
;; |---------+---------+-------------+-------------+-------------|
;; | base03  | #002b36 |   0  43  54 | 193 100  21 | 234 #1c1c1c |
;; | base02  | #073642 |   7  54  66 | 192  90  26 | 235 #262626 |
;; | base01  | #586e75 |  88 110 117 | 194  25  46 | 240 #585858 |
;; | base00  | #657b83 | 101 123 131 | 195  23  51 | 241 #626262 |
;; | base0   | #839496 | 131 148 150 | 186  13  59 | 244 #808080 |
;; | base1   | #93a1a1 | 147 161 161 | 180   9  63 | 245 #8a8a8a |
;; | base2   | #eee8d5 | 238 232 213 |  44  11  93 | 254 #e4e4e4 |
;; | base3   | #fdf6e3 | 253 246 227 |  44  10  99 | 230 #ffffd7 |
;; | yellow  | #b58900 | 181 137   0 |  45 100  71 | 136 #af8700 |
;; | orange  | #cb4b16 | 203  75  22 |  18  89  80 | 166 #d75f00 |
;; | red     | #dc322f | 220  50  47 |   1  79  86 | 160 #d70000 |
;; | magenta | #d33682 | 211  54 130 | 331  74  83 | 125 #af005f |
;; | violet  | #6c71c4 | 108 113 196 | 237  45  77 |  61 #5f5faf |
;; | blue    | #268bd2 |  38 139 210 | 205  82  82 |  33 #0087ff |
;; | cyan    | #2aa198 |  42 161 152 | 175  74  63 |  37 #00afaf |
;; | green   | #859900 | 133 153   0 |  68 100  60 |  64 #5f8700 |

;;; Solarized light theme
(load-theme 'sanityinc-solarized-light t nil)
;(set-face-background 'default "cornsilk2")

(set-face-background 'mode-line (cm-adjust-color (face-background 'default) -12))
(set-face-foreground 'mode-line (cm-adjust-color (face-foreground 'default) -8))
(set-face-background 'mode-line-inactive (cm-adjust-color (face-background 'default) -2))
(set-face-foreground 'mode-line-inactive (cm-adjust-color (face-foreground 'default) +16))
(set-face-background 'hl-line (cm-adjust-color (face-background 'default) -8))
(set-face-background 'helm-selection "#268bd2")
(set-face-foreground 'helm-selection (face-background 'default))

;; the solarized annotate is unviewable, so revert to the default
(eval-after-load "vc-annotate"
  '(setq vc-annotate-color-map 
      '(
        (20 . "#FFCCCC")
        (40 . "#FFD8CC")
        (60 . "#FFE4CC")
        (80 . "#FFF0CC")
        (100 . "#FFFCCC")
        (120 . "#F6FFCC")
        (140 . "#EAFFCC")
        (160 . "#DEFFCC")
        (180 . "#D2FFCC")
        (200 . "#CCFFD2")
        (220 . "#CCFFDE")
        (240 . "#CCFFEA")
        (260 . "#CCFFF6")
        (280 . "#CCFCFF")
        (300 . "#CCF0FF")
        (320 . "#CCE4FF")
        (340 . "#CCD8FF")
        (360 . "#CCCCFF"))))

;;------------------------------
;; Anti-zenburn theme
;; low-contrast gray theme
;(load-theme 'anti-zenburn t)

;;------------------------------
;; Zenburn theme
;(load-theme 'zenburn t)
;(set-face-background 'hl-line (cm-adjust-color (face-background 'default) -5))
;(set-face-background 'region (cm-adjust-color (face-background 'default) +10))
;(set-face-foreground 'vertical-border (cm-adjust-color (face-background 'default) -10))

;;------------------------------
;; standard light colors with gray background
;;(set-face-background 'default "#50717C")
;;(set-face-foreground 'mode-line (cm-adjust-color (face-background 'default) +32))
;;(set-face-background 'mode-line (cm-adjust-color (face-background 'default) -12))
;;(set-face-foreground 'mode-line-inactive (cm-adjust-color (face-background 'default) +24))
;;(set-face-background 'mode-line-inactive (cm-adjust-color (face-background 'default) -4))
;;(set-face-foreground 'font-lock-comment-face (cm-adjust-color (face-foreground 'font-lock-comment-face) +10))
;;(set-face-foreground 'font-lock-comment-delimiter-face (face-foreground 'font-lock-comment-face))
;;(set-face-background 'default (cm-adjust-color (face-background 'default) -4))

;;------------------------------
;; regardless of the theme, match the fringe the default background
(set-face-background 'fringe (face-attribute 'default :background))
;; and hide the org-mode leading stars
(make-face 'org-hide)
(set-face-foreground 'org-hide (face-attribute 'default :background))
(set-face-background 'org-hide (face-attribute 'default :background))
(setq org-hide-leading-stars t)


(provide 'emacs-ui)
