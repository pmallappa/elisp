;;======================================================================
;; Set the colors
;; the default font is set in .emacs, the fixed-font should match this
;;======================================================================
;; Set the fonts
;; you can get the 'Source Code Pro' font at
;; https://github.com/adobe-fonts/source-code-pro
;; a copy has also been place in my dropbox account
(if (eq system-type 'darwin)
    (progn
      (set-face-font 'default "Consolas-14")
      (set-face-font 'variable-pitch "Verdana-12")
      (copy-face 'default 'fixed-pitch)))
(if (or (eq system-type 'cygwin) (eq system-type 'windows-nt))
    (progn
      (set-face-font 'variable-pitch "Segoe UI Semibold-9")
      (set-face-font 'default "Consolas-10")
      (copy-face 'default 'fixed-pitch)))

;; Consolas, courier font has the best unicode support of the ones listed
;;      (set-face-font 'default "NK57 Monospace Sc Rg-9")
;;      (set-face-font 'default "Consolas-10")
;;      (set-face-font 'default "Source Code Pro Semibold-9")
;;      (set-face-font 'default "Source Code Pro Medium-9")
;;      (set-face-font 'default "Courier New-9")
;;      (set-face-font 'default "Lucida Console-9")
;;      (set-face-font 'default "Lucida Sans Typewriter-9")
;;      (set-face-font 'default "Lucida Console-9")
;;      (set-face-font 'variable-pitch "Calibri-10")
;;      (set-face-font 'variable-pitch "Verdana-9")
;;      (set-face-font 'variable-pitch "Segoe UI Semibold-10")
;;      (set-face-font 'variable-pitch "Segoe UI Semibold-9")
;;      (set-face-font 'variable-pitch "MS Reference Sans Serif-9")

;; Fix the UI
(setq visible-bell nil)
(setq ring-bell-visible nil)
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

; adjust the space between lines
;(setq-default line-spacing 0)

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

;; Titlebar text
(setq frame-title-format
      (concat invocation-name " on "
              system-name
	      " -- %f"))

;; minimize extraneous info
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'elisp-slime-nav-mode)
(diminish 'magit-auto-revert-mode)
(diminish 'smartparens-mode)
(diminish 'auto-complete-mode)

;; display the function the point is in within the modeline if any
(setq which-func-unknown "")
(which-function-mode)


;;======================================================================
;; Color Themes
;;(load-theme 'oldlace t nil)

;;------------------------------
;; A better Solarized theme with some adjustments
;; here's the canonical colors as defined in the solarized source site
;; http://ethanschoonover.com/solarized
;; | SOLARIZED | HEX     | 16/8 | TERMCOL   | XTERM/HEX   | RGB         | HSB         |
;; |-----------+---------+------+-----------+-------------+-------------+-------------|
;; | base03    | #002b36 | 8/4  | brblack   | 234 #1c1c1c | 0  43  54   | 193 100  21 |
;; | base02    | #073642 | 0/4  | black     | 235 #262626 | 7  54  66   | 192  90  26 |
;; | base01    | #586e75 | 10/7 | brgreen   | 240 #585858 | 88 110 117  | 194  25  46 |
;; | base00    | #657b83 | 11/7 | bryellow  | 241 #626262 | 101 123 131 | 195  23  51 |
;; | base0     | #839496 | 12/6 | brblue    | 244 #808080 | 131 148 150 | 186  13  59 |
;; | base1     | #93a1a1 | 14/4 | brcyan    | 245 #8a8a8a | 147 161 161 | 180   9  63 |
;; | base2     | #eee8d5 | 7/7  | white     | 254 #e4e4e4 | 238 232 213 | 44  11  93  |
;; | base3     | #fdf6e3 | 15/7 | brwhite   | 230 #ffffd7 | 253 246 227 | 44  10  99  |
;; | yellow    | #b58900 | 3/3  | yellow    | 136 #af8700 | 181 137   0 | 45 100  71  |
;; | orange    | #cb4b16 | 9/3  | brred     | 166 #d75f00 | 203  75  22 | 18  89  80  |
;; | red       | #dc322f | 1/1  | red       | 160 #d70000 | 220  50  47 | 1  79  86   |
;; | magenta   | #d33682 | 5/5  | magenta   | 125 #af005f | 211  54 130 | 331  74  83 |
;; | violet    | #6c71c4 | 13/5 | brmagenta | 61 #5f5faf  | 108 113 196 | 237  45  77 |
;; | blue      | #268bd2 | 4/4  | blue      | 33 #0087ff  | 38 139 210  | 205  82  82 |
;; | cyan      | #2aa198 | 6/6  | cyan      | 37 #00afaf  | 42 161 152  | 175  74  63 |
;; | green     | #859900 | 2/2  | green     | 64 #5f8700  | 133 153   0 | 68 100  60  |

(load-theme 'sanityinc-solarized-light t nil)
(set-face-background 'mode-line (cm-adjust-color (face-background 'default) -8))
(set-face-background 'mode-line-inactive (face-background 'default))

;;------------------------------
;; an excellent flat gray theme
;(load-theme 'anti-zenburn t nil)
;(set-face-foreground 'default "#535363")

;;------------------------------
;; standard light colors with gray background
;(set-face-background 'default "ivory2")
;(set-face-foreground 'mode-line (cm-adjust-color (face-foreground 'default) -15))
;(set-face-background 'mode-line (cm-adjust-color (face-background 'default) -10))
;(set-face-background 'mode-line-inactive (cm-adjust-color (face-background 'default) -5))
;(set-face-foreground 'mode-line-inactive (cm-adjust-color (face-foreground 'default) +10))

;;------------------------------
;; regardless of the theme, match the fringe the default background
(set-face-background 'fringe (face-attribute 'default :background))

(provide 'emacs-ui)
