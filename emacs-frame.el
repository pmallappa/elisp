;;;_.======================================================================
;;;_. set the frame variables and support functions
(require 'cm-frame)

; adjust the top position of the frame
(setq cmframe-top-margin 25)

; is there a secondary monitor to the right of the primary?
(setq cmframe-monitor2-p 'nil)

; if so, set its dimensions here
;(setq cmframe-monitor2-width 1280)
;(setq cmframe-monitor2-height 1024)

; set the right margin (add window manager space here as well)
(setq cmframe-right-margin 15)


(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 85))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 (transparent) to 100 (opaque): ")
  (set-frame-parameter (selected-frame) 'alpha value))

;; toggle fullscreen mode 
(defun toggle-fullscreen (&optional f)
  (interactive)
  (toggle-frame-fullscreen))

;; modify the keybinding to switch frames
(global-set-key "\M-o" 'other-window)

;; adjust the frame to fit the current resolution on launching
(add-hook 'window-setup-hook 'frame-adjust t)


;;============================================================
;; https://github.com/tungd/dotfiles/blob/master/emacs/init.el#L278
;(setq default-frame-alist
;      '((left-fringe . 24) (right-fringe . 0))
;      initial-frame-alist default-frame-alist)
;
;(setq td-screen-layouts
;      '((:query (= (x-display-pixel-width) 1920)
;                :height 35 :width 100 :top 0 :left 600)
;        (:query (and (= (x-display-pixel-width) 1280) (= (x-display-screens) 2))
;                :height 48 :width 100 :top 10 :left (+ (x-display-pixel-width) 200))
;        (:query (= (x-display-pixel-width) (+ 1366 1280))
;                :height 35 :width 100 :top 0 :left (- 1366 760))))
;
;(defun set-frame-size-and-position-according-to-display ()
;  (interactive)
;  (when (display-graphic-p)
;    (mapc (lambda (layout)
;            (when (eval (plist-get layout :query))
;              (and (plist-get layout :width)
;                   (set-frame-width (selected-frame) (eval (plist-get layout :width))))
;              (and (plist-get layout :height)
;                   (set-frame-height (selected-frame) (eval (plist-get layout :height))))
;              (set-frame-position (selected-frame)
;                                  (eval (plist-get layout :left))
;                                  (eval (plist-get layout :top)))))
;          td-screen-layouts)))
;
;(set-frame-size-and-position-according-to-display)
;
;(defalias 'aa #'set-frame-size-and-position-according-to-display
;  "Auto Adjust frame size according to current display")


(provide 'emacs-frame)

