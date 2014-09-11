;; ======================================================================
;; Frame sizing and movement

;;; set the frame variables and support functions
(require 'cm-frame)

;; adjust the top position of the frame
(if (eq system-type 'darwin)
    (setq cmframe-top-margin 27)
  (setq cmframe-top-margin 10))


;; is there a secondary monitor to the right of the primary?
(setq cmframe-monitor2-p 'nil)

;; if so, set its dimensions here
;(setq cmframe-monitor2-width 1280)
;(setq cmframe-monitor2-height 1024)

; set the right margin (add window manager space here as well)
(setq cmframe-right-margin 15)

(global-set-key (kbd "C-c f a") 'frame-adjust)
(global-set-key (kbd "C-c f s") 'frame-shrink)
(global-set-key (kbd "C-c f e") 'toggle-frame-enlarge)
(global-set-key (kbd "C-c f f") 'toggle-frame-maximize)
(global-set-key (kbd "C-c f |") 'toggle-window-split)

;;; ======================================================================
;; misc settings
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/= (cadr (frame-parameter nil 'alpha)) 100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 90)))) ;focused / background
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 (transparent) to 100 (opaque): ")
  (set-frame-parameter (selected-frame) 'alpha value))

;; modify the keybinding to switch frames
(global-set-key "\M-o" 'other-window)

;; adjust the frame to fit the current resolution on launching
;(add-hook 'after-make-frame-functions 'my-screen-right)
;(add-hook 'window-setup-hook 'my-screen-right)
(run-with-idle-timer 0.1 nil 'frame-adjust)

(provide 'emacs-frame)

