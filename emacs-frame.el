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

;; toggle full screen (for mac version)
(defun toggle-fullscreen ()
  "Toggles emacs fullscreen mode.
For Homebrew emacs (OSX), will use a true fullscreen mode,
otherwise, it will enlarge the frame"
  (interactive)
  (if (eq system-type 'darwin)
      (ns-toggle-fullscreen)
  (frame-enlarge)))


;; adjust the frame to fit the current resolution on launching
(add-hook 'window-setup-hook 'frame-adjust t)



(provide 'emacs-frame)

