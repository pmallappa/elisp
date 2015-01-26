;; ======================================================================
;; Frame sizing and movement

;;; set the frame variables and support functions
(require 'cm-frame)

;; adjust the top position of the frame
(if (eq system-type 'darwin)
    (setq cmframe-top-margin 27)
  (setq cmframe-top-margin 0))

;; is there a secondary monitor to the right of the primary?
(setq cmframe-monitor2-p t)

;; if so, set its dimensions here
(setq cmframe-monitor2-width 1920)
(setq cmframe-monitor2-height 1080)

; set the right margin (add window manager space here as well)
(setq cmframe-horizontal-margin 15)

(global-set-key (kbd "C-c f e") 'cmframe-toggle-frame-enlarge)
(global-set-key (kbd "C-c f f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c f l") 'cmframe-left)
(global-set-key (kbd "C-c f m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c f r") 'cmframe-right)
(global-set-key (kbd "C-c f s") 'cmframe-frame-shrink)
(global-set-key (kbd "C-c f t") 'cmframe-toggle-window-split)

;;======================================================================
;; Moving around windows
(defun sacha/vsplit-last-buffer (prefix)
  "Split the window vertically and keep the cursor in the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))

(defun sacha/hsplit-last-buffer (prefix)
  "Split the window horizontally and keep the cursor in the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))

;(bind-key "C-x 2" 'sacha/vsplit-last-buffer)
;(bind-key "C-x 3" 'sacha/hsplit-last-buffer)

;; Ace Window
;; When prefixed with one `universal-argument', instead of switching
;; to selected window, the selected window is swapped with current one.

;; When prefixed with two `universal-argument', the selected window is
;; deleted instead.
(global-set-key (kbd "M-p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;; ======================================================================
;; misc settings
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/= (cadr (frame-parameter nil 'alpha)) 100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 20)))) ;focused / background
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 (transparent) to 100 (opaque): ")
  (set-frame-parameter (selected-frame) 'alpha value))

;; adjust the frame to fit the current resolution on launching
;(add-hook 'after-make-frame-functions 'my-screen-right)
;(add-hook 'window-setup-hook 'my-screen-right)
(run-with-idle-timer 0.1 nil 'cmframe-frame-adjust)

(provide 'emacs-frame)

