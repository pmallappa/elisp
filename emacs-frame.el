;; ======================================================================
;; Frame sizing and movement

(require 'frame-cmds)

;; ======================================================================
;; set the keys for the frame functions
;; from the library frame-cmds.el
(global-set-key (kbd "C-c f e") 'maximize-frame)
(global-set-key (kbd "C-c f l") 'my-screen-left)
(global-set-key (kbd "C-c f r") 'my-screen-right)

(defun my-screen-right ()
  (interactive)
  (set-frame-parameter nil 'width MY_DEFAULT_WIDTH)
  (maximize-frame-vertically)
  (move-frame-to-screen-right 0))

(defun my-screen-left ()
  (interactive)
  (set-frame-parameter nil 'width MY_DEFAULT_WIDTH)
  (maximize-frame-vertically)
  (move-frame-to-screen-left 0))

(defun frame-setup ()
  (interactive)
  (my-screen-right)
  (maximize-frame-vertically)
  (set-frame-alist-parameter-from-frame 'initial-frame-alist 'height)
  (set-frame-alist-parameter-from-frame 'default-frame-alist 'height))

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

;; toggle fullscreen mode 
(defun toggle-fullscreen (&optional f)
  (interactive)
  (toggle-frame-fullscreen))

;; modify the keybinding to switch frames
(global-set-key "\M-o" 'other-window)

;; adjust the frame to fit the current resolution on launching
;(add-hook 'after-make-frame-functions 'my-screen-max-vertical)
;(add-hook 'window-setup-hook 'my-screen-right)

(provide 'emacs-frame)

