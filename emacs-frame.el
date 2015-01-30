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
(require 'hydra)
(key-chord-mode 1)

(defun hydra-universal-argument (arg)
  (interactive "P")
  (setq prefix-arg (if (consp arg)
                       (list (* 4 (car arg)))
                     (if (eq arg '-)
                         (list -4)
                       '(4)))))

 (defhydra hydra-window (global-map "C-M-o")
  "window"
  ("h" windmove-left "left")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("l" windmove-right "right")
  ("a" ace-window "ace")
  ("u" hydra-universal-argument "universal")
  ("s" (lambda () (interactive) (ace-window 4)) "swap")
  ("d" (lambda () (interactive) (ace-window 16)) "delete")
  ("o"))

(key-chord-define-global "yy" 'hydra-window/body)

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

