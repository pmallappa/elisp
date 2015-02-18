;;======================================================================
;; Setting for Hydra multiple key command package
(require 'hydra)
(require 'hydra-examples)

;; Moving around windows
(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window ()
   "Window"
   ("b" windmove-left "Left")
   ("f" windmove-right "Right")
   ("p" windmove-up "Up")
   ("n" windmove-down "Down")
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)) "Vert")
   ("d" delete-other-windows "Delete other windows")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)) "horz")
   ("h" hydra-move-splitter-left "Adj Left")
   ("j" hydra-move-splitter-down "Down")
   ("k" hydra-move-splitter-up "Up")
   ("l" hydra-move-splitter-right "Right")
   ("q" nil "cancel")))

;; Setting and adjusting frames
(global-set-key
 (kbd "C-c f")
 (defhydra hydra-frame ()
   "Frame"
   ("e" cmframe-toggle-frame-enlarge "Enlarge")
   ("f" toggle-frame-fullscreen "Fullscreen")
   ("l" cmframe-left "Left Monitor")
   ("m" toggle-frame-maximized "Maximized")
   ("r" cmframe-right "Right Monitor")
   ("s" cmframe-frame-shrink "Shrink")
   ("t" cmframe-toggle-window-split "Toggle Split")
   ("q" nil "cancel")))

(provide 'emacs-hydra)
