;;======================================================================
;; Setting for Hydra multiple key command package
(require 'hydra)
(require 'hydra-examples)

;; Moving around windows
(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window ()
   "window"
   ("b" windmove-left "L")
   ("f" windmove-right "R")
   ("p" windmove-up "U")
   ("n" windmove-down "D")
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)) "vert")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)) "horz")
   ("h" hydra-move-splitter-left)
   ("j" hydra-move-splitter-down)
   ("k" hydra-move-splitter-up)
   ("l" hydra-move-splitter-right)
   ("d" delete-other-windows "1" :color blue)
   ("q" nil "cancel")))

(provide 'emacs-hydra)
