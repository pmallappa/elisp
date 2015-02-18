;;======================================================================
;; Setting for Hydra multiple key command package
(require 'hydra)
(require 'hydra-examples)
(hydra-add-font-lock)

;; ==============================
;; Creating and moving around windows
(global-set-key
 (kbd "C-c m")
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
   ("q" nil "quit" :color blue)))

;; ==============================
;; Setting and adjusting the frame
(global-set-key
 (kbd "C-c f")
 (defhydra hydra-frame (:color red)
  "Frame"
  ("e" cmframe-toggle-frame-enlarge "Enlarge")
   ("f" toggle-frame-fullscreen "Fullscreen")
   ("l" cmframe-left "Left Monitor")
   ("m" toggle-frame-maximized "Maximized")
   ("r" cmframe-right "Right Monitor")
   ("s" cmframe-frame-shrink "Shrink")
   ("t" cmframe-toggle-window-split "Toggle Split")
   ("q" nil "quit" :color blue)))


;; ==============================
;; Goto line
(global-set-key
 (kbd "M-g")
 (defhydra hydra-goto-line (global-map "M-g"
                           :pre (linum-mode 1)
                           :post (linum-mode -1)
                           :color red)
  ("g" goto-line "line")
  ("c" goto-char "char")))


(provide 'emacs-hydra)
