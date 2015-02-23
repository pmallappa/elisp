;;======================================================================
;; Setting for Hydra multiple key command package
(require 'hydra)
(require 'hydra-examples)
(hydra-add-font-lock)

;; ==============================
;; Creating and moving around windows
(global-set-key
 (kbd "C-c m")
 (defhydra hydra-window (:color red)
   "
Window Methods
--------------------------------------------
_b_ Left              _f_ Right
_p_ Up                _n_ Down
_v_ Split Vertically  _x_ Split Horizontally
_h_ Move split left   _l_ Move split right
_k_ Move split up     _j_ Move split down
_d_ Delete other windows

"   

   ("b" windmove-left nil)
   ("f" windmove-right nil)
   ("p" windmove-up nil)
   ("n" windmove-down nil)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)) nil)
   ("d" delete-other-windows nil)
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)) nil)
   ("h" hydra-move-splitter-left nil)
   ("j" hydra-move-splitter-down nil)
   ("k" hydra-move-splitter-up nil)
   ("l" hydra-move-splitter-right nil)
   ("q" nil "quit" :color blue)))

;; ==============================
;; Setting and adjusting the frame
(global-set-key
 (kbd "C-c f")
 (defhydra hydra-frame (:exit t)
   "
Frame Methods
--------------------------------
_s_ Shrink       _e_ Enlarge
_m_ Maximize     _f_ Fullscreen
_l_ Frame left   _r_ Frame right
_t_ Toggle Split

"
   ("e" cmframe-toggle-frame-enlarge nil)
   ("f" toggle-frame-fullscreen nil)
   ("s" cmframe-frame-shrink nil)
   ("m" toggle-frame-maximized nil)
   ("l" cmframe-left nil)
   ("r" cmframe-right nil)
   ("t" cmframe-toggle-window-split nil)
   ("q" nil "quit")))

;; ==============================
;; Goto line
(global-set-key
 (kbd "M-g")
 (defhydra hydra-goto-line (:pre (linum-mode 1)
                            :post (linum-mode -1)
                            :color blue)
   ("g" goto-line "line")))

(provide 'emacs-hydra)
