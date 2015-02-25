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
Move to window  Move split
    _p_            _k_      _v_ Split Vertically
 _b_  +_f_      _h_  +_l_   _x_ Split Horizontally
    _n_            _j_      _d_ Delete other windows

"   
   ("b" windmove-left nil)
   ("f" windmove-right nil)
   ("p" windmove-up nil)
   ("n" windmove-down nil)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)) nil)
   ("d" delete-other-windows "Delete" :color blue)
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)) nil)
   ("h" hydra-move-splitter-left nil)
   ("j" hydra-move-splitter-down nil)
   ("k" hydra-move-splitter-up nil)
   ("l" hydra-move-splitter-right nil)
   ("t" cmframe-toggle-window-split nil)
   ("q" nil "Quit" :color blue)))

;; ==============================
;; Setting and adjusting the frame
;; TODO Fix frame maximized and frame shrink to toggle fullframe off if
;; necessary

(global-set-key
 (kbd "C-c f")
 (defhydra hydra-frame (:exit t)
   "
_s_ Shrink Frame  _e_ Enlarge
_m_ Maximize      _f_ Fullscreen
_l_ Frame left    _r_ Frame right
_t_ Toggle Split

"
   ("e" cmframe-toggle-frame-enlarge nil)
   ("f" toggle-frame-fullscreen nil)
   ("s" cmframe-frame-shrink nil)
   ("m" cmframe-frame-maximize nil)
   ("l" cmframe-left nil)
   ("r" cmframe-right nil)
   ("t" cmframe-toggle-window-split nil)
   ("q" nil "quit")))

;; ==============================
;; Goto line
(global-set-key
 (kbd "M-g")
 (defhydra hydra-goto-line (goto-map ""
                            :pre (linum-mode 1)
                            :post (linum-mode -1))
   "goto-line"
   ("g" goto-line "go")
   ("m" set-mark-command "mark" :bind nil)
   ("q" nil "quit")))


(global-set-key
 (kbd "C-c r")
 (defhydra hydra-minframe (:exit t)
   "
Core Frame Function
--------------------
_m_ Maximize _f_ Fullscreen
_v_ Vertical _h_ Horizontal

"
   ("m" toggle-frame-maximized nil)
   ("f" toggle-frame-fullscreen nil)
   ("v" split-window-right nil)
   ("h" split-window-below nil)))

(provide 'emacs-hydra)
