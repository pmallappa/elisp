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
Select window  Move split
    ^_p_^             ^_k_^         _v_ Split Vertically
  _b_ + _f_         _h_ + _l_       _x_ Split Horizontally
    ^_n_^             ^_j_^         _d_ Delete other windows

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
;; more hydra frame stuff
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

;; ==============================
;; Goto line
(global-set-key
 (kbd "M-g")
 (defhydra hydra-goto-line (goto-map "")
   "goto-line"
   ("g" goto-line "go")
   ("l" linum-mode "linum")
   ("m" set-mark-command "mark" :bind nil)
   ("q" nil "quit")))

;; ==============================
;; Insert date
(global-set-key
 (kbd "C-c d")
 (defhydra hydra-insert-date (:exit t)
   "Insert Date"
   ("t" (insert-date-time) "date/time")
   ("d" (insert-date) "date")
   ("o" (insert-date "[%Y-%m-%d %a %k:%M]") "org inactive stamp")
   ("O" (insert-date "<%Y-%m-%d %a %k:%M>") "org active stamp")
   ("q" nil "quit")))

;; ==============================
;; Apropos (in the hydra examples)
(global-set-key (kbd "C-c h") 'hydra-apropos/body)

;; ==============================
;; Zoom in and out
(global-set-key
 (kbd "C-c z")
 (defhydra hydra-zoom ()
   "zoom"
   ("i" text-scale-increase "in")
   ("o" text-scale-decrease "out")
   ("q" nil "quit")))

;;     (defhydra hydra-zoom (global-map "<f2>")
;;       "zoom"
;;       ("g" text-scale-increase "in")
;;       ("l" text-scale-decrease "out"))


(defhydra hydra-global-org (:color blue
                            :hint nil)
  "
Timer^^ ^Clock^ ^Capture^
--------------------------------------------------
s_t_art _w_ clock in _c_apture
 _s_top _o_ clock out _l_ast capture
_r_eset _j_ clock goto
_p_rint
"
  ("t" org-timer-start)
  ("s" org-timer-stop)
  ;; Need to be at timer
  ("r" org-timer-set-timer)
  ;; Print timer value to buffer
  ("p" org-timer)
  ("w" (org-clock-in '(4)))
  ("o" org-clock-out)
  ;; Visit the clocked task from any buffer
  ("j" org-clock-goto)
  ("c" org-capture)
  ("l" org-capture-goto-last-stored))

(defhydra hydra-refile (:hint nil
                        :color teal)
  "
Refile:^^ _k_eep: %`org-refile-keep
----------------------------------
_l_ast _a_rchive
_o_ther
_t_his

"
  ("t" worf-refile-this)
  ("o" worf-refile-other)
  ("l" worf-refile-last)
  ("k" (setq org-refile-keep (not org-refile-keep))
   :exit nil)
  ("a" (org-archive-subtree))
  ("q" nil "quit"))


(provide 'emacs-hydra)

