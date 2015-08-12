;;======================================================================
;; Setting for Hydra multiple key command package
(require 'hydra)
(require 'hydra-examples)
(hydra-add-font-lock)

;;==============================
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
(global-set-key
 (kbd "C-c f")
 (defhydra hydra-frame (:exit t)
   "
_e_ Enlarge     _m_ Maximize       _f_ Fullscreen   
_l_ Frame Left  _r_ Frame Right
_s_ Shrink      _t_ Toggle Split
_v_ Split V     _h_ Split H

"
   ("f" toggle-frame-fullscreen nil)
   ("s" cmframe-frame-shrink nil)
   ("e" cmframe-frame-enlarge nil)
   ("m" cmframe-frame-maximize nil)
   ("l" cmframe-left nil)
   ("r" cmframe-right nil)
   ("t" cmframe-toggle-window-split nil)
   ("v" split-window-right nil)
   ("h" split-window-below nil)
   ("q" nil "quit")))

;; ==============================
;; Goto line
(global-set-key
 (kbd "M-g")
 (defhydra hydra-goto-line (goto-map "")
   "goto-line"
   ("g" goto-line "go" :exit t)
   ("l" linum-mode "linum" :exit t)
   ("m" set-mark-command "mark" :bind nil)
   ("q" nil "quit")))

;; ==============================
;; Insert date
(global-set-key
 (kbd "C-c C-d")
 (defhydra hydra-insert-date (:exit t)
   "Insert Date"
   ("t" (insert-date-time) "date/time")
   ("d" (insert-date) "date")
   ("O" (insert-date "[%Y-%m-%d %a %k:%M]") "org active stamp")
   ("o" (insert-date "<%Y-%m-%d %a %k:%M>") "org inactive stamp")
   ("q" nil "quit")))

;;==============================
;; load convience URLs
(global-set-key
 (kbd "C-c C-b")
 (defhydra hydra-browse (:exit t)
   "Browse the following pages"
   ("a" (ie "https://helpdesk.industrysoftware.automation.siemens.com/CAisd/pdmweb.exe") "AHD")
   ("j" (ie "https://jira.industrysoftware.automation.siemens.com/secure/Dashboard.jspa") "JIRA")
   ("f" (crm "http://www.feedly.com") "Feedly")
   ("g" (crm "https://mail.google.com") "Gmail")))

;; ==============================
;; Apropos (in the hydra examples)
(global-set-key (kbd "C-c h") 'hydra-apropos/body)

;; ==============================
;; Zoom in and out
(defun cm_text-scale-reset ()
  (interactive)
  (text-scale-adjust 0))

(global-set-key
 (kbd "C-c C-z")
 (defhydra hydra-zoom ()
   "zoom"
   ("i" text-scale-increase "in")
   ("o" text-scale-decrease "out")
   ("r" cm_text-scale-reset "reset")
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

(defhydra hydra-helm (:hint nil :color pink)
        "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
        ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
        ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
        ("<tab>" helm-keyboard-quit "back" :exit t)
        ("<escape>" nil "quit")
        ("\\" (insert "\\") "\\" :color blue)
        ("h" helm-beginning-of-buffer)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-end-of-buffer)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("n" helm-next-source)
        ("p" helm-previous-source)
        ("K" helm-scroll-other-window-down)
        ("J" helm-scroll-other-window)
        ("c" helm-recenter-top-bottom-other-window)
        ("m" helm-toggle-visible-mark)
        ("t" helm-toggle-all-marks)
        ("u" helm-unmark-all)
        ("H" helm-help)
        ("s" helm-buffer-help)
        ("v" helm-execute-persistent-action)
        ("d" helm-persistent-delete-marked)
        ("y" helm-yank-selection)
        ("w" helm-toggle-resplit-and-swap-windows)
        ("f" helm-follow-mode))

(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

(provide 'emacs-hydra)

