;;;_.============================================================
;;;_. Org mode personal organizer
(add-to-list 'load-path
            (expand-file-name (concat EMACS_PKGS "/org-mode/lisp")))

; this one is necessary for the org-mediawiki option
;(add-to-list 'load-path
;            (expand-file-name (concat EMACS_PKGS "/org-mode/EXPERIMENTAL")))

;; from Sacha Chua's blog
;;http://sachachua.com/wp/2007/12/29/how-to-use-emacs-org-as-a-basic-day-planner/

;;;_.============================================================
;;;_. Set the return key to activate a link
;; Needs to be set before org.el is loaded.
(setq org-return-follows-link t)

;;;_.============================================================
;;;_. load org
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;;_.============================================================
;;;_. Set up some faces
(make-face 'org-hide)
(set-face-foreground 'org-hide "#343434")
(setq org-hide-leading-stars t)

;;;_.============================================================
;;;_. capture templates (replaces remember)

(setq org-default-notes-file "~/org/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry
         (file+olp "~/org/journal.org" "Tasks")
         "* TODO %^{Brief Description} %^g\n  Added: %U\n  %?\n  %a")
        ("a" "Appointment" entry
         (file+olp "~/org/journal.org" "Appointments")
         "* Appt %^{Brief Description} %^T %^g\n  %i%?\n  %a")
        ("j" "Journal" entry
         (file+olp "~/org/journal.org" "Journal")
         "* %^{Brief Description} %T %^g\n  %i%?\n  %a")
        ("n" "Note" entry
         (file+olp "~/org/journal.org" "Notes")
         "* %^{Brief Description} %T %^g\n  %i%?\n  %a")
        ("b" "Bill" entry
         (file+olp "~/org/journal.org" "Bills")
         "* Paid %^{Bill Paid|AT&T|USAA Auto|USAA Master Card} %T\n   Amount: $%^{Amount $}\n   Source: %^{Source Acct|NFCU chkg|Fifth-Third chkg}\n  Confirm: %^{Confirmation #}\n    Notes: %^{Notes}\n")
        ("f" "Funds" entry
         (file+olp "~/org/journal.org" "Funds")
         "* Transferred Money %T\n     From: %^{Transferred From:|NFCU Svgs|NFCU Chk|Fifth-Third Chk}\n       To: %^{To:|NFCU Chkg|NFCU Svgs|Fifth-Third Chk}\n   Amount: $%^{Amount $}\n  Confirm: %^{Confirmation #}\n")
        ("p" "Password" entry
         (file+olp "~/org/passwords.gpg" "Passwords")
         "* %^{Title}\n  :PROPERTIES:\n  :Update:   %u\n  :Url:      %^{Url}p\n  :Username: %^{Username}p\n  :Password: %^{Password}p\n  :Notes:    %^{Notes}p\n  :END:\n")))

;;;_.============================================================
;;;_. various settings
(setq org-agenda-include-diary t
      org-agenda-remove-tags t
      org-agenda-include-all-todo t
      org-agenda-tags-column -100
      org-tags-column -100
      org-use-fast-todo-selection t
      org-confirm-elisp-link-function `y-or-n-p
      org-attach-directory "/Users/cmcmahan/org/data/")
 
;;;_.============================================================
;;;_. Set up org files
(setq org-agenda-files 
      (list 
       (concat HOME_DIR "/org/fun.org")
       (concat HOME_DIR "/org/journal.org")
       (concat HOME_DIR "/org/reference.org")
       (concat HOME_DIR "/org/Xetron.org")))

;;;_.============================================================
;;;_. Set up the keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-special-ctrl-a/e t)

;;;_.============================================================
;;;_. Set up the global tags list
(setq org-tag-alist 
      '(("Appt"       . ?a)
        ("Emacs"      . ?e)
        ("KMA"        . ?k)
        ("Navy"       . ?n)
        ("NACRA"      . ?N)
        ("Personal"   . ?p)
        ("Reference"  . ?r)
        ("Xetron"     . ?x)))

;;;_.============================================================
;;;_. Create custom agenda views
;; good resource is 
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html


(setq org-agenda-block-separator 
      "===================================================================")
(setq org-agenda-current-time-string
      "now - - - - - - - - - - - - - -")

(setq org-agenda-custom-commands
      '(("d" "Agenda and all tasks"
         ((agenda)
          (todo "OPEN")
          (todo "NEXT")))
        ("n" "Agenda and Navy-related tasks"
         ((agenda)
          (tags-todo "Navy")))
        ("x" "Agenda and Xetron-related tasks"
         ((agenda)
          (tags-todo "xetron")))
        ("o" "Agenda and all open tasks"
         ((agenda)
          (todo "OPEN")))
        ("c" "Calendar Agenda"
         ((agenda)
          (todo))
         ((org-agenda-ndays 7)
          (org-agenda-remove-tags t)
          (org-agenda-start-on-weekday 1) ; start on Monday
          (org-agenda-entry-types '(:timestamp :sexp))
          (org-agenda-prefix-format " %-12:t ")
          (org-deadline-warning-days 0)
          (org-agenda-include-all-todo nil)
          (org-agenda-repeating-timestamp-show-all t)
          (org-agenda-filter-preset '("-nocal1"))
          (org-agenda-hide-tags-regexp ".*"))
         ("~/.org-agenda.txt"))
        ))

;; To use the GeekTool agenda from a command line, put the following
;; in the shell command field. Note, emacs server must be running for
;; this to function correctly. I could have called emacs itself, but I
;; usually have it open anyway.
;;
;; /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -e "(progn (org-store-agenda-views)) ; cat ~/.org-agenda.txt"

(provide 'emacs-org)

;;;_.======================================================================
;;;_. Local variables
;;Local Variables:
;;indent-tabs-mode: nil
;;allout-layout: (-1 : 0)
;;End:
