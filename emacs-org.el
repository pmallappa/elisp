;;============================================================
;; Org mode personal organizer

;; loaded and initialized from elpa packaging
(setq org-export-backends (quote (ascii html md)))

;;============================================================
;; Set up some faces
(make-face 'org-hide)
(set-face-foreground 'org-hide (face-attribute 'default :background))
(setq org-hide-leading-stars t)

;; hide underline and bold fonts
(eval-after-load "org" '(clean-fonts))

;;============================================================
;; Set the return key to activate a link
;; Needs to be set before org.el is loaded.
(setq org-return-follows-link t)

;;============================================================
;; capture templates (replaces remember)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry
         (file+olp "~/org/journal.org" "Tasks")
         "* TODO %^{Task Description} %^g\n  Added:  %U\n  %?\n  %a\n")
        ("a" "Appointment" entry
         (file+olp "~/org/journal.org" "Appointments")
         "* %^{Appt Description}  %^g\n  %T\n  %i%?\n  %a\n")
        ("n" "Note" entry
         (file+olp "~/org/journal.org" "Notes")
         "* %^{Note Description}  %^g\n  %T\n  %i%?\n  %a\n")

        ;; Siemens entries
        ("s" "Siemens")
        ("st" "Todo" entry
         (file+olp "~/org/Siemens.org" "Tasks")
         "* TODO %^{Task Description} %^g\n  Added: %U\n  %?\n  %a\n")
        ("sa" "Appointment" entry
         (file+olp "~/org/Siemens.org" "Appointments")
         "* %^{Appt Description}  %^g\n  %T\n  %i%?\n  %a\n")
        ("sn" "Note" entry
         (file+olp "~/org/Siemens.org" "Notes")
         "* %^{Note Description}  %^g\n  %T\n  %i%?\n  %a\n")

        ("b" "Bill" entry
         (file+olp "~/org/journal.org" "Bills")
         "* Paid %^{Bill Paid|AT&T|Matrix|USAA Auto Ins|USAA Master Card} %T\n   Amount: $%^{Amount $}\n   Source: %^{Source Acct|Fifth-Third|NFCU chkg}\n  Confirm: %^{Confirmation #}\n    Notes: %^{Notes}\n")
        ("f" "Funds" entry
         (file+olp "~/org/journal.org" "Funds")
         "* Transferred Money %T\n     From: %^{Transferred From:|Fifth-Third Chkg|NFCU Chkg|NFCU Svgs}\n       To: %^{To:|NFCU Svgs|NFCU Chkg|Fifth-Third Chk}\n   Amount: $%^{Amount $}\n  Confirm: %^{Confirmation #}\n")
        ("p" "Password" entry
         (file+olp "~/org/passwords.gpg" "Passwords")
         "* %^{Title}\n  :PROPERTIES:\n  :Update:   %u\n  :Url:      %^{Url}\n  :Username: %^{Username}\n  :Password: %^{Password}\n  :Notes:    %^{Notes}\n  :END:")))

;;============================================================
;; various settings
(setq org-agenda-include-diary t
      org-agenda-remove-tags t
      org-agenda-include-all-todo t
      org-agenda-tags-column -100
      org-tags-column -100
      org-use-fast-todo-selection t
      org-confirm-elisp-link-function `y-or-n-p
      org-attach-directory (concat HOME_DIR "/org/data"))
 
;;============================================================
;; Set up org files
(setq org-agenda-files 
      (list 
       (concat HOME_DIR "/org/fun.org")
       (concat HOME_DIR "/org/journal.org")
       (concat HOME_DIR "/org/vtu.org")
       (concat HOME_DIR "/org/Siemens.org")
       (concat HOME_DIR "/org/reference.org")))

;;============================================================
;; Set up the keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-special-ctrl-a/e t)

;;============================================================
;; Set up the global tags list
(setq org-tag-persistent-alist
      '(("Emacs"      . ?e)
        ("Navy"       . ?n)
        ("Personal"   . ?p)
        ("Seimens"    . ?s)
        ("crypt"      . ?c)
        (one-on-one   . ?o)
        ("Reference"  . ?r)))

;;============================================================
;; set up encryption within org files. Use the 'crypt' tag to encrypt the
;; contents of the tag. This uses the emacs easypg library, so that must be
;; installed and working as well
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-
(setq auto-save-default nil)

;; To decrypt, place the cursor on the heading and execute
;; M-x org-decrypt-entry


;;============================================================
;; Create custom agenda views
;; good resource is 
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html


(setq org-agenda-block-separator 
      "===================================================================")

(setq org-agenda-current-time-string
      "now - - - - - - - - - - - - - -")

(setq org-agenda-custom-commands
      '(("d" "Agenda and all tasks"
         ((agenda)
          (todo))
         ((org-agenda-start-on-weekday 1)) ; start on Monday
         ("~/.org-agenda.txt"))
        ))

;; To use the GeekTool agenda from a command line, put the following
;; in the shell command field. Note, emacs server must be running for
;; this to function correctly. I could have called emacs itself, but I
;; usually have it open anyway.
;; 'emacsclient -e "(progn (org-store-agenda-views))" ; cat ~/.org-agenda.txt'

;; Set up the TODO states
(setq org-todo-keyword-faces
      '(("TODO"      . (:foreground "red"          :weight bold))
	("WAITING"   . (:foreground "orange"       :weight bold))
	("DELEGATED" . (:foreground "orange"       :weight bold))
	("OPEN"      . (:foreground "white"        :weight bold))
	("DONE"      . (:foreground "forest green" :weight bold))
	("CANCELLED" . (:foreground "forest green" :weight bold))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "OPEN(o!)" "|" "DONE(d!/!)")
	(sequence "WAITING(w@/!)" "DELEGATED(g@/!)" "|" "CANCELLED(c!/!)")))

;; log and add notes when completing a task
(setq org-log-done 'note)

;; Mark todo done when all subentries are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


;;============================================================
;; ediff hooks for org mode
;(add-hook 'ediff-select-hook 'f-ediff-org-unfold-tree-element)
;(add-hook 'ediff-unselect-hook 'f-ediff-org-fold-tree)
;;; Check for org mode and existence of buffer
;(defun f-ediff-org-showhide(buf command &rest cmdargs)
;  "If buffer exists and is orgmode then execute command"
;  (if buf
;      (if (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
;          (save-excursion (set-buffer buf) (apply command cmdargs)))))
;
;(defun f-ediff-org-unfold-tree-element ()
;  "Unfold tree at diff location"
;  (f-ediff-org-showhide ediff-buffer-A 'org-reveal) 
;  (f-ediff-org-showhide ediff-buffer-B 'org-reveal) 
;  (f-ediff-org-showhide ediff-buffer-C 'org-reveal))
;;;
;(defun f-ediff-org-fold-tree ()
;  "Fold tree back to top level"
;  (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1) 
;  (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1) 
;  (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))

(add-hook 'ediff-prepare-buffer-hook 'f-ediff-prepare-buffer-hook-setup)
(defun f-ediff-prepare-buffer-hook-setup ()
  ;; specific modes
  (cond ((eq major-mode 'org-mode)
         (f-org-vis-mod-maximum))
        ;; room for more modes
        ))
(defun f-org-vis-mod-maximum ()
  "Visibility: Show the most possible."
  (cond
   ((eq major-mode 'org-mode)
    (visible-mode 1))  ; default 0
   (t
    (message "ERR: not in Org mode")
    (ding))))

;;============================================================
;; settings to export and publish

;; enable markdown export
(eval-after-load "org" '(require 'ox-md nil t))
(eval-after-load "org" '(require 'ox-publish))

;; set up the files for publishing
(setq org-publish-project-alist
      '(
        ("org-files"
         :base-directory "~/org" 
         :base-extension "org"
         :publishing-directory "~/public_html"
         :publishing-function org-html-publish-to-html
         :preserve-breaks t)
        ("org-static"
         :base-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("org"
         :components ("org-files" "org-static"))
        ))


;;;============================================================
;;; integrate Mobile Org using Dropbox
;;; After capturing notes or making changes on the device to your Org
;;; files, be sure to sync in MobileOrg. Then run org-mobile-pull from
;;; Emacs to integrate your changes. After integrating, you can run
;;; org-mobile-push to make sure MobileOrg has access to the latest
;;; version of your files.
;
;;; Set to the location of your Org files on your local system
;(setq org-directory "~/org")
;
;;; Set to the name of the file where new notes will be stored
;(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;
;;; Set to <your Dropbox root directory>/MobileOrg.
;(setq org-mobile-directory "~/Dropbox/MobileOrg")


; good references
; http://howardabrams.com/projects/dot-files/emacs-org.html
; http://sachachua.com/wp/2007/12/29/how-to-use-emacs-org-as-a-basic-day-planner/


(provide 'emacs-org)

;;======================================================================
;; Local variables
;;Local Variables:
;;indent-tabs-mode: nil
;;allout-layout: (-1 : 0)
;;End:
