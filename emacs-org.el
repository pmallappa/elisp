;;============================================================
;; Org mode personal organizer
;; good org references here!
;; http://howardabrams.com/projects/dot-files/emacs-org.html
;; http://sachachua.com/wp/2007/12/29/how-to-use-emacs-org-as-a-basic-day-planner/

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
      '(
        ("j" "Journal")
        ("jt" "Todo" entry
         (file+olp "~/org/journal.org" "Tasks")
         "* TODO %^{Task Description} %^g\n  Added:  %U\n  %?\n  %a\n")
        ("ja" "Appointment" entry
         (file+olp "~/org/journal.org" "Appointments")
         "* %^{Appt Description}  %^g\n  %^T\n  %i%?\n  %a\n")
        ("jn" "Note" entry
         (file+olp "~/org/journal.org" "Notes")
         "* %^{Note Description}  %^g\n  %T\n  %i%?\n  %a\n")

        ;; Siemens entries
        ("s" "Siemens")
        ("st" "Todo" entry
         (file+olp "~/org/siemens.org" "Tasks")
         "* TODO %^{Task Description} %^g\n  Added: %U\n  %?\n  %a\n")
        ("sa" "Appointment" entry
         (file+olp "~/org/siemens.org" "Appointments")
         "* %^{Appt Description}  %^g\n  %^T\n  %i%?\n  %a\n")
        ("sn" "Note" entry
         (file+olp "~/org/siemens.org" "Notes")
         "* %^{Note Description}  %^g\n  %T\n  %i%?\n  %a\n")

        ;; Navy entries
        ("n" "Navy")
        ("nt" "Todo" entry
         (file+olp "~/org/navy.org" "Tasks")
         "* TODO %^{Task Description} %^g\n  Added: %U\n  %?\n  %a\n")
        ("na" "Appointment" entry
         (file+olp "~/org/navy.org" "Appointments")
         "* %^{Appt Description}  %^g\n  %^T\n  %i%?\n  %a\n")
        ("nn" "Note" entry
         (file+olp "~/org/navy.org" "Notes")
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
       (concat HOME_DIR "/org/navy.org")
       (concat HOME_DIR "/org/siemens.org")
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
      '(("emacs"      . ?e)
        ("navy"       . ?n)
        ("personal"   . ?p)
        ("seimens"    . ?s)
        ("crypt"      . ?c)
        ("reference"  . ?r)))

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
(setq org-todo-keywords
      '((sequence "TODO(t)" "OPEN(o!)" "|" "DONE(d!/!)")
	(sequence "WAITING(w@/!)" "DELEGATED(g@/!)" "|" "CANCELLED(c!/!)")))

(setq org-todo-keyword-faces
      '(("TODO"      . (:foreground "red"          :weight bold))
	("OPEN"      . (:foreground "DodgerBlue"   :weight bold))
	("DONE"      . (:foreground "forest green" :weight bold))
	("WAITING"   . (:foreground "orange"       :weight bold))
	("DELEGATED" . (:foreground "orange"       :weight bold))
	("CANCELLED" . (:foreground "forest green" :weight bold))))

;; log and add notes when completing a task
(setq org-log-done 'note)

;; Mark todo done when all subentries are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


;;;============================================================
;;; publishing functions 
(defun org-pandoc-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown using Pandoc.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'pandoc filename
		      (concat
                       "."
                       (or (plist-get plist :md-extension) "md"))
		      plist pub-dir))

(defun org-pandoc-publish-to-html (plist filename pub-dir)
  "Publish an org file to Markdown using Pandoc.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'pandoc filename
		      (concat
                       "."
                       (or (plist-get plist :html-extension) "html"))
		      plist pub-dir))

(defun org-md-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown using ox-md.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'md filename
		      (concat
                       "."
                       (or (plist-get plist :md-extension) "md"))
		      plist pub-dir))

;;============================================================
;; Make org files behave in ediff
;; unfold the entire org file within ediff
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
(eval-after-load "org" '(require 'ox-pandoc nil t))
(eval-after-load "org" '(require 'ox-publish))

;; set up the files for publishing
(setq org-publish-project-alist
      '(
        ("orghtml"
         :base-directory "~/org" 
         :base-extension "org"
         :publishing-directory "~/public/org_html"
         :html-extension "html"
         :with-sub-superscript nil
         :publishing-function org-html-publish-to-html
         :preserve-breaks t)
        ("panhtml"
         :base-directory "~/org" 
         :base-extension "org"
         :publishing-directory "~/public/pandoc_html"
         :html-extension "html"
         :publishing-function org-pandoc-publish-to-html)
        ("orgmd"
         :base-directory "~/org" 
         :base-extension "org"
         :md-extension "markdown"
         :publishing-directory "~/public/org_md"
         :publishing-function org-md-publish-to-md
         :preserve-breaks t)
        ("panmd"
         :base-directory "~/org" 
         :base-extension "org"
         :md-extension "markdown"
         :publishing-directory "~/public/pandoc_md"
         :publishing-function org-pandoc-publish-to-md
         :preserve-breaks t)
        ("orgstatic"
         :base-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :recursive t
         :publishing-directory "~/public/org_html/"
         :publishing-function org-publish-attachment)
        ("panstatic"
         :base-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :recursive t
         :publishing-directory "~/public/pandoc_html/"
         :publishing-function org-publish-attachment)
        ("html-org"
         :components ("orghtml" "orgstatic"))
        ("html-pandoc"
         :components ("panhtml" "panstatic"))
        ("markdown-org"
         :components ("orgmd"))
        ("markdown-pandoc"
         :components ("panmd"))
        ))


;;============================================================
;; Org trello integration
(require 'org-trello)
;; activate for each org file
;(add-hook 'org-mode-hook 'org-trello-mode)


;;============================================================
;; integrate Mobile Org using Dropbox
;; After capturing notes or making changes on the device to your Org
;; files, be sure to sync in MobileOrg. Then run org-mobile-pull from
;; Emacs to integrate your changes. After integrating, you can run
;; org-mobile-push to make sure MobileOrg has access to the latest
;; version of your files.

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(provide 'emacs-org)

;;======================================================================
;; Local variables
;;Local Variables:
;;indent-tabs-mode: nil
;;allout-layout: (-1 : 0)
;;End:
