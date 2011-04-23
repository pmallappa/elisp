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
          (todo))
         ((org-agenda-start-on-weekday 1)) ; start on Monday
         ("~/.org-agenda.txt"))
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
          (org-agenda-hide-tags-regexp ".*")))
        ))

;; To use the GeekTool agenda from a command line, put the following
;; in the shell command field. Note, emacs server must be running for
;; this to function correctly. I could have called emacs itself, but I
;; usually have it open anyway.
;;
;; /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -e "(progn (org-store-agenda-views)) ; cat ~/.org-agenda.txt"

;;;_. Set up the TODO states
(setq org-todo-keyword-faces
      '(("TODO"      . (:foreground "red"          :weight bold))
	("NEXT"      . (:foreground "white"        :weight bold))
	("WAITING"   . (:foreground "orange"       :weight bold))
	("DELEGATED" . (:foreground "orange"       :weight bold))
	("OPEN"      . (:foreground "white"         :weight bold))
	("DONE"      . (:foreground "forest green" :weight bold))
	("CANCELLED" . (:foreground "forest green" :weight bold))
	("CLOSED"    . (:foreground "forest green" :weight bold))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "OPEN(o!)" "|" "DONE(d!/!)")
	(sequence "WAITING(w@/!)" "DELEGATED(g@/!)" "|" "CANCELLED(c!/!)")))

;;;_.============================================================
;;;_. Export to HTML options
(setq org-export-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/stylesheet.css\" />")

;; When t, place a stamp at the bottom saying 'HTML generated by
;; org-mode xxx in emacs xxx"
(setq org-export-creator-info t)

(setq org-export-exclude-tags (quote ("noexport" "ARCHIVE")))

;;;_.============================================================
;;;_. Publishing options

(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "~/org/"
         :base-extension "org"
         :publishing-directory "~/org/public_html"
         :publishing-function org-publish-org-to-html
         :headline-levels 3
         :section-numbers nil
         :table-of-contents nil
         :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/stylesheet.css\" />"
         :auto-preamble t
         :footnotes nil
         :sub-superscript nil
         :auto-postamble nil)

        ("css"
         :base-directory "~/org/css/"
         :base-extension "css\\|el"
         :publishing-directory "~/org/public_html/css"
         :publishing-function org-publish-attachment)

        ("data"
         :base-directory "~/org/data/"
         :base-extension "doc|docx|css\\|el"
         :publishing-directory "~/org/public_html/data"
         :publishing-function org-publish-attachment)

        ("website"
         :components ("orgfiles" "css" "data")
         )))

;;;;_.============================================================
;;;; org MediaWiki publishing
;(load-library "org-mediawiki")

;;;_.============================================================
;;;_. integrate Mobile Org using Dropbox
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
(setq org-mobile-directory "~/Dropbox/MobileOrg")


;;;_.============================================================
;;;_. Modify the calendar grid to prevent repeating times
(defadvice org-agenda-add-time-grid-maybe (around mde-org-agenda-grid-tweakify
                                                  (list ndays todayp))
  (if (member 'remove-match (car org-agenda-time-grid))
      (flet ((extract-window
              (line)
              (let ((start (get-text-property 1 'time-of-day line))
                    (dur (get-text-property 1 'duration line)))
                (cond
                 ((and start dur) (cons start dur))
                 (start start)
                 (t nil))))
             (duration-add
              (time duration)
              (+ time (* 100 (/ duration 60)) (% duration 60))))
        (let* ((windows (delq nil (mapcar 'extract-window list)))
               (org-agenda-time-grid
                (list (car org-agenda-time-grid)
                      (cadr org-agenda-time-grid)
                      (remove-if
                       (lambda (time)
                         (find-if (lambda (w)
                                    (if (numberp w)
                                        (equal w time)
                                      (and (>= time (car w))
                                           (< time (duration-add
                                                    (car w) (cdr w))))))
                                  windows))
                       (caddr org-agenda-time-grid)))))
          ad-do-it))
    ad-do-it))
(ad-activate 'org-agenda-add-time-grid-maybe)

;;;_.======================================================================
;;;_. org-info export libraries

;; add the following to the top of each org file to export
;;  #+INFOJS_OPT: path:org-info.js
;;  #+INFOJS_OPT: toc:nil localtoc:t view:info mouse:underline buttons:nil
;;  #+INFOJS_OPT: up:http://orgmode.org/worg/
;;  #+INFOJS_OPT: home:http://orgmode.org


;;;;_.======================================================================
;;;;_. Set up automatic reminders 
(require 'appt)
(setq org-agenda-include-diary t)
(setq appt-time-msg-list nil)
(org-agenda-to-appt)

(defadvice  org-agenda-redo (after org-agenda-redo-add-appts)
  "Pressing `r' on the agenda will also add appointments."
  (progn 
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)))

(ad-activate 'org-agenda-redo)

;; enable appt reminders, set the format to 'window and provide
;; a display function that calls todochiku
(appt-activate 1)
(setq appt-display-format 'window)
  ;;(setq appt-disp-window-function (function my-appt-disp-window))



(provide 'emacs-org)

;;;_.======================================================================
;;;_. Local variables
;;Local Variables:
;;indent-tabs-mode: nil
;;allout-layout: (-1 : 0)
;;End:
