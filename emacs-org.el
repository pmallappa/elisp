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

;; highlight source code
(setq org-src-fontify-natively t)

;; hide underline and bold fonts
;(eval-after-load "org" '(clean-fonts))

;;============================================================
;; Set the return key to activate a link
;; Needs to be set before org.el is loaded.
(setq org-return-follows-link t)

;;============================================================
;; set the archive file location
(setq org-archive-location (concat HOME_DIR "/org/archives/%s_archive::"))

;;============================================================
;; needed for the calculator conversion table in the fun.org file
(require 'calc-funcs)

;;============================================================
;; don't see scheduled tasks until the scheduled data
(setq org-agenda-todo-ignore-scheduled 'future
      org-agenda-todo-ignore-with-date nil
      org-agenda-todo-ignore-timestamp nil
      org-agenda-todo-ignore-deadlines nil)

(setq org-agenda-tags-todo-honor-ignore-options t)

;;============================================================ prevent linum
;; mode from activating in org buffers as it slows emacs to a crawl with
;; large org files
(add-hook 'org-mode-hook (lambda() (linum-mode -1)))

;;============================================================
;; enable babel code execute functions
;(require 'ob-calc)
;(require 'ob-sql)
;(require 'ob-latex)
;(require 'ob-gnuplot)
;
;(require 'ob-ditaa)
;(setq org-ditaa-jar-path "/usr/local/bin/ditaa")

;;============================================================
;; capture templates (replaces remember)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/pending.org")
         "* TODO %^{Task Description} %^g\n  Added:  %U\n  %?\n  %a" :empty-lines-after 1)
        ("a" "Appointment" entry (file "~/org/pending.org")
         "* %^{Appt Description}  %^g\n  %^T\n  %i%?\n  %a" :empty-lines-after 1)
        ("n" "Note" entry (file "~/org/pending.org")
         "* %^{Note Description}  %^g\n  %U\n  %i%?\n  %a" :empty-lines-after 1)
        ("m" "Meeting" entry (file "~/org/pending.org")
         "* %^{With whom} :meeting:\n  %?" :clock-in t :empty-lines-after 1 :jump-to-captured :unnarrowed)
        ("p" "Phone Call" entry (file "~/org/pending.org")
         "* %^{Mode|IM|Phone Call} with %^{With whom} :phone:\n  %?" :clock-in t :empty-lines-after 1 :jump-to-captured :unnarrowed)
        ("w" "Password" table-line (file+olp "~/org/passwords.gpg" "Passwords")
         "| %^{Title} | %^{Username} | %^{Password} | %^{URL} | %^{Notes}")

        ("s" "Siemens capture templates")
        ("st" "Transport" entry (file+olp "~/org/siemens.org" "Support" "Transports (Support)")
         "* TODO Transport %^{Transport Number}\n  Added:  %U\n  %?" :empty-lines-after 1 :jump-to-captured :unnarrowed)
        ("sa" "AHD Ticket" entry (file+olp "~/org/siemens.org" "Support" "AHD (Support)")
        "* AHD-%^{AHD Number} - %^{Description}\n  Added: %U\n** TODO Complete AHD-%\\1 - %\\2\n   Added: %U\n** Details for AHD-%\\1\n   %?\n** Notes for AHD-%\\1\n" :empty-lines-after 1 :jump-to-captured :unnarrowed)
        ("sj" "JIRA Ticket" entry (file+olp "~/org/siemens.org" "Support" "JIRA (Support)")
        "* JIRA %^{JIRA Number} - %^{Description}\n  Added: %U\n** TODO Complete JIRA %\\1 - %\\2\n   Added: %U\n** Details for %\\1\n   %?\n** Notes for %\\1\n" :empty-lines-after 1 :jump-to-captured :unnarrowed)
        ("so" "One on One" entry (file+olp "~/org/siemens.org" "Notes")
         "* One on One %u :meeting:onOne:\n  %?" :clock-in t :empty-lines-after 1 :jump-to-captured :unnarrowed)

        ("f" "Financial capture templates")
        ("fb" "Bill" entry (file+olp "~/org/finances.org" "Bills")
         "* Paid %^{Bill Paid|AT&T|Matrix|USAA Auto Ins|USAA Master Card}\n  %U\n  Amount: $%^{Amount $}\n  Source: %^{Source Acct|Fifth-Third|NFCU chkg}\n  Confirm: %^{Confirmation #}" :empty-lines-after 1)
        ("ff" "Funds" entry (file+olp "~/org/finances.org" "Funds")
         "* Transferred Money %U\n     From: %^{Transferred From:|Fifth-Third Chkg|NFCU Chkg|NFCU Svgs}\n       To: %^{To:|NFCU Svgs|NFCU Chkg|Fifth-Third Chk}\n   Amount: $%^{Amount $}\n  Confirm: %^{Confirmation #}" :empty-lines-after 1)
        ))

;;============================================================
;; Targets include this file and any file contributing to the agenda - up to
;; 3 levels deep for refiling entries (C-c C-w)
(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

;;============================================================
;; use associated apps for the following link types
(setq org-file-apps
      '((auto-mode . emacs)
         ("\\.mm\\'" . default)
         ("\\.x?html?\\'" . default)
         ("\\.pdf\\'" . default)
         ("\\.docx?\\'" . default)
         ("\\.pptx?\\'" . default)
         ("\\.xlsx?\\'" . default)
         ("\\.png\\'" . default)
         ("\\.jsp\\'" . emacs)
         ("_archive\\'" . emacs) ; org-mode archive files
         (directory . emacs)))

;;============================================================
;; various settings
(setq org-agenda-include-diary t
      org-agenda-remove-tags t
      org-agenda-include-all-todo t
      org-agenda-tags-column -125
      org-tags-column -125
      org-use-fast-todo-selection t
      org-confirm-elisp-link-function `y-or-n-p
      org-attach-directory (concat HOME_DIR "/org/data"))
 
;;============================================================
;; Set up org files
(setq org-agenda-files 
      (list 
       (concat HOME_DIR "/org/journal.org")
       (concat HOME_DIR "/org/navy.org")
       (concat HOME_DIR "/org/siemens.org")
       (concat HOME_DIR "/org/finances.org")
       (concat HOME_DIR "/org/pending.org")))

;;============================================================
;; Set up the keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; modify C-a and C-e to jump to beg and end of headlines
(setq org-special-ctrl-a/e t)

;;============================================================
;; Set up the global tags list
(setq org-tag-persistent-alist
      '(("phone"       . ?p)
        ("appointment" . ?a)
        ("meeting"     . ?m)
        ("note"        . ?n)
        ("noexport"    . ?x)
        ("crypt"       . ?c)))

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
;; # -*- buffer-auto-save-file-name: nil; -*-

;; To decrypt, place the cursor on the heading and execute
;; M-x org-decrypt-entry
(define-key org-mode-map (kbd "C-c /") 'org-decrypt-entry)

;;============================================================
;; Create custom agenda views
;; good resource is 
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html

;; set the agenda grid display
;; default value is
;;((daily today require-timed)
;; "----------------"
;; (800 1000 1200 1400 1600 1800 2000))
(setq org-agenda-time-grid
      '((daily)
        "----------------"
        (800 1000 1200 1400 1600 1800 2000)))


(setq org-agenda-block-separator 
      "===================================================================")

(setq org-agenda-current-time-string
      "now - - - - - - - - - - - - - -")

(setq org-agenda-custom-commands
      '(("a" "Combined agenda and tasks"
         ((agenda)
          (todo))
         ((org-agenda-start-on-weekday 1)) ; start on Monday
         ("~/.org-agenda.txt"))
        ("w" "Work agenda and tasks"
         ((agenda)
          (tags-todo "pending")
          (tags-todo "+siemens+task")
          (tags-todo "+siemens+note")
          (tags-todo "+siemens+AHD")
          (tags-todo "+siemens+JIRA")
          (tags-todo "+siemens+TRANSPORTS"))
         ((org-agenda-files '("~/org/siemens.org" "~/org/pending.org"))))
        ("n" "Navy agenda and tasks"
         ((agenda)
          (tags-todo "pending")
          (tags-todo "navy"))
         ((org-agenda-files '("~/org/navy.org" "~/org/pending.org"))))
        ("p" "Personal agenda and tasks"
         ((agenda)
          (tags-todo "pending")
          (tags-todo "personal"))
         ((org-agenda-files '("~/org/journal.org" "~/org/finances.org" "~/org/pending.org"))))
        ("c" "Calendar" agenda ""
         ((org-agenda-ndays 7)
          (org-agenda-start-on-weekday 1)
          (org-agenda-time-grid nil)                    
          (org-agenda-repeating-timestamp-show-all t)
          (org-agenda-entry-types '(:timestamp :sexp))))
        ))

;; To use the GeekTool agenda from a command line, put the following
;; in the shell command field. Note, emacs server must be running for
;; this to function correctly. I could have called emacs itself, but I
;; usually have it open anyway.
;; 'emacsclient -e "(progn (org-store-agenda-views))" ; cat ~/.org-agenda.txt'

;;============================================================
;; Set up the TODO states
(setq org-todo-keywords
      '((sequence "TODO(t)" "OPEN(o)" "WAIT(w@/!)" "|" "DONE(d@)" "TRFX(f@)" "CANX(c@)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "Burlywood3"))
 	("OPEN" . (:foreground "ForestGreen"))
        ("WAIT" . (:foreground "Firebrick3"))
	("DONE" . (:foreground "Wheat4"))
	("TRFX" . (:foreground "Wheat4"))
	("CANX" . (:foreground "Wheat4"))))

(setq org-priority-faces
      '((?A . (:foreground "red"))
        (?B . (:foreground "Wheat4"))
        (?C . (:foreground "grey"))))

;; log and add notes when completing a task
(setq org-log-done 'note)

;; Mark todo done when all subentries are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

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

;;;============================================================
;;; publishing functions 
(defun org-pandoc-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown using Pandoc.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'pandoc filename (concat "." (or (plist-get plist :md-extension) "md"))
   plist pub-dir))

(defun org-md-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown using ox-md.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'md filename (concat "." (or (plist-get plist :md-extension) "md"))
   plist pub-dir))

;;============================================================
;; fix the default styles
;; set the css by modifying the file ~/org/templates/org-html.txt
(setq org-html-head-include-default-style nil)
(setq org-html-use-unicode-chars t)

;; fix the default table attributes (UGH!!)
(setq org-html-table-default-attributes
      '(:border "1px solid" :cellspacing "0" :cellpadding "1"))

;; always use the infojs options when publishing
(setq org-html-use-infojs t)

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
         :publishing-directory "~/public/html"
         :html-extension "html"
         :exclude "pending.org"
         :with-sub-superscript nil
         :publishing-function org-html-publish-to-html
         :preserve-breaks t)
        ("orgmd"
         :base-directory "~/org" 
         :base-extension "org"
         :md-extension "markdown"
         :exclude "pending.org"
         :publishing-directory "~/public/org_md"
         :publishing-function org-md-publish-to-md
         :preserve-breaks t)
        ("panmd"
         :base-directory "~/org" 
         :base-extension "org"
         :md-extension "markdown"
         :exclude "pending.org"
         :publishing-directory "~/public/pandoc_md"
         :publishing-function org-pandoc-publish-to-md
         :preserve-breaks t)
        ("static"
         :base-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :recursive t
         :publishing-directory "~/public/html/"
         :publishing-function org-publish-attachment)
        ("html"
         :components ("orghtml" "static"))
        ("markdown"
         :components ("orgmd"))
        ("markdown-pandoc"
         :components ("panmd"))
        ))

;; prevent mode hooks from running when exporting to speed things up
(defun is-buffer-file-temp ()
  (interactive)
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file"
  (let ((f (buffer-file-name))
        (rlt t))
    (if f
        (if (and (not (string-match temporary-file-directory f))
                 (not (file-exists-p (replace-regexp-in-string "\.html$" ".org" f))))
          (setq rlt nil)))
    rlt))


;;======================================================================
;; https://github.com/abo-abo/worf/blob/master/worf.el
;; Thanks to Leo Ufimtsev
;; in Stack Overflow
;; http://stackoverflow.com/questions/28030946/emacs-org-mode-search-only-headers
;;
;; use M-x worf-goto to search org headers
;;
;; Type a number to limit to that header depth
;; continue with the search string
;; Or just type a search string to search all headers
;;
(defun worf--pretty-heading (str lvl)
  "Prettify heading STR or level LVL."
  (setq str (or str ""))
  (setq str (propertize str 'face (nth (1- lvl) org-level-faces)))
  (let (desc)
    (while (and (string-match org-bracket-link-regexp str)
                (stringp (setq desc (match-string 3 str))))
      (setq str (replace-match
                 (propertize desc 'face 'org-link)
                 nil nil str)))
    str))
(defun worf--pattern-transformer (x)
  "Transform X to make 1-9 select the heading level in `worf-goto'."
  (if (string-match "^[1-9]" x)
      (setq x (format "^%s" x))
    x))

(defun goto-org-heading ()
  "Jump to a heading with `helm'."
  (interactive)
  (require 'helm-match-plugin)
  (let ((candidates
         (org-map-entries
          (lambda ()
            (let ((comp (org-heading-components))
                  (h (org-get-heading)))
              (cons (format "%d%s%s" (car comp)
                            (make-string (1+ (* 2 (1- (car comp)))) ?\ )
                            (if (get-text-property 0 'fontified h)
                                h
                              (worf--pretty-heading (nth 4 comp) (car comp))))
                    (point))))))
        helm-update-blacklist-regexps
        helm-candidate-number-limit)
    (helm :sources
          `((name . "Headings")
            (candidates . ,candidates)
            (action . (lambda (x) (goto-char x)
                        (call-interactively 'show-branches)
                        (worf-more)))
            (pattern-transformer . worf--pattern-transformer)))))

(provide 'emacs-org)

;;======================================================================
;; Local variables
;;Local Variables:
;;indent-tabs-mode: nil
;;allout-layout: (-1 : 0)
;;End:
