
;;======================================================================
;; get rid of the default messages on startup
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;;======================================================================
;; default to unix line endings
(setq-default buffer-file-coding-system 'undecided-unix)

;;======================================================================
;; Change the default mode to org-mode instead of lisp
(setq initial-major-mode 'org-mode)

;;======================================================================
;; font lock settings
;; enable font-lock for ALL modes that support it
(global-font-lock-mode t)

;; increase this from the default of 300 to complete font
;; locking on larger files
(setq max-lisp-eval-depth 1000)

;;======================================================================
;; Global Programming indentation
;; DO NOT CHANGE THIS! It will make the code look horrible to others
;; if you use any tabs for indenting
(setq default-tab-width 8)

;; use this to change the indentation offset
(setq c-basic-offset 4)

;; use tabs for indentation (t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 76)

;; Replace 'yes' and 'no' with 'y' and 'n'
;(fset 'yes-or-no-p 'y-or-n-p)

;;======================================================================
;; Set the cursor styles
;Values are interpreted as follows:
;;  t              use the cursor specified for the frame
;;  nil            don't display a cursor
;;  `bar'          display a bar cursor with default width
;;  (bar . WIDTH)  display a bar cursor with width WIDTH
;;  others         display a box cursor.

;; can be set to '(bar . 2) or 't
(setq-default cursor-type '(bar . 3))

;; turn off the visible cursor in non-selected windows
(setq-default cursor-in-non-selected-windows nil)

;; turn the blinking off
(blink-cursor-mode 0)

;;======================================================================
;; Highlight matching parenthesis
;; style can be 'mixed, 'expression or 'parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;;======================================================================
;; emacs bookmarks
(setq bookmark-default-file (concat EMACS_CONFIGS "/emacs.bmk"))

;;======================================================================
;; make backup files in a single directory and keep versions
(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/backups/temp/")))
      version-control t        ; Use version numbers for backups
      kept-new-versions 5      ; Number of newest versions to keep
      kept-old-versions 2      ; Number of oldest versions to keep
      delete-old-versions t    ; Delete excess backup versions
      backup-by-copying t)

;;======================================================================
;; make intermediate directories if necessary when saving a file
(add-hook 'before-save-hook
     (lambda()
       (let ((dir (file-name-directory buffer-file-name)))
	 (when (and (not (file-exists-p dir))
		    (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
	   (make-directory dir t)))))

;;;======================================================================
;;; crontab mode
;;; edit and automatically apply changes to the crontab file
;;; use the command C-cC-c to apply the changes.
;;; The local variable crontab-apply-after-save in the crontab file
;;; will apply the changes automatically on saving the local .crontab
;;; file as well
;(require 'crontab-mode)
;(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))

;;======================================================================
;; mode-line modifications

;; first turn on column line mode
(setq column-number-mode nil)

;; time and date
(setq display-time-format " %a %m/%d %H:%M ")     ;;Fri 08/19 15:26
(setq display-time-day-and-date t)
(display-time)

;; format for the filename in the modeline
;; The default, "%12b", just displays the filename.
;; You can find the complete path by invoking the macro 'M-x path',
;; defined in .emacs-macros.el
(setq-default mode-line-buffer-identification '("%12b"))

;; format for the title on the titlebar
(setq frame-title-format
      (concat invocation-name " on "
              system-name
	      " -- %f"))

;;======================================================================
;; provide unique names for buffers with the same filename loaded
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;======================================================================
;; re-enable the commands upcase region and downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;======================================================================
;; Sensibly split windows horizontally column threshold
(setq split-width-threshold 160)

;;======================================================================
;; Enable the commands `narrow-to-region' ("C-x n n") and
;;`eval-expression' ("M-ESC", or "ESC ESC").
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;;======================================================================
;; ediff configuration
;; split windows horizontally:
(setq ediff-split-window-function 'split-window-horizontally)

;; only hilight current diff:
(setq-default ediff-highlight-all-diffs 'nil)

;; turn off whitespace checking:
(setq ediff-diff-options "-w")

;; place the control window in the same frame as the ediff buffers
;; to switch from one to the other interactively, use the command
;; ediff-toggle-multiframe
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;;;======================================================================
;;;; adds a bitmap to the fringe that marks the current mark. Handy
;;;; when popping off the ring
;;; overlay an arrow where the mark is
;(defvar mp-overlay-arrow-position)
;(make-variable-buffer-local 'mp-overlay-arrow-position)
;(add-to-list 'overlay-arrow-variable-list  'mp-overlay-arrow-position)  
;
;(defun mp-mark-hook ()
;  ;; (make-local-variable 'mp-overlay-arrow-position)
;  (unless (or (minibufferp (current-buffer)) (not (mark)))
;    (set
;     'mp-overlay-arrow-position
;     (save-excursion
;       (goto-char (mark))
;       (forward-line 0)
;       (point-marker)))))
;(add-hook 'post-command-hook 'mp-mark-hook)
;
;;; If you want to change the bitmap (defaults to the left arrow)
;;(put 'mp-overlay-arrow-position 'overlay-arrow-bitmap 'right-triangle)
;
;;; or you can make a custom one, here's mine:
;;; make the mark fringe bitmap look cool dude
;(define-fringe-bitmap 'mp-hollow-right-arrow [128 192 96 48 24 48 96 192 128] 9 8 'center)
;(put 'mp-overlay-arrow-position 'overlay-arrow-bitmap 'mp-hollow-right-arrow)


;;======================================================================
;; For Mac users, swap the command and option keys between mac and windows keyboards
(defun swap-meta-and-super ()
  "Swap the mapping of meta and super. Very useful for people using their Mac
with a Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (progn
      (setq mac-command-modifier 'super)
      (setq mac-option-modifier 'meta)
      (message "Command is now bound to SUPER and Option is bound to META."))))

;;======================================================================
;; Search for a regexp across all marked files within dired
(defun my-dired-multi-occur (string)
  "Search string in files marked by dired."
  (interactive "MList lines matching regexp: ")
  (require 'dired)
  (multi-occur (mapcar 'find-file (dired-get-marked-files)) string))


;;======================================================================
;; Join the current and subsequent lines
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

;;======================================================================
;; get the IP address of the current machine
;; unix version
(defun get-ip-address (&optional dev)
  "get the IP-address for device DEV (default: eth0)"
  (let ((dev (if dev dev "eth0"))) 
    (format-network-address (car (network-interface-info dev)) t)))

;(defun get-ip-address () 
;  "Win32: get the IP-address of the first network interface"
;  (let ((ipconfig (shell-command-to-string "ipconfig | findstr Address"))) 
;    (string-match "\\(\\([0-9]+.\\)+[0-9]+\\)" ipconfig)
;    (match-string 0 ipconfig)))

;;======================================================================
;; find non-ascii characters within a buffer
(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;;======================================================================
;; TrueCrypt shortcuts
(defun tcmount ()
  "Mount the truecrypt favorite volumes"
  (interactive)
  (shell-command
   (concat "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
	   " --auto-mount=favorites"
	   " --password=\"" (read-passwd "Passphrase: ") "\"")))


(defun tcdmount ()
  "Unmount the truecrypt favorite volumes"
  (interactive)
  (shell-command
   "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt --dismount"))

;;======================================================================
;; ignore case in file and buffer name completions
(setq completion-ignore-case 't)
(setq read-file-name-completion-ignore-case 't)
(setq read-buffer-completion-ignore-case 't)

(provide 'emacs-misc)
