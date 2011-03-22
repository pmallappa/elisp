;;;_.======================================================================
;;;_. Load path
;(add-to-list 
; 'load-path (expand-file-name (concat EMACS_PKGS "/git")))

;;;_.======================================================================
;;;_. get rid of the default messages on startup
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;;;_.======================================================================
;;;_. default to unix line endings
(setq-default buffer-file-coding-system 'undecided-unix)

;;;_.======================================================================
;;;_. Change the default mode to org-mode instead of lisp
;(setq initial-major-mode 'org-mode)

;;;_.======================================================================
;;;_. font lock settings
;; enable font-lock for ALL modes that support it
(global-font-lock-mode t)

;; increase this from the default of 300 to complete font
;; locking on larger files
(setq max-lisp-eval-depth 1000)

;;;_.======================================================================
;;;_. Global Programming indentation
;; DO NOT CHANGE THIS! It will make the code look horrible to others
;; if you use any tabs for indenting
(setq default-tab-width 8)

;; use this to change the indentation offset
(setq c-basic-offset 4)

;; use tabs for indentation (t)
(setq indent-tabs-mode nil)

;;;_.======================================================================
;;;_. Set the cursor styles
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

;;;_.======================================================================
;;;_. Highlight matching parenthesis
;; style can be 'mixed, 'expression or 'parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;;;_.======================================================================
;;;_. emacs bookmarks
(setq bookmark-default-file (concat EMACS_PKGS "/emacs.bmk"))

;;;_.======================================================================
;;;_. make backup files in a single directory and keep versions
(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/backups/temp/")))
      version-control t        ; Use version numbers for backups
      kept-new-versions 5      ; Number of newest versions to keep
      kept-old-versions 2      ; Number of oldest versions to keep
      delete-old-versions t    ; Delete excess backup versions
      backup-by-copying t)

;;;;_.======================================================================
;;;;_. crontab mode
;;; edit and automatically apply changes to the crontab file
;;; use the command C-cC-c to apply the changes.
;;; The local variable crontab-apply-after-save in the crontab file
;;; will apply the changes automatically on saving the local .crontab
;;; file as well
;(require 'crontab-mode)
;(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))

;;;_.======================================================================
;;;_. modeline modifications

;; set the encoding characters displayed in the modeline
(setq eol-mnemonic-dos ?\\
      eol-mnemonic-unix ?/
      eol-mnemonic-mac ?:
      eol-mnemonic-undecided ??)

;; first turn on column line mode
;(setq column-number-mode nil)

;; the standard display time package
;; see documentation for the function
;; format-time-string
;; for an explanation of the format
;; (format-time-string "%a %d %b %H:%M %Z" (current-time) nil)
;; is a good quick way to test your formats

;; work around a bug in the new time zones for 2007 not being loaded
;; into emacs correctly. Need to change manually when the time zone
;; changes
;(set-time-zone-rule "EST5") ;; 
;(set-time-zone-rule "EDT4") ;; 2d Sunday of March to 1st Sunday of Nov

(setq display-time-format " %a %d %b %H:%M ")     ;;;_.Fri 10 Nov 15:26
(setq display-time-day-and-date t)
(display-time)

;; format for the filename in the modeline
;; The default, "%12b", just displays the filename.
;; You can find the complete path by invoking the macro 'M-x path',
;; defined in .emacs-macros.el
(setq-default mode-line-buffer-identification '("%12b"))

;; format for the title on the titlebar
(setq frame-title-format
      (concat invocation-name "@"
              system-name
;	      (shell-command-to-string "hostname|tr -d \\\\n")
	      " -- %f"))

;;;_.======================================================================
;;;_. re-enable the commands upcase region and downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;_.======================================================================
;;;_. Set threshold to split horizontally
(setq split-width-threshold nil)

;;;_.======================================================================
;;;_. Enable the commands `narrow-to-region' ("C-x n n") and
;;;_.`eval-expression' ("M-ESC", or "ESC ESC").
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;;;_.======================================================================
;;;_. ediff configuration
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

;;;_.======================================================================
;;; adds a bitmap to the fringe that marks the current mark. Handy
;;; when popping off the ring

;; overlay an arrow where the mark is
(defvar mp-overlay-arrow-position)
(make-variable-buffer-local 'mp-overlay-arrow-position)
(add-to-list 'overlay-arrow-variable-list  'mp-overlay-arrow-position)  

(defun mp-mark-hook ()
  ;; (make-local-variable 'mp-overlay-arrow-position)
  (unless (or (minibufferp (current-buffer)) (not (mark)))
    (set
     'mp-overlay-arrow-position
     (save-excursion
       (goto-char (mark))
       (forward-line 0)
       (point-marker)))))
(add-hook 'post-command-hook 'mp-mark-hook)

;; If you want to change the bitmap (defaults to the left arrow)
;(put 'mp-overlay-arrow-position 'overlay-arrow-bitmap 'right-triangle)

;; or you can make a custom one, here’s mine:
;; make the mark fringe bitmap look cool dude
(define-fringe-bitmap 'mp-hollow-right-arrow [128 192 96 48 24 48 96 192 128] 9 8 'center)
(put 'mp-overlay-arrow-position 'overlay-arrow-bitmap 'mp-hollow-right-arrow)


;;;_.======================================================================
;;;_. get the IP address of the current machine
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

;;;_.======================================================================
;;;_. TrueCrypt shortcuts
(defun tcmount ()
  "Mount the truecrypt favorite volumes"
  (interactive)
  (shell-command
   (concat "/Applications/Utilities/TrueCrypt.app/Contents/MacOS/TrueCrypt"
	   " --auto-mount=favorites"
	   " --password=\"" (read-passwd "Passphrase: ") "\"")))

(defun tcdmount ()
  "Unmount the truecrypt favorite volumes"
  (interactive)
  (shell-command
   "/Applications/Utilities/TrueCrypt.app/Contents/MacOS/TrueCrypt --dismount"))


;;;_.======================================================================
;;;_. ignore case in file and buffer name completions
(setq completion-ignore-case 't)
(setq read-file-name-completion-ignore-case 't)
(setq read-buffer-completion-ignore-case 't)

(provide 'emacs-misc)