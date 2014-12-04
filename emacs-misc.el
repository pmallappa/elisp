
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
;; provide unique names for buffers with the same filename loaded
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;======================================================================
;; re-enable the commands upcase region and downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;======================================================================
;; Enable the commands `narrow-to-region' ("C-x n n") and
;;`eval-expression' ("M-ESC", or "ESC ESC").
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;;======================================================================
;; Sensibly split windows horizontally column threshold
(setq split-width-threshold 140)

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
;;; adds a bitmap to the fringe that marks the current mark. Handy
;;; when popping off the ring
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


;;;======================================================================
;;; For Mac users, swap the command and option keys between mac and windows keyboards
;(defun swap-meta-and-super ()
;  "Swap the mapping of meta and super. Very useful for people using their Mac
;with a Windows external keyboard from time to time."
;  (interactive)
;  (if (eq mac-command-modifier 'super)
;      (progn
;        (setq mac-command-modifier 'meta)
;        (setq mac-option-modifier 'super)
;        (message "Command is now bound to META and Option is bound to SUPER."))
;    (progn
;      (setq mac-command-modifier 'super)
;      (setq mac-option-modifier 'meta)
;      (message "Command is now bound to SUPER and Option is bound to META."))))

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
;; browse the unicode character set
;; not all characters will display, depending on font selected
(defun list-unicode-display (&optional regexp)
  "Display a list of unicode characters and their names in a buffer."
  (interactive "sRegexp (default \".*\"): ")
  (let* ((regexp (or regexp ".*"))
         (case-fold-search t)
         (cmp (lambda (x y) (< (cdr x) (cdr y))))
         ;; alist like ("name" . code-point)
         (char-alist (sort (cl-remove-if-not (lambda (x) (string-match regexp (car x)))
                                             (ucs-names))
                           cmp)))
    (with-help-window "*Unicode characters*"
      (with-current-buffer standard-output
        (dolist (c char-alist)
          (insert (format "0x%06X\t" (cdr c)))
          (insert (cdr c))
          (insert (format "\t%s\n" (car c))))))))


;;======================================================================
;; ignore case in file and buffer name completions
(setq completion-ignore-case 't)
(setq read-file-name-completion-ignore-case 't)
(setq read-buffer-completion-ignore-case 't)

;;======================================================================
;; convience functions
;;======================================================================

;;======================================================================
;; Convert degrees, minutes and seconds to decimal format.
;; Place the results into the kill ring for pasting
(defun deg2dec (deg min sec)
  "Convert degrees, minutes, seconds to decimal equivilent"
  (interactive "nDeg: 
nMin: 
nSec: ")
  (let ((s (concat (calc-eval (format "deg(%s@ %s' %s\")" deg min sec)))))
    (message (concat "DEG:: "
                     (int-to-string deg) "d "
                     (int-to-string min) "' "
                     (int-to-string sec) "\"    Decimal: " s))
    ; place into the kill ring for pasting                       
    (kill-new s)))

;;======================================================================
;; immediately go to the scratch buffer from anywhere else
(defun scratch ()
  "Switch to an existing *scratch* buffer or create a new one if necessary"
  (interactive)
  (switch-to-buffer "*scratch*")
  (if current-prefix-arg
      (delete-region (point-min) (point-max))
    (goto-char (point-max)))
  )

;; From: Kevin Rodgers <kevin.d.rodgers@gmail.com>
;; Subject: Re: How to get rid of *GNU Emacs* buffer on start-up?
;; Newsgroups: gnu.emacs.help
;; Date: Fri, 19 Sep 2008 20:35:08 -0600
(defun switch-to-new-buffer ()
 "Switch to a new *scratch* buffer. Creates additional buffers if
 scratch is already in use"
  (interactive)
  (switch-to-buffer (generate-new-buffer "*scratch*"))
  (setq switch-to-new-buffer t))

;;======================================================================
;; Generate a tabarall (tgz) from within dired
;; From: Emilio Lopes <eclig@gmx.net>
;; Subject: Re: Archiving files and directories from dired
;; Newsgroups: comp.emacs
;; tar gzip selected files and directories from within dired
(defun my-dired-do-tarball (tarfile)
  (interactive "star.tgz file name: ")
  (dired-do-shell-command (format "tar czf %s *" tarfile)
                          nil
                          (dired-get-marked-files t)))


;;======================================================================
;; kill trailing whitespace
;; Thanks Roman Belenov <roman@nstl.nnov.ru>
;;======================================================================
(defun kill-whitespace ()
  "Kill trailing whitespace"
  (interactive)
  (save-excursion
    (progn
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
	(replace-match "" nil nil)))))

;;======================================================================
;; scroll with the cursor in place, moving the
;; page instead
;; Navigation Functions
(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

(global-set-key "\M-n" 'scroll-up-in-place)
(global-set-key "\M-p" 'scroll-down-in-place)

;;======================================================================
;; toggle truncate lines and redraw the display
(defun trunc ()
  "Toggle the truncate-line variable"
  (interactive)
  (toggle-truncate-lines (if truncate-lines nil t)))

;;======================================================================
;; functions to display file and path information
;; show the full path and filename in the message area
(defun path ()
    (interactive)
    (message buffer-file-name)
    (kill-new (buffer-file-name)))

;;======================================================================
;; provide save-as functionality without renaming the current buffer
;; From: Robinows@aol.com
(defun save-as (new-filename)
  (interactive "FFilename:")
  (write-region (point-min) (point-max) new-filename))

;;======================================================================
;; Functions to insert the date, the time, and the date and time at
;; point.
;; Useful for keeping records and automatically creating program headers
(defvar insert-time-format "%H:%M"
  "*Format for \\[insert-time]. See `format-time-string' for info on how to format.")

(defvar insert-date-format "%Y-%m-%d %a"
  "*Format for \\[insert-date]. See `format-time-string' for info on how to format.")

(defun insert-time ()
  "Insert the current time according to the variable `insert-time-format'."
  (interactive "*")
  (insert (concat (format-time-string insert-time-format (current-time)))))

(defun insert-date ()
  "Insert the current date according to the variable `insert-date-format'."
  (interactive "*")
  (insert (concat (format-time-string insert-date-format (current-time)))))

(defun insert-date-time ()
  "Insert the current date formatted with `insert-date-format',
then a space, then the current time formatted with
`insert-time-format'."
  (interactive "*")
  (progn
    (insert-date)
    (insert " ")
    (insert-time)))

(defun idt ()
  "Shortcut to `insert-date-time'"
(interactive)
(insert-date-time))

(defun mp-insert-date ()
  (interactive)
  (insert (format-time-string "%x")))
 
(defun mp-insert-time ()
  (interactive)
  (insert (format-time-string "%X")))
 
(global-set-key (kbd "C-c i d") 'mp-insert-date)
(global-set-key (kbd "C-c i t") 'mp-insert-time)

(defun insert-current-file-name ()
  (interactive)
  (insert (buffer-file-name (current-buffer))))


;;======================================================================
;; byte compile the current buffer on saving it
(defvar mode-specific-after-save-buffer-hooks nil
  "Alist (MAJOR-MODE . HOOK) of after-save-buffer hooks
specific to major modes.")

(defun run-mode-specific-after-save-buffer-hooks ()
  "Run hooks in `mode-specific-after-save-buffer-hooks' that match the
current buffer's major mode.  To be put in `after-save-buffer-hooks'."
  (let ((hooks mode-specific-after-save-buffer-hooks))
    (while hooks
      (let ((hook (car hooks)))
    (if (eq (car hook) major-mode)
	(funcall (cdr hook))))
      (setq hooks (cdr hooks)))))

(defun ask-to-byte-compile ()
  "Ask the user whether to byte-compile the current buffer
if its name ends in `.el' and the `.elc' file also exists."
  (let ((name (buffer-file-name)))
    (and name
     (string-match "\\.el$" name)
     (file-exists-p (concat name "c"))
     (if (y-or-n-p (format "Byte-compile %s? " name))
	 (byte-compile-file name)
       (message "")))))

(setq mode-specific-after-save-buffer-hooks
      '((emacs-lisp-mode . ask-to-byte-compile)
	(lisp-mode . ask-to-byte-compile)))

(setq after-save-hook '(run-mode-specific-after-save-buffer-hooks))

;;======================================================================
;; apply join-line over a region
;; From: "Ankur Jain" <jainankur@gmail.com>
;; Newsgroups: gnu.emacs.help
;; Date: Thu, 31 May 2007 10:21:00 +0530
(defun join-region (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

;;======================================================================
;; this function prints an ascii table in a new buffer 4 columns
(defun ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
;  (pop-to-buffer "*ASCII*" t)
;  (local-set-key "q" 'bury-buffer)
  (with-help-window "*Ascii characters*"
    (with-current-buffer standard-output
      (save-excursion
        (let ((i -1))
          (insert "ASCII characters 0 thru 127.\n\n")
          (insert " Dec  Hex  Char |  Dec  Hex  Char |  Dec  Hex  Char |  Dec  Hex  Char\n")
          (insert " ---------------+-----------------+-----------------+----------------\n")
          (while (< i 31)
            (insert (format "%4d %4x %4s  | %4d %4x %4s  | %4d %4x %4s  | %4d %4x %4s\n"
                            (setq i (+ 1  i)) i (single-key-description i)
                            (setq i (+ 32 i)) i (single-key-description i)
                            (setq i (+ 32 i)) i (single-key-description i)
                            (setq i (+ 32 i)) i (single-key-description i)))
            (setq i (- i 96))))))))

;; unicode symbols
(defun unicode-table (&optional regexp)
  "Display a list of unicode characters and their names in a buffer."
  (interactive "sRegexp (default \".*\"): ")
  (let* ((regexp (or regexp ".*"))
         (case-fold-search t)
         (cmp (lambda (x y) (< (cdr x) (cdr y))))
         ;; alist like ("name" . code-point)
         (char-alist (sort (cl-remove-if-not (lambda (x) (string-match regexp (car x)))
                                             (ucs-names))
                           cmp)))
    (with-help-window "*Unicode characters*"
      (with-current-buffer standard-output
        (dolist (c char-alist)
          (insert (format "0x%06X\t" (cdr c)))
          (insert (cdr c))
          (insert (format "\t%s\n" (car c))))))))

;;======================================================================
;; this command will list all available fonts. Good if the font
;; you want does not appear in the font dialog
(defun list-fonts()
  "Return a list of all available fonts"
  (interactive)
    (pop-to-buffer "*fontlist*")
    (erase-buffer)
    (insert-string (prin1-to-string (x-list-fonts "*")))

    ; delete the leading ("
    (goto-char (point-min))
    (delete-char 2)

    ; replace " " with a newline
    (while (re-search-forward "\" \"" nil t)
      (replace-match "\n"))

    ; delete the trailing ")
    (goto-char (point-max))
    (delete-char -2)

    ; sort the region
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-min))

    ; set the 'q' key to hide the window
    (local-set-key "q" (quote delete-window))
  )

;;======================================================================
(defun explore ()
  "Open the current directory in the OS file explorer"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute
     "explore"
     (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin")
    (shell-command "open ."))
   ))

;;======================================================================
(defun replace-extended-chars (beg end)
  "Replace the most common extended characters with ASCII versionsversions
For a list of unicode characters, see `unicode-table'"
  (interactive "r")
  (save-excursion
    (format-replace-strings
     '(
       ("\x2010" . "-")
       ("\x2011" . "-")
       ("\x2012" . "-")
       ("\x2013" . "--")
       ("\x2014" . "--")
       ("\x2015" . "--")
       ("\x2018" . "'")
       ("\x2019" . "'")
       ("\x201A" . ",")
       ("\x201B" . "'")
       ("\x201C" . "\"")
       ("\x201D" . "\"")
       ("\x201E" . "\"")
       ("\x201F" . "\"")
       ("\x2033" . "\"")
       ("\x301D" . "\"")
       ("\x301E" . "\"")
       ("\xFF02" . "\"")
       ("\x2026" . "...")
       )
     nil beg end)))

;;======================================================================
;; "Unfill" a paragraph by converting to a single line
;; Stefan Monnier <foo at acm.org>. It is the opposite of
;; fill-paragraph Takes a multi-line paragraph and makes it into a
;; single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column 46488))
    (fill-paragraph nil)))

(defun unfill-region ()   "Do the opposite of fill-region; stuff all
paragraphs in the current region into long lines."
  (interactive)
  (let ((fill-column 9000))
    (fill-region (point) (mark))))

;;======================================================================
;; run to eliminate bold and underline faces
(defun clean-fonts ()
(interactive)
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil))
  (face-list)))

;;;_*======================================================================
;;;_* get the font information for the text under the cursor
(defun what-face (point)
  "Return the font-lock face information at the current point
Thanks to Miles Bader <miles@lsi.nec.co.jp> for this (gnus.emacs.help)"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
    (if face
	(message "Face: %s" face)
      (message "No face at %d" point))))

;;============================================================
;; toggle between variable pitch and fixed pitch font for 
;; the current buffer
(defun fixed-pitch-mode ()
  (buffer-face-mode -1))

(defun toggle-pitch (&optional arg)
  "Switch between the `fixed-pitch' face and the `variable-pitch' face"
  (interactive)
  (buffer-face-toggle 'variable-pitch))

;;======================================================================
;; Color convenience functions

;;======================================================================
(require 'thingatpt)

(defun rgb (red green blue)
  "Convert decimal RGB color specification to hexidecimal place into the kill ring"
  (interactive "nRed: 
nGreen: 
nBlue: ")
  (let ((s (concat (format "#%02x%02x%02x" red green blue))))
           (message (concat "DEC RGB: "
                            (int-to-string red) " "
                            (int-to-string green) " "
                            (int-to-string blue) "    HEX: " s))
           ; place into the kill ring for pasting
           (kill-new s)))

(defun h2d (nbr)
  "Convert hexidecimal number to decimal and place into the kill ring"
  (interactive "sHex Number: ")
  (let ((decNbr (string-to-number nbr 16)))
    (message "Hex %s is Dec %d" nbr decNbr)
    (kill-new (int-to-string decNbr))))

(defun d2h (nbr)
  "Convert decimal number to hexidecimal and place into the kill ring"
  (interactive "nDec Number: ")
  (let ((hexNbr (format "%02x" nbr)))
    (message "Dec %d is Hex %s" nbr hexNbr)
    (kill-new hexNbr)))

;; blatantly stolen from sqlplus-shine-color
(defun cm-adjust-color-16bit (color percent)
  "Returns the 16-bit hex value (65k) of the RGB color
adjusted by the percent specified and places the result into the
kill ring.

See `cm-adjust-color' for details"
  (when (equal color "unspecified-bg")
    (setq color (if (< percent 0) "white" "black")))
  (kill-new
   (apply 'format "#%02x%02x%02x" 
          (mapcar (lambda (value)
                    (min 65535 (max 0 (* (+ (/ value 655) percent) 655))))
                  (color-values color)))))

(defun cm-adjust-color (color percent)
  "Returns the 8-bit hex value of the RGB color (256 colors)
adjusted by the percent specified and places the result into the
kill ring.

COLOR can be in the form of a color name (SteelBlue4),
or a hex representation (#36648b).

PERCENT is a decimal number. Negative values darken the
output color, and positive value brighten it.

An example usage to adjust the background color of a face in
relation to the default background would be:
  (set-face-background 'mode-line (cm-adjust-color (face-background 'default) -10))

Also see `cm-adjust-color-16bit' for a version which returns 16-bits per RGB color (65k)"
  (when (equal color "unspecified-bg")
    (setq color (if (< percent 0) "white" "black")))
  (kill-new
   (apply 'format "#%02x%02x%02x"
          (mapcar
           (lambda (value)
             (/
              (min 65535 (max 0 (* (+ (/ value 655) percent) 655)))
              256))
           (color-values color)))))

(provide 'emacs-misc)
