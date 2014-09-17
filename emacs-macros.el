;;;_* -*-mode: emacs-lisp -*-

;;======================================================================
;; convience functions
;;======================================================================

(require 'thingatpt)

;;======================================================================
;; convert a string of 3 decimal numbers to hex and place the result
;; into the kill ring for pasting
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

;; set filename only in the Modeline display
(defun short-file-name ()
  "Display the file path and name in the modeline"
  (interactive "*")
  (setq-default mode-line-buffer-identification '("%12b")))

;; set the full path and filename only in the Modeline display
(defun long-file-name ()
  "Display the full file path and name in the modeline"
  (interactive "*")
  (setq-default mode-line-buffer-identification
    '("%S:"(buffer-file-name "%f"))))

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
(defun explore ()
  "Open the current directory in the OS file explorer"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore"
                       (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin")
    (shell-command "open ."))
   ))

;;======================================================================
;; WGUC Stream access
(defun wguc ()
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "open" 
                       (concat (getenv "PROGRAMFILES") "\\VideoLAN\\VLC\\vlc.exe")
                       "http://cpr2.streamguys.net/wguc-nopreroll"))
   ((string-equal system-type "darwin")
     (shell-command "open /Applications/vlc.app http://cpr2.streamguys.net/wguc-nopreroll"))))


;;======================================================================
;; Siemens development convenience functions


;;======================================================================
;; keyboard macro definitions.
;; The macro name is just after defalias '<macro>.  You execute the
;; macro by typing;;; Esc-x <macro_name>;;;
;; 1) define the macro ( C-x ( to begin, type macro then C-x ) to end )
;; 2) name the macro ( M-x name-last-kbd-macro )
;; 3) insert the macro into your .emacs file. Go the the end of the
;;    emacs and execute the following
;;    M-x insert-kbd-macro <return> macro name <return>

;; bring up the color display
(defalias 'colors
  (read-kbd-macro "M-x list-colors-display RET"))

;; bring up the faces display
(defalias 'faces
  (read-kbd-macro "M-x list-faces-display RET"))

;; justification of a paragraph at the current fill column
(defalias 'justify-center
  (read-kbd-macro "M-x set-justification-center"))
(defalias 'justify-full
  (read-kbd-macro "M-x set-justification-full"))
(defalias 'justify-right
  (read-kbd-macro "M-x set-justification-right"))
(defalias 'justify-left
  (read-kbd-macro "M-x set-justification-left"))
(defalias 'justify-none
  (read-kbd-macro "M-x set-justification-none"))

;;======================================================================
(defun replace-smart-quotes (beg end)
  "Replace the curly smart quotes with ASCII versions"
  (interactive "r")
  (save-excursion
    (format-replace-strings
     '(("\x201c" . "\"")
       ("\x201d" . "\"")
       ("\x2018" . "'")
       ("\x2019" . "'"))
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
;; TrueCrypt shortcuts
(defun tcmount ()
  "Mount the truecrypt favorite volumes"
  (interactive)
  (shell-command
   (concat "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
	   " --auto-mount=favorites"
	   " --password=\"" (read-passwd "Passphrase: ") "\"")))

(defun tcmount1b ()
  "Mount the truecrypt volume JAVA_LIB01b on Passport2TB"
  (interactive)
  (shell-command
   (concat "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
           " /Volumes/Passport2TB/Java/java_lib01 /Volumes/JAVA_LIB01b"
	   " --password=\"" (read-passwd "Passphrase: ") "\"")))
(defun tcmount2b ()
  "Mount the truecrypt volume JAVA_LIB02b on Passport2TB"
  (interactive)
  (shell-command
   (concat "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
           " /Volumes/Passport2TB/Java/java_lib02 /Volumes/JAVA_LIB02b"
	   " --password=\"" (read-passwd "Passphrase: ") "\"")))
(defun tclist ()
  "List the mounted TrueCrypt directories"
  (interactive)
  (shell-command
   (concat "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
           " --list")))

(defun tcdmount ()
  "Unmount the truecrypt favorite volumes"
  (interactive)
  (shell-command
   "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt --dismount"))

;; load the page in browser and save the file name to the clipboard
;;| filename | url |
(fset 'loadvid
   [?\C-a tab ?\C-  ?\M-e ?\M-w tab ?\M-x ?f ?x return return])

;; Create a file link in org mode to the current dired file 
;; Two buffers showing: org file listing, and dired listing with files to link
;; Start with the cursor on the filename in the dired buffer
;; | filename | url |
(fset 'filevid
   [?\C-c ?l ?\C-  ?\C-e ?\M-b ?\C-b ?\M-w ?\C-x ?o ?\M-< ?\C-s ?\M-y ?\C-  ?\M-a ?\C-c ?\C-l return ?\C-x ?o ?\C-n])

(provide 'emacs-macros)

;;;_*======================================================================
;;;_* Local variables
;;Local Variables:
;;indent-tabs-mode: nil
;;allout-layout: (-1 : 0)
;;End:
