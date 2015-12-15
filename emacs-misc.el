
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
;; ediff configuration
;; split windows horizontally:
(setq ediff-split-window-function 'split-window-horizontally)

;; only highlight current diff:
(setq-default ediff-highlight-all-diffs 'nil)

;; turn off whitespace checking:
(setq ediff-diff-options "-w")

;; place the control window in the same frame as the ediff buffers
;; to switch from one to the other interactively, use the command
;; ediff-toggle-multiframe
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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
          (insert (cdr c))
          (insert (format "\t0x%04X\t" (cdr c)))
          (insert (format "%s\n" (car c))))))))

;;======================================================================
;; ignore case in file and buffer name completions
(setq completion-ignore-case 't)
(setq read-file-name-completion-ignore-case 't)
(setq read-buffer-completion-ignore-case 't)

;;======================================================================
;; convience functions


(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (let ((filep (buffer-file-name)))
    (if filep (find-file (concat "/sudo::" filep))
      (message "Current buffer does not have an associated file."))))

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

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))


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

(global-set-key (kbd "M-n") 'scroll-up-in-place)
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
(defvar insert-time-format "%k:%M"
  "*Format for \\[insert-time]. See `format-time-string' for info on how to format.")
(setq insert-time-format "%k:%M") ;; 08:09

(defvar insert-date-format "%Y-%m-%d"
  "*Format for \\[insert-date]. See `format-time-string' for info on how to format.")
(setq insert-date-format "%Y-%m-%d %a") ;; 2015-03-26 Thu

(defun insert-time (&optional time-format)
  "Insert the current time. Optional time format defaults to `insert-time-format'."
  (interactive "*")
  (let ((tformat (or time-format insert-time-format)))
    (insert (concat (format-time-string tformat (current-time)) " "))))

(defun insert-date (&optional date-format)
  "Insert the current date. Option format defaults to  `insert-date-format'."
  (interactive "*")
  (insert-time (or date-format insert-date-format)))

(defun insert-date-time ()
  "Insert the current date formatted with `insert-date-format',
then a space, then the current time formatted with
`insert-time-format'."
  (interactive "*")
  (insert-time
   (concat insert-date-format " " insert-time-format)))

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
    (local-set-key "q" (quote delete-window)))

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
  "Replace the most common extended characters with ASCII versions
For a list of unicode characters, see `unicode-table'"
  (interactive "r")
  (save-excursion
    (format-replace-strings
     '(("\x2010" . "-")
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
       ("\x2022" . "-")
       ("\x2033" . "\"")
       ("\x301D" . "\"")
       ("\x301E" . "\"")
       ("\xFF02" . "\"")
       ("\x2026" . "..."))
     nil beg end)))

;;======================================================================
(defun unicode-to-hex (beg end)
  "Replace extended unicode characters with their hexidecimal representation
Useful for placing unicode within Eastern European files to accurately 
represent the extended characters, while using the normal ASCII characters 
where possible

For a list of unicode characters, see the variable `unicode-table' 
or execute 'M-x `list-unicode-display'

Look into this method to recover the codepoint for a given
character as well
http://stackoverflow.com/questions/236097/finding-the-unicode-codepoint-of-a-character-in-gnu-emacs

  (or (get-char-property (point) 'untranslated-utf-8)
      (encode-char (char-after) 'ucs))

Also, you can verify the results by copying and pasting to this site
https://www.branah.com/unicode-converter

TODO rewrite to 
retrive unicode for each character, 
if it's >= 0x6
  replace with unicode
"
  (interactive "r")
  ;; preserve case sensitivity
  (setq-local case-fold-search 'nil)
  (save-excursion
    (format-replace-strings
     '(("À"."\\u00C0")("Á"."\\u00C1")("Â"."\\u00C2")("Ã"."\\u00C3")
       ("Ä"."\\u00C4")("Å"."\\u00C5")("Æ"."\\u00C6")("Ç"."\\u00C7")
       ("È"."\\u00C8")("É"."\\u00C9")("Ê"."\\u00CA")("Ë"."\\u00CB")
       ("Ì"."\\u00CC")("Í"."\\u00CD")("Î"."\\u00CE")("Ï"."\\u00CF")
       ("Ð"."\\u00D0")("Ñ"."\\u00D1")("Ò"."\\u00D2")("Ó"."\\u00D3")
       ("Ô"."\\u00D4")("Õ"."\\u00D5")("Ö"."\\u00D6")("×"."\\u00D7")
       ("Ø"."\\u00D8")("Ù"."\\u00D9")("Ú"."\\u00DA")("Û"."\\u00DB")
       ("Ü"."\\u00DC")("Ý"."\\u00DD")("Þ"."\\u00DE")("ß"."\\u00DF")
       ("à"."\\u00E0")("á"."\\u00E1")("â"."\\u00E2")("ã"."\\u00E3")
       ("ä"."\\u00E4")("å"."\\u00E5")("æ"."\\u00E6")("ç"."\\u00E7")
       ("è"."\\u00E8")("é"."\\u00E9")("ê"."\\u00EA")("ë"."\\u00EB")
       ("ì"."\\u00EC")("í"."\\u00ED")("î"."\\u00EE")("ï"."\\u00EF")
       ("ð"."\\u00F0")("ñ"."\\u00F1")("ò"."\\u00F2")("ó"."\\u00F3")
       ("ô"."\\u00F4")("õ"."\\u00F5")("ö"."\\u00F6")("÷"."\\u00F7")
       ("ø"."\\u00F8")("ù"."\\u00F9")("ú"."\\u00FA")("û"."\\u00FB")
       ("ü"."\\u00FC")("ý"."\\u00FD")("þ"."\\u00FE")("ÿ"."\\u00FF")
       ("Ā"."\\u0100")("ā"."\\u0101")("Ă"."\\u0102")("ă"."\\u0103")
       ("Ą"."\\u0104")("ą"."\\u0105")("Ć"."\\u0106")("ć"."\\u0107")
       ("Ĉ"."\\u0108")("ĉ"."\\u0109")("Ċ"."\\u010A")("ċ"."\\u010B")
       ("Č"."\\u010C")("č"."\\u010D")("Ď"."\\u010E")("ď"."\\u010F")
       ("Đ"."\\u0110")("đ"."\\u0111")("Ē"."\\u0112")("ē"."\\u0113")
       ("Ĕ"."\\u0114")("ĕ"."\\u0115")("Ė"."\\u0116")("ė"."\\u0117")
       ("Ę"."\\u0118")("ę"."\\u0119")("Ě"."\\u011A")("ě"."\\u011B")
       ("Ĝ"."\\u011C")("ĝ"."\\u011D")("Ğ"."\\u011E")("ğ"."\\u011F")
       ("Ġ"."\\u0120")("ġ"."\\u0121")("Ģ"."\\u0122")("ģ"."\\u0123")
       ("Ĥ"."\\u0124")("ĥ"."\\u0125")("Ħ"."\\u0126")("ħ"."\\u0127")
       ("Ĩ"."\\u0128")("ĩ"."\\u0129")("Ī"."\\u012A")("ī"."\\u012B")
       ("Ĭ"."\\u012C")("ĭ"."\\u012D")("Į"."\\u012E")("į"."\\u012F")
       ("İ"."\\u0130")("ı"."\\u0131")("Ĳ"."\\u0132")("ĳ"."\\u0133")
       ("Ĵ"."\\u0134")("ĵ"."\\u0135")("Ķ"."\\u0136")("ķ"."\\u0137")
       ("ĸ"."\\u0138")("Ĺ"."\\u0139")("ĺ"."\\u013A")("Ļ"."\\u013B")
       ("ļ"."\\u013C")("Ľ"."\\u013D")("ľ"."\\u013E")("Ł"."\\u0141")
       ("ł"."\\u0142")("Ń"."\\u0143")("ń"."\\u0144")("Ņ"."\\u0145")
       ("ņ"."\\u0146")("Ň"."\\u0147")("ň"."\\u0148")("ŉ"."\\u0149")
       ("Ŋ"."\\u014A")("ŋ"."\\u014B")("Ō"."\\u014C")("ō"."\\u014D")
       ("Ŏ"."\\u014E")("ŏ"."\\u014F")("Ő"."\\u0150")("ő"."\\u0151")
       ("Œ"."\\u0152")("œ"."\\u0153")("Ŕ"."\\u0154")("ŕ"."\\u0155")
       ("Ŗ"."\\u0156")("ŗ"."\\u0157")("Ř"."\\u0158")("ř"."\\u0159")
       ("Ś"."\\u015A")("ś"."\\u015B")("Ŝ"."\\u015C")("ŝ"."\\u015D")
       ("Ş"."\\u015E")("ş"."\\u015F")("Š"."\\u0160")("š"."\\u0161")
       ("Ţ"."\\u0162")("ţ"."\\u0163")("Ť"."\\u0164")("ť"."\\u0165")
       ("Ŧ"."\\u0166")("ŧ"."\\u0167")("Ũ"."\\u0168")("ũ"."\\u0169")
       ("Ū"."\\u016A")("ū"."\\u016B")("Ŭ"."\\u016C")("ŭ"."\\u016D")
       ("Ů"."\\u016E")("ů"."\\u016F")("Ű"."\\u0170")("ű"."\\u0171")
       ("Ų"."\\u0172")("ų"."\\u0173")("Ŵ"."\\u0174")("ŵ"."\\u0175")
       ("Ŷ"."\\u0176")("ŷ"."\\u0177")("Ÿ"."\\u0178")("Ź"."\\u0179")
       ("ź"."\\u017A")("Ż"."\\u017B")("ż"."\\u017C")("Ž"."\\u017D")
       ("ž"."\\u017E")("ſ"."\\u017F")("ƀ"."\\u0180")("Ɓ"."\\u0181")
       ("Ƃ"."\\u0182")("ƃ"."\\u0183")("Ƅ"."\\u0184")("ƅ"."\\u0185")
       ("Ɔ"."\\u0186")("Ƈ"."\\u0187")("ƈ"."\\u0188")("Ɖ"."\\u0189")
       ("Ɗ"."\\u018A")("Ƌ"."\\u018B")("ƌ"."\\u018C")("ƍ"."\\u018D")
       ("Ǝ"."\\u018E")("Ə"."\\u018F")("Ɛ"."\\u0190")("Ƒ"."\\u0191")
       ("ƒ"."\\u0192")("Ɠ"."\\u0193")("Ɣ"."\\u0194")("ƕ"."\\u0195")
       ("Ɩ"."\\u0196")("Ɨ"."\\u0197")("Ƙ"."\\u0198")("ƙ"."\\u0199")
       ("ƚ"."\\u019A")("ƛ"."\\u019B")("Ɯ"."\\u019C")("Ɲ"."\\u019D")
       ("Ɵ"."\\u019F")("Ơ"."\\u01A0")("ơ"."\\u01A1")("Ƣ"."\\u01A2")
       ("ƣ"."\\u01A3")("Ƥ"."\\u01A4")("ƥ"."\\u01A5")("Ʀ"."\\u01A6")
       ("Ƨ"."\\u01A7")("ƨ"."\\u01A8")("Ʃ"."\\u01A9")("ƪ"."\\u01AA")
       ("ƫ"."\\u01AB")("Ƭ"."\\u01AC")("ƭ"."\\u01AD")("Ʈ"."\\u01AE")
       ("Ư"."\\u01AF")("ư"."\\u01B0")("Ʊ"."\\u01B1")("Ʋ"."\\u01B2")
       ("Ƴ"."\\u01B3")("ƴ"."\\u01B4")("Ƶ"."\\u01B5")("ƶ"."\\u01B6")
       ("Ʒ"."\\u01B7")("Ƹ"."\\u01B8")("ƹ"."\\u01B9")("ƻ"."\\u01BB")
       ("Ƽ"."\\u01BC")("ƽ"."\\u01BD")("ƾ"."\\u01BE")("ƿ"."\\u01BF")
       ("ǀ"."\\u01C0")("ǁ"."\\u01C1")("ǂ"."\\u01C2")("ǃ"."\\u01C3")
       ("Ǆ"."\\u01C4")("ǅ"."\\u01C5")("ǆ"."\\u01C6")("Ǉ"."\\u01C7")
       ("ǈ"."\\u01C8")("ǉ"."\\u01C9")("Ǌ"."\\u01CA")("ǋ"."\\u01CB")
       ("ǌ"."\\u01CC")("Ǎ"."\\u01CD")("ǎ"."\\u01CE")("Ǐ"."\\u01CF")
       ("ǐ"."\\u01D0")("Ǒ"."\\u01D1")("ǒ"."\\u01D2")("Ǔ"."\\u01D3")
       ("ǔ"."\\u01D4")("Ǖ"."\\u01D5")("ǖ"."\\u01D6")("Ǘ"."\\u01D7")
       ("ǘ"."\\u01D8")("Ǚ"."\\u01D9")("ǚ"."\\u01DA")("Ǜ"."\\u01DB")
       ("ǜ"."\\u01DC")("ǝ"."\\u01DD")("Ǟ"."\\u01DE")("ǟ"."\\u01DF")
       ("Ǡ"."\\u01E0")("ǡ"."\\u01E1")("Ǣ"."\\u01E2")("ǣ"."\\u01E3")
       ("Ǥ"."\\u01E4")("ǥ"."\\u01E5")("Ǧ"."\\u01E6")("ǧ"."\\u01E7")
       ("Ǩ"."\\u01E8")("ǩ"."\\u01E9")("Ǫ"."\\u01EA")("ǫ"."\\u01EB")
       ("Ǭ"."\\u01EC")("ǭ"."\\u01ED")("Ǯ"."\\u01EE")("ǯ"."\\u01EF")
       ("ǰ"."\\u01F0")("Ǳ"."\\u01F1")("ǳ"."\\u01F3")("Ƕ"."\\u01F6")
       ("Ƿ"."\\u01F7")("Ȝ"."\\u021C")("ȝ"."\\u021D")("Ȣ"."\\u0222")
       ("ȣ"."\\u0223")("ȷ"."\\u0237")("ȸ"."\\u0238")("ȹ"."\\u0239")
       ("Ɂ"."\\u0241")("ɂ"."\\u0242")("Ʉ"."\\u0244")("Ʌ"."\\u0245")
       ("ɐ"."\\u0250")("ɑ"."\\u0251")("ɒ"."\\u0252")("ɓ"."\\u0253")
       ("ɔ"."\\u0254")("ɕ"."\\u0255")("ɖ"."\\u0256")("ɗ"."\\u0257")
       ("ɘ"."\\u0258")("ə"."\\u0259")("ɚ"."\\u025A")("ɛ"."\\u025B")
       ("ɜ"."\\u025C")("ɝ"."\\u025D")("ɞ"."\\u025E")("ɟ"."\\u025F")
       ("ɠ"."\\u0260")("ɡ"."\\u0261")("ɢ"."\\u0262")("ɣ"."\\u0263")
       ("ɤ"."\\u0264")("ɥ"."\\u0265")("ɦ"."\\u0266")("ɧ"."\\u0267")
       ("ɨ"."\\u0268")("ɩ"."\\u0269")("ɪ"."\\u026A")("ɬ"."\\u026C")
       ("ɭ"."\\u026D")("ɮ"."\\u026E")("ɯ"."\\u026F")("ɱ"."\\u0271")
       ("ɲ"."\\u0272")("ɳ"."\\u0273")("ɴ"."\\u0274")("ɵ"."\\u0275")
       ("ɶ"."\\u0276")("ɷ"."\\u0277")("ɸ"."\\u0278")("ɹ"."\\u0279")
       ("ɻ"."\\u027B")("ɽ"."\\u027D")("ɾ"."\\u027E")("ɿ"."\\u027F")
       ("ʀ"."\\u0280")("ʁ"."\\u0281")("ʂ"."\\u0282")("ʃ"."\\u0283")
       ("ʄ"."\\u0284")("ʅ"."\\u0285")("ʆ"."\\u0286")("ʇ"."\\u0287")
       ("ʈ"."\\u0288")("ʉ"."\\u0289")("ʊ"."\\u028A")("ʋ"."\\u028B")
       ("ʌ"."\\u028C")("ʍ"."\\u028D")("ʎ"."\\u028E")("ʏ"."\\u028F")
       ("ʐ"."\\u0290")("ʑ"."\\u0291")("ʒ"."\\u0292")("ʓ"."\\u0293")
       ("ʔ"."\\u0294")("ʕ"."\\u0295")("ʖ"."\\u0296")("ʗ"."\\u0297")
       ("ʘ"."\\u0298")("ʙ"."\\u0299")("ʚ"."\\u029A")("ʛ"."\\u029B")
       ("ʜ"."\\u029C")("ʝ"."\\u029D")("ʞ"."\\u029E")("ʟ"."\\u029F")
       ("ʠ"."\\u02A0")("ʡ"."\\u02A1")("ʢ"."\\u02A2")("ʣ"."\\u02A3")
       ("ʤ"."\\u02A4")("ʥ"."\\u02A5")("ʦ"."\\u02A6")("ʧ"."\\u02A7")
       ("ʨ"."\\u02A8")("ʩ"."\\u02A9")("ʪ"."\\u02AA")("ʫ"."\\u02AB")
       ("ʬ"."\\u02AC")("ʭ"."\\u02AD")("ș"."\\u0219")("ț"."\\u021b")
       ("’"."\\u2019")("'"."\\u0027")("\""."\\u0022"))
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

;;======================================================================
;; get the font information for the text under the cursor
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
