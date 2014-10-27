;;;_* -*-mode: emacs-lisp -*-

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
;; macro by typing;;; M-x <macro_name>;;;
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
;; TrueCrypt shortcuts
(defun tcmount ()
  "Mount the truecrypt favorite volumes"
  (interactive)
  (shell-command
   (concat "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
	   " --auto-mount=favorites"
	   " --password=\"" (password-read-and-add "Password: " "truecrypt") "\"")))

(defun tcmount1 ()
  "Mount the truecrypt volume JAVA_LIB01 from the Passport2TB"
  (interactive)
  (shell-command
   (concat "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
           " /Volumes/MyBook3TB/Java/3tb-java_lib01 /Volumes/3TB_JAVA1"
	   " --password=\"" (password-read-and-add "Password: " "truecrypt") "\"")))

(defun tcmount2 ()
  "Mount the truecrypt volume JAVA_LIB02 from Passport2TB"
  (interactive) 
  (shell-command
   (concat "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
           " /Volumes/MyBook3TB/Java/3tb-java_lib02 /Volumes/3TB_JAVA2"
	   " --password=\"" (password-read-and-add "Password: " "truecrypt") "\"")))

(defun tcdmount ()
  "Unmount the truecrypt favorite volumes"
  (interactive)
  (shell-command
   "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt --dismount")
  (message "Drives Dismounted"))

(defun tclist ()
  "List the mounted TrueCrypt directories"
  (interactive)
  (shell-command
   (concat "/Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt"
           " --list")))


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
