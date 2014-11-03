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
;; Replaced by OSX native encrypted disk image capabilities
;; Good explanation at
;; http://www.theinstructional.com/guides/disk-management-from-the-command-line-part-3
;;
;; example command-lines
;;
;; Create new encrypted disk image
;; hdiutil create -size {size} -encryption {aes-128|aes-256} -volname {Name of volume} -fs {filesystem} {filename.dmg}
;; so 
;; hdiutil create -size 150g -encryption aes-128 -volname JAVA_LIB -fs HFS+ java_lib.dmg 
;;
;; mount the encrypted disk image with a password prompt from the shell
;; hdiutil attach /path/to/image.dmg
;; so
;; hdiutil attach -stdinpass /Volumes/Passport2TB/Java.dmg
;;
;; unmount
;; hdiutil detach /Volumes/Passport2TB/Java_Lib
;;
;; increase disk size
;; hdiutil resize -size {new size} {imagename}.dmg
;; so to increase from 40 to 60g
;; hdiutil resize -size 60g -stdinpass /Volumes/Passport2TB/Java.dmg

(defun hdi-mount (dmgfile)
  "Mount the dmgfile using hdiutil"
   (shell-command
    (concat "hdiutil attach " dmgfile)))
(defun hdi-dmount (volume)
  "Unmount the volume using hdiutil"
   (shell-command
    (concat "hdiutil detach " volume)))
(defun jmount ()
  "Mount the JAVA_LIB disk"
  (interactive)
  (hdi-mount "/Volumes/Passport2TB/Java/java_lib.dmg"))
(defun jdmount ()
  "Unmount the JAVA_LIB disk"
  (interactive)
  (hdi-dmount "/Volumes/JAVA_LIB"))
(defun jmount2 ()
  "Mount the JAVA_LIB2 disk"
  (interactive)
  (hdi-mount "/Volumes/MyBook3TB/Java/java_lib.dmg"))
(defun jdmount2 ()
  "Unmount the JAVA_LIB2 disk"
  (interactive)
  (hdi-dmount "/Volumes/JAVA_LIB2"))

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
