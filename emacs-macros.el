;;;_* -*-mode: emacs-lisp -*-

;;======================================================================
;; WGUC Stream access
(defun wguc ()
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "open" 
                       (concat (getenv "ProgramFiles(x86)") "\\VideoLAN\\VLC\\vlc.exe")
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
;; OSX native encrypted disk
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

(if (eq system-type 'darwin)
    (progn
      (defun hdi-mount (dmgfile)
        "Mount the dmgfile using hdiutil"
        (shell-command
         (concat "hdiutil attach " dmgfile)))
      (defun hdi-dmount (volume)
        "Unmount the volume using hdiutil"
        (shell-command
         (concat "hdiutil detach " volume)))))

(provide 'emacs-macros)

;;;_*======================================================================
;;;_* Local variables
;;Local Variables:
;;indent-tabs-mode: nil
;;allout-layout: (-1 : 0)
;;End:
