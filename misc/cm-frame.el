;;======================================================================
;; set the frame variables and support functions

(defun cm-display-pixel-width ()
  "Return width of current display"
  ; emacs 24.4 changed the return value of display-pixel-width
  (if (and (>= emacs-major-version 24)
           (>= emacs-minor-version 4))
      (nth 3 (assq 'geometry (frame-monitor-attributes)))
    (display-pixel-width)))

(defvar cmframe-monitor2-p 'nil
  "Indicates whether a secondary monitor is active to the right
of the primary monitor. Set to 't to indicate a monitor, 'nil otherwise")

(defvar cmframe-monitor2-width (cm-display-pixel-width)
  "Width (in pixels) of the secondary monitor on a two-monitor
display, set to the right of the primary display. The
dimensions of the primary monitor (on the left) are retrieved
programmatically, and so don't need to be manually set

If the variable `cmframe-monitor2-p' is nil, this value is ignored.
The default value is the same as the primary monitor")

(defvar cmframe-monitor2-height (display-pixel-height)
  "Height (in pixels) of the secondary monitor on a two-monitor
display set to the right of the primary display The dimensions
of the primary monitor (on the left) are retrieved
programmatically, and so don't need to be manually set

If the variable `cmframe-monitor2-p' is nil, this value is ignored.
The default value is the same as the primary monitor")

(defvar cmframe-windowmgr-offset 30
  "Arbitrary offset in pixels to allow for the Window Manager title height")

(defvar cmframe-top-margin 15
  "Number of pixels to place the frame from the top of the screen")

(defvar cmframe-horizontal-margin 15
  "Number of pixels to place the frame from the sides of the screen")

(defvar right-monitor-p nil
  "Flag whether frame is in the right monitor")

(defvar enlarged-p nil
  "Flag whether frame is enlarged")

(defun set-monitor-right()
  (setq right-monitor-p 't))

(defun set-monitor-left()
  (setq right-monitor-p 'nil))

(defun is-right-monitor()
  "Return true if the frame is in the right monitor"
  (eq right-monitor-p 't))

(defun set-enlarged-on()
  "Set the status of the current frame to 'enlarged'"
  (setq enlarged-p 't))

(defun set-enlarged-off()
  "Set the status of the current frame to 'standard'"
  (setq enlarged-p 'nil))

(defun is-enlarged()
  "Return true if the frame is enlarged, nil if it's a standard size"
  (eq enlarged-p 't))

(defun get-frame-width(screen-width)
  "Return the number of columns for a frame in a given screen width. 
Final frame size is determined by the value returned by
`is-enlarged', which shows whether a large or standard frame is
desired
If the size of the frame exceeds the screen width, shrink to fit the screen"
  (if (is-enlarged)
      (+ 1 (pixels-to-cols (- screen-width (* 2 cmframe-horizontal-margin))))

    ; If user-defined default width is too wide, fit within the monitor
    (min 
     MY_DEFAULT_WIDTH 
     (pixels-to-cols (- screen-width (* 2 cmframe-horizontal-margin))))))

(defun get-frame-height(screen-height)
  "Return the number of rows for a frame in a given screen height"
  (pixels-to-rows (- screen-height (+ cmframe-windowmgr-offset cmframe-top-margin))))

(defun pixels-to-rows(pixels)
  "Return the frame pixel width for the given number of columns"
  (/ pixels (frame-char-height)))

(defun rows-to-pixels(rows)
  "Return the frame pixel height for the given number of rows"
  (* rows (frame-char-height)))

(defun pixels-to-cols(pixels)
  "Return the number of columns for a given frame pixel width"
  ; subtract 1 column to account for window frame width
  (- (/ pixels (frame-char-width)) 1))

(defun cols-to-pixels(columns)
  "Return the frame pixel width for the given number of columns"
  ; take the fringe on either side into account
  (* (+ 2 columns) (frame-char-width)))

(defun cmframe-frame-adjust()
  "Resize the frame size to an enlarged or standard size, based
on the status returned by `is-enlarged', and move to the screen
determined by `is-right-monitor`"
  (interactive)

  ;; ensure the frame is not currently fullscreen or maximized
  (if (eq (frame-parameter nil 'fullscreen) 'fullscreen)
      (toggle-frame-fullscreen))
  
  (if (eq (frame-parameter nil 'fullscreen) 'maximized)
      (toggle-frame-maximized))

  ;; determine and set the frame size
  (set-frame-size (selected-frame)
                  (get-frame-width 
                   (if (is-right-monitor)
                       cmframe-monitor2-width
                     (cm-display-pixel-width)))
                  (get-frame-height
                   (if (is-right-monitor)
                       cmframe-monitor2-height
                     (display-pixel-height))))

  (set-frame-position (selected-frame)
                      (max 0 (frame-position-left))
                      cmframe-top-margin))

(defun frame-position-left ()
  (-
      (if (is-right-monitor)
          (+ (cm-display-pixel-width)
             (- cmframe-monitor2-width 
                (cols-to-pixels (get-frame-width cmframe-monitor2-width))))
        ;; left monitor
        (- (cm-display-pixel-width)
           (cols-to-pixels (get-frame-width (cm-display-pixel-width)))))
      cmframe-horizontal-margin))

;;;_.======================================================================
;;;_. define the interactive frame functions
(defun cmframe-frame-enlarge()
  "Adjust the frame size to fill the current screen"
  (interactive)
  (set-enlarged-on)
  (cmframe-frame-adjust))

(defun cmframe-toggle-frame-enlarge()
  "Toggle frame state from enlarged to standard"
  (interactive)
  (if (is-enlarged)
      (cmframe-frame-shrink)
    (cmframe-frame-enlarge)))

(defun cmframe-frame-shrink()
  "Adjust the frame size to a width of `MY_DEFAULT_WIDTH' and
move to the right side of the screen"
  (interactive)
  (set-enlarged-off)
  (cmframe-frame-adjust))

(defun cmframe-right()
  "Move the frame to the right monitor while maintaining its size
relative to the screen.

If the variable `cmframe-monitor2-p' is nil, the frame will remain on the left."
  (interactive)
  (if (eq cmframe-monitor2-p t)
      (progn 
      (set-monitor-right)
      (cmframe-frame-adjust))
    (progn
      (message "No Secondary Monitor")
      (sit-for 3)
      (message ""))))

(defun cmframe-left()
  "Move the frame to the left monitor while maintaining 
its size (relative to the screen)"
  (interactive)
  (set-monitor-left)
  (cmframe-frame-adjust))

;;;_.======================================================================
;;;_. function to set current font and resize frame
(defun set-font(&optional arg)
  "Select the default font for this frame
With prefix argument ARG, include proportional fonts"
  (interactive)
  (menu-set-font)
  (cmframe-frame-adjust))

;;;_.======================================================================
;;;_. Frame max toggle - From: "rgb" <rbielaws@i1.net> /
;;   gnu.emacs.help / 18 Mar 2005 16:30:32 -0800
(make-variable-frame-local 'my-frame-state)

(defun cmframe-frame-maximize ()
  "Maximize Emacs window"
  (interactive)
  (modify-frame-parameters nil '((my-frame-state . 't)))
  (toggle-frame-maximized))

(defun cmframe-frame-restore ()
   "Restore Emacs window in Win32"
   (interactive)
   (modify-frame-parameters nil '((my-frame-state . nil)))
   (if (eq system-type 'darwin)
       (cmframe-toggle-frame-maximize)
     (w32-send-sys-command ?\xF120)))

(defun cmframe-toggle-frame-maximize ()
  "Maximize/Restore Emacs frame based on `my-frame-state'"
  (interactive)
  (if (cdr (assoc 'my-frame-state (frame-parameters)))
	  (cmframe-frame-restore)
    (cmframe-frame-maximize)))

;;;_.======================================================================
;;;_. Toggle 2 windows between vertical and horizontal split
;;From: "Fabrice Niessen" <tdhkhcidiacy@spammotel.com>
;;Subject: Re: Toggle between Vertical and Horizontal Windows Splitting
;;Newsgroups: gnu.emacs.help
;;Date: Fri, 14 Nov 2008 14:54:46 +0100
(defun cmframe-toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."

  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;;; ======================================================================
;;; set the keys for the frame functions
;(global-set-key (kbd "C-c f a") 'cmframe-frame-adjust)
;(global-set-key (kbd "C-c f s") 'cmframe-frame-shrink)
;(global-set-key (kbd "C-c f e") 'cmframe-toggle-frame-enlarge)
;(global-set-key (kbd "C-c f m") 'cmframe-toggle-frame-maximize)
;(global-set-key (kbd "C-c f t") 'cmframe-toggle-window-split)
;(if (and (>= emacs-major-version 24)
;         (>= emacs-minor-version 4))
;    (global-set-key (kbd "C-c f f") 'toggle-frame-fullscreen)
;  (global-set-key (kbd "C-c f f") 'cmframe-toggle-frame-maximize))

(provide 'cm-frame)

;;;_.======================================================================
;;;_. Local variables
;;Local Variables:
;;indent-tabs-mode: nil
;;allout-layout: (-1 : 0)
;;End:
