;; ======================================================================
;; Frame sizing and movement

(require 'frame-cmds)

(defun cm-restore-frame ()
  "Restore frame to default width and height"
  (interactive)
  (set-frame-width (selected-frame)
                   MY_DEFAULT_WIDTH)
  (maximize-frame-vertically (selected-frame)))

(defun cm-move-frame-to-screen-right ()
  "hydra-capable version of `move-frame-to-screen-left'"
  (interactive)
  (move-frame-to-screen-right 0 (selected-frame)))

(defun cm-move-frame-to-screen-left ()
  "hydra-capable version of `move-frame-to-screen-left'"
  (interactive)
  (move-frame-to-screen-left 0 (selected-frame)))

;; To get information on multi-displays, look into the functions
;; (display-monitor-attributes-list)
;; Here's the results at work (1366x768 _>)
;; In this windows configuration the commands to move right and left are not working
;; (((geometry 0 0 1920 1080) (workarea 0 0 1920 1050) (mm-size 677 381) (name . "\\\\.\\DISPLAY1") (frames #<frame emacs on PL1USMIF0388NB -- c:/cygwin/home/ndr5mz/elisp/emacs-frame.el 0000000001CD1C48>)) ((geometry -1366 312 1366 768) (workarea -1366 312 1366 768) (mm-size 482 271) (name . "\\\\.\\DISPLAY2") (frames)) ((geometry 1920 0 1920 1080) (workarea 1920 0 1920 1080) (mm-size 677 381) (name . "\\\\.\\DISPLAY3") (frames)))
;; (frame-monitor-attributes)


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
;; misc settings
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/= (cadr (frame-parameter nil 'alpha)) 100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 20)))) ;focused / background

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 (transparent) to 100 (opaque): ")
  (set-frame-parameter (selected-frame) 'alpha value))

;; adjust the frame to fit the current resolution on launching
(add-hook 'window-setup-hook 'cm-restore-frame)
(add-hook 'window-setup-hook 'cm-move-frame-to-screen-right)


(provide 'emacs-frame)
