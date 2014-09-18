;;======================================================================
;; mode-line modifications

;; set the modeline to display path and filename
(defun long-file-name ()
  "Display the full file path and name in the modeline"
  (interactive "*")
  (setq-default mode-line-buffer-identification
    '("%S:"(buffer-file-name "%f"))))

;; set the modeline to display filename only
(defun short-file-name ()
  "Display the file name without path in the modeline"
  (interactive "*")
  (setq-default mode-line-buffer-identification '("%12b")))

;; turn on column line mode
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

;; minimize extraneous info
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'elisp-slime-nav-mode)
(diminish 'magit-auto-revert-mode)
(diminish 'smartparens-mode)
(diminish 'auto-complete-mode)

;; display the function the point is in within the modeline if any
(setq which-func-unknown "")
(which-function-mode)

(provide 'emacs-modeline)
