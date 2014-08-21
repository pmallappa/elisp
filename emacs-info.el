;;; emacs-info.el --- 

(require 'info)

;;;_.======================================================================
;;;_. Info mode settings
;; point the packages to the appropriate info directory
;(setq Info-default-directory-list 
;      (list
;       (concat EMACS_CONFIGS "/info")
;       "/Applications/Emacs.app/Contents/Resources/info"
;       (concat EMACS_CONFIGS "/info")
;       (concat EMACS_CONFIGS "/info/elisp")
;       (concat EMACS_CONFIGS "/info/gnus")
;       (concat EMACS_DIR  "/info")
;       (concat CYGWIN_DIR "/usr/share/info")
;       (concat CYGWIN_DIR "/usr/info")
;       ))

(cond ((eq system-type 'windows-nt)
      (add-to-list  'Info-default-directory-list "c:/cygwin/usr/share/info")))
(cond ((eq system-type 'darwin)
      (add-to-list  'Info-default-directory-list "/usr/share/info")))

(setq Info-directory-list Info-default-directory-list)

;;============================================================
;; display Info mode buffers in proportional font
;; http://yoo2080.wordpress.com/2013/05/30/monospace-font-in-tables-and-source-code-blocks-in-org-mode-proportional-font-in-other-parts/

(add-hook 'Info-mode-hook 'variable-pitch-mode)

;;; but code examples in monospace font
(defvar my-rx-info-code (rx bol "     " (* not-newline) eol))
(add-hook 'Info-mode-hook 'my-Info-font-lock)
(defun my-Info-font-lock ()
  (interactive)
  (require 'org)
  (font-lock-add-keywords
   nil
   `((,my-rx-info-code
      .
      ;; let's just use org-block
      (quote org-block)
      ))))


(provide 'emacs-info)
