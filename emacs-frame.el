;;;_.======================================================================
;;;_. set the frame variables and support functions
(require 'cm-frame)
; is there a secondary monitor to the right of the primary?
(setq cmframe-monitor2-p 'nil)

; if so, set its dimensions here
;(setq cmframe-monitor2-width 1280)
;(setq cmframe-monitor2-height 1024)

; set the right margin (add window manager space here as well)
(setq cmframe-right-margin 15)

(provide 'emacs-frame)

