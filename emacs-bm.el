;;======================================================================
;; emacs bookmark enhancements
;;======================================================================

(setq bookmark-default-file (concat EMACS_CONFIGS "/emacs.bmk"))

(require 'bookmark+)

;; always start with the same file, not the last one used
(setq bmkp-last-as-first-bookmark-file nil)

;; speed things up
(setq bmkp-propertize-bookmark-names-flag nil)
(setq bmkp-bmenu-state-file nil)
(setq bmkp-bmenu-commands-file nil)

;(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
;(autoload 'bm-next     "bm" "Goto bookmark."                     t)
;(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(provide 'emacs-bm)
