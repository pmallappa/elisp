;;;_.============================================================
;;;_. Setup Calc mode

;; load the calculator unit conversion functions without having to call calc (useful in org-mode tables)
(require 'calc-ext)
(require 'calc-macs)
(eval-when-compile
  (require 'calc-alg))


(provide 'emacs-calc)
