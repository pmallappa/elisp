;; fun using lisp from within emacs functions
;; http://www.masteringemacs.org/articles/2012/04/25/fun-emacs-calc/

;;============================================================
;; call calc to return the factorial of a number
;; need calc because the result can easily exceed the max integer
;; value, even for a 64-bit system.
(defun factorial (n)
  (string-to-number (factorial--1 n)))

(defun factorial--1 (n)
  (if (<= n 1)
      "1"
    (calc-eval (format "%s * %s"
                       (number-to-string n)
                       (factorial--1 (- n 1))))))

(defun deg2dec (deg min sec)
  "Convert degrees, minutes, seconds to decimal equivilent"
  (interactive "nDeg: 
nMin: 
nSec: ")
  (let ((s (concat (calc-eval (format "deg(%s@ %s' %s\")" deg min sec)))))
           (message (concat "DEG:: "
                            (int-to-string deg) "d "
                            (int-to-string min) "' "
                            (int-to-string sec) "\"    Decimal: " s))
           ; place into the kill ring for pasting
           (kill-new s)))

;;============================================================
;; example of a loop in emacs
(defun a-to-z ()
  (interactive)
  (dotimes (c 26)
    (insert (format "(global-set-key (kbd \"<menu> g %c\") \"%c\")\n"
                    (+ ?a c) (+ ?A c)))))
