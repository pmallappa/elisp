;	@printf "Generating magit-pkg.el\n"
;	@printf "(define-package \"magit\" \""$(VERSION)"\"\n" > $@
;	@printf "  \"Control Git from Emacs.\"\n"      >> $@
;	@printf "  '((cl-lib \"0.3\")\n"               >> $@
;	@printf "    (git-commit-mode \"0.14.0\")\n"   >> $@
;	@printf "    (git-rebase-mode \"0.14.0\")))\n" >> $@

(define-package "magit" "1.9.0"
  "Control Git from Emacs."
  '((cl-lib "0.3")
    (git-commit-mode "0.14.0")
    (git-rebase-mode "0.14.0")))
