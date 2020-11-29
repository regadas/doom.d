(put 'projectile-grep 'disabled nil)

(auto-save-visited-mode +1)

(setq-default TeX-engine 'xetex
              pdf-latex-command "xelatex")

(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
