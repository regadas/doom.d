;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! bazel)
(package! reformatter)
(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el")))
(package! modus-themes)
(package! k8s-mode)
(package! kubel)
(package! ob-http)
(package! mermaid-mode)
(package! ob-mermaid)
(package! protobuf-mode)
(package! org-cv
  :recipe (:host gitlab
           :repo "Titan-C/org-cv"))
(package! sql-trino)
(package! sql-bigquery)
(package! zoom)
(package! d2-mode)
(package! just-mode)
(package! ox-gfm)
(package! consult-gh)
(package! pr-review)

(package! nyan-mode)

(package! rainbow-csv
  :recipe (:host github
           :repo "emacs-vs/rainbow-csv"
           :files ("*.el")))

(package! shell-maker)
(package! acp)
(package! agent-shell)
(package! agent-shell-macext
  :recipe (:host github
           :repo "cxa/agent-shell-macext"
           :files ("*.el")))
(package! agent-review
  :recipe (:host github
           :repo "nineluj/agent-review"
           :files ("*.el")))

(unpin! magit)
(unpin! transient)
(unpin! lsp-mode)
(unpin! (:term ghostel))
