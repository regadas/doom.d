;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! bazel)
(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el")))
(package! ef-themes)
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
(package! undo-hl
  :recipe (:host github
           :repo "casouri/undo-hl"))
(package! counsel-jq)
(package! zoom)
(package! d2-mode)
(package! just-mode)
(package! ox-gfm)
(package! claude-code-ide
  :recipe (:host github
           :repo "manzaltu/claude-code-ide.el"))
(package! vterm-anti-flicker-filter
  :recipe (:host github
           :repo "martinbaillie/vterm-anti-flicker-filter"))

(unpin! magit)
(unpin! transient)
