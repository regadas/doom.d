;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! bazel)
(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el")))
(package! ef-themes)
(package! modus-themes)
(package! catppuccin-theme)
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
(package! pr-inbox
  :recipe (:local-repo "lisp/pr-inbox"
           :build (:not compile)))
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
(package! ghostel
  :recipe (:host github
           :repo "dakra/ghostel"
           :files (:defaults
                   "etc"
                   "src"
                   "vendor"
                   "build.zig"
                   "build.zig.zon"
                   "symbols.map")))
(package! evil-ghostel
  :recipe (:host github
           :repo "dakra/ghostel"
           :files ("extensions/evil-ghostel/*.el")))

(package! consult-gh)
(package! pr-review)

(package! nyan-mode)

(package! shell-maker)
(package! acp)
(package! agent-shell)
(package! agent-review
  :recipe (:host github
           :repo "nineluj/agent-review"
           :files ("*.el")))

(unpin! magit)
(unpin! transient)
(unpin! lsp-mode)
