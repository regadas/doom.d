;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! nyan-mode)
(package! bazel)
(package! copilot
  :pin "581cadd6f4229223fd5c57984f9595aeb86c84f7"
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el" "dist")))
(package! dimmer)
(package! ef-themes)
(package! modus-themes)
(package! catppuccin-theme)
(package! imenu-list)
(package! jest-test-mode
  :recipe (:host github
           :repo "regadas/jest-test-mode"))
(package! k8s-mode)
(package! kubel)
(package! magit-delta)
(package! ob-http)
(package! mermaid-mode)
(package! ob-mermaid)
(package! ob-sql-mode)
(package! org-cv
  :recipe (:host gitlab
           :repo "Titan-C/org-cv"))
(package! protobuf-mode)
(package! sql-trino)
(package! undo-hl
  :recipe (:host github
           :repo "casouri/undo-hl"))
(package! counsel-jq)
(package! zoom)
(package! d2-mode)
(package! sqlformat)
(package! eglot-java)
(package! gptel)
(package! lsp-tailwindcss
   :recipe (:host github
            :repo "merrickluo/lsp-tailwindcss"))
(package! lsp-ltex)
(package! ellama)
(package! sql-bigquery
   :recipe (:host github
            :repo "regadas/sql-bigquery"))
(package! treesit-auto)
(package! svg-tag-mode)
(package! ox-gfm)
(package! silicon-el
   :recipe (:host github
            :repo "iensu/silicon-el"))
