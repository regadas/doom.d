;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Filipe Regadas"
      user-mail-address "oss@regadas.email")

(setq gc-cons-threshold most-positive-fixnum)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'catppuccin
      doom-font (font-spec :family "Iosevka" :size 16 :weight 'Medium))

(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(setq-default line-spacing 3)

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"               ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always  ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

;; Prevents some cases of Emacs flickering
;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative
      scroll-conservatively 101)

(auto-save-visited-mode +1)

;; highlight undoed text
(use-package! undo-hl
  :hook ((text-mode . undo-hl-mode)
         (prog-mode . undo-hl-mode)))

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! (:when (featurep! :tools lookup)
       :nv "gh"   #'+lookup/documentation
       ))

(after! evil
  (setq-default evil-escape-key-sequence "jj"))

(after! treemacs
  (setq treemacs-git-mode 'deferred
        treemacs-collapse-dirs 20))

(after! projectile
  (setq projectile-project-search-path '("~/projects/" "~/projects/spotify" "~/projects/experiments")
        projectile-project-root-files-bottom-up '(".projectile" ".git")))

(setq-default TeX-engine 'xetex
              pdf-latex-command "xelatex")

(use-package! kubernetes
  :init (progn
          (setq kubernetes-overview-custom-views-alist '((overview . (context statefulsets deployments services pods)))))
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes)

(after! go-mode
  (if (featurep! +lsp)
      (add-hook 'go-mode-hook #'lsp-deferred)
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)))

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-regexps
               '("^ghe\\.spotify\\.net$" . "github")))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
(after! lsp-mode
  ;; Disable invasive lsp-mode features
  (setq
   lsp-ui-sideline-enable t   ; not anymore useful than flycheck
   lsp-ui-doc-enable nil        ; slow and redundant with K

   lsp-ui-sideline-update-mode 'point
   lsp-inhibit-message t

   lsp-java-vmargs '("-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-Xmx8G" "-Xms1G")
   lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/1.12.0/jdt-language-server-1.12.0-202206011637.tar.gz")

  (with-eval-after-load 'lsp-rust
    (require 'dap-cpptools))

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.codelenses" '(("test" . t)) t)
     ("gopls.staticcheck" t t)))

  (setq ;; lsp-rust-analyzer-server-display-inlay-hints t
   ;; lsp-metals-show-inferred-type t
   lsp-lens-enable t
   lsp-bash-highlight-parsing-errors t
   +lsp-company-backends '(:separate company-capf company-yasnippet)))

(defadvice! +lsp--fix-indent-width-in-web-mode-a (orig-fn mode)
  :around #'lsp--get-indent-width
  (if (provided-mode-derived-p mode 'web-mode)
      'tab-width
    (funcall orig-fn mode)))

(use-package! ox-awesomecv
  :after org)

(after! org
  (setq org-latex-compiler "xelatex"))

(after! bazel
  (setq bazel-buildifier-before-save t))

;; dhall-mode highlight the syntax and run dhall format on save
(use-package! dhall-mode
  :defer t
  :init
  (add-hook 'dhall-mode-local-vars-hook #'lsp! 'append)
  :config
  (setq
   dhall-format-at-save (featurep! :editor format +onsave)
   ;; uncomment the next line to disable automatic format
   ;; dhall-format-at-save nil

   ;; comment the next line to use unicode syntax
   dhall-format-arguments (\` ("--ascii"))

   ;; header-line is obsoleted by lsp-mode
   dhall-use-header-line nil)

  (set-repl-handler! 'dhall-mode #'dhall-repl-show)

  (reformatter-define dhall-freeze-all
    :program dhall-command
    :args '("freeze" "--all")
    :group 'dhall
    :lighter " DhFreezeAll")
  (map! :after dhall-mode
        :map dhall-mode-map
        :localleader
        "l" #'dhall-lint
        "ff" #'dhall-freeze
        "fa" #'dhall-freeze-all
        "t" #'dhall-buffer-type-show))

(use-package! dimmer
  :hook (prog-mode . dimmer-mode)
  :config
  (dimmer-configure-company-box)
  (dimmer-configure-org)
  (dimmer-configure-magit)
  (dimmer-configure-which-key)
  (setq dimmer-fraction 0.3))

;; copilot.el
;; accept completion from copilot and fallback to company
(defun custom-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("<tab>" . 'custom-tab)
         ("TAB" . 'custom-tab)
         :map company-mode-map
         ("<tab>" . 'custom-tab)
         ("TAB" . 'custom-tab)))

;;tree-sitter
(use-package! tree-sitter
  :when (bound-and-true-p module-file-suffix)
  :hook (prog-mode . tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)
  (defadvice! doom-tree-sitter-fail-gracefully-a (orig-fn &rest args)
    "Don't break with errors when current major mode lacks tree-sitter support."
    :around #'tree-sitter-mode
    (condition-case e
        (apply orig-fn args)
      (error
       (unless (string-match-p (concat "^Cannot find shared library\\|"
                                       "^No language registered\\|"
                                       "cannot open shared object file")
                               (error-message-string e))
         (signal (car e) (cadr e)))))))

(use-package! ob-sql-mode
  :after org)
