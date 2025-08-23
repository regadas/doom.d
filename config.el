;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; (add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
;; (add-to-list 'default-frame-alist '(undecorated-round . t))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Filipe Regadas"
      user-mail-address "oss@regadas.email")

(setq auth-sources '("~/.authinfo.gpg"))

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
(setq doom-theme 'modus-operandi
      ;; doom-font (font-spec :family "TX-02" :size 16))
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))

(setq mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t)

(after! modus-themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-fringes 'subtle
        modus-themes-prompts '(extrabold italic)))

(setq inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"               ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil)                   ; I can trust my computers ... can't I?

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/brain-dump")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(after! magit
  (setq magit-diff-refine-hunk t
        magit-format-file-function #'magit-format-file-nerd-icons))

(use-package! zoom
  ;; :hook (doom-first-input . zoom-mode)
  :config
  (setq zoom-size '(0.678 . 0.678)))

(after! embark
  (set-popup-rule! "^\\*Embark Export" :ignore t))

(after! vterm
  (setq vterm-max-scrollback 10000
        vterm-timer-delay 0.03)
  (define-key vterm-mode-map [deletechar] #'vterm-send-delete))

;; highlight undoed text
;; (use-package! undo-hl
;;   :hook ((text-mode . undo-hl-mode)
;;          (prog-mode . undo-hl-mode)))

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(after! dap-mode
  (setq dap-java--var-format "\"$%s\""))

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

(map! (:when (modulep! :tools lookup)
        :nv "gh"   #'+lookup/documentation
        ))

(after! evil
  (setq-default evil-escape-key-sequence "jj"))

(after! projectile
  (setq projectile-project-search-path '("~/projects/" "~/projects/spotify" "~/projects/experiments")
        projectile-project-root-files-bottom-up '(".projectile" ".git")
        projectile-enable-caching t))

;; (setq-default TeX-engine 'xetex
;;               pdf-latex-command "xelatex")

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^ghe\\.spotify\\.net$" :type "github") 'append))

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
  (add-hook! 'dhall-mode-local-vars-hook #'lsp! 'append)
  :config
  (set-repl-handler! 'dhall-mode #'dhall-repl-show)
  (reformatter-define dhall-freeze-all
    :program dhall-command
    :args '("freeze" "--all")
    :lighter " DhFreezeAll")
  (map! :map dhall-mode-map
        :localleader
        "l" #'dhall-lint-buffer
        "ff" #'dhall-freeze-buffer
        "fa" #'dhall-freeze-all-buffer
        "t" #'dhall-buffer-type-show))

(use-package! sql-bigquery
  :after ob-sql-mode)

(use-package! ob-sql-mode
  :after org)

(use-package! jest-test-mode
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(map! :after jest-test-mode
      :map jest-test-mode-map
      :localleader
      :prefix "t"
      "a" #'jest-test-run
      "t" #'jest-test-run-at-point)

(use-package! kubel
  :after (vterm)
  :config (kubel-vterm-setup))

(after! sql
  ;; set formatter to sql-formatter
  (set-formatter!
    'sql-formatter
    '("sql-formatter")
    :modes '(sql-mode)))

;; (add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))
(after! d2-mode
  (reformatter-define d2-format
    :program "d2"
    :stdin nil
    :stdout nil
    :args (list "fmt" input-file)
    :lighter " D2Fmt"))

(after! lsp-mode
  ;; Disable invasive lsp-mode features
  (setq lsp-lens-enable nil
        ;; lsp-copilot-enabled t
        lsp-use-plists t
        lsp-log-io nil
        lsp-auto-guess-root t
        lsp-enable-file-watchers nil
        lsp-idle-delay 0.25                      ;; Increased from 0.25 for better performance
        lsp-response-timeout 10
        lsp-headerline-breadcrumb-enable nil    ;; Disable breadcrumbs (heavy UI)
        lsp-headerline-breadcrumb-segments nil  ;; Completely disable breadcrumb segments
        lsp-enable-symbol-highlighting t      ;; Disable symbol highlighting (heavy)
        lsp-enable-on-type-formatting nil       ;; Disable real-time formatting
        lsp-enable-folding nil                  ;; Disable code folding
        lsp-semantic-tokens-enable nil          ;; Disable semantic tokens (very heavy)
        lsp-enable-text-document-color nil      ;; Disable color decorations
        lsp-enable-links nil                     ;; Disable link navigation
        lsp-eldoc-enable-hover nil              ;; Disable eldoc hover
        lsp-signature-render-documentation nil  ;; Don't render docs in signature
        ;; lsp-completion-provider :none
        lsp-keep-workspace-alive nil

        lsp-java-completion-max-results 20
        lsp-java-maven-download-sources t
        ;; lsp-java-completion-guess-method-arguments nil
        lsp-java-compile-null-analysis-mode "automatic"
        lsp-java-vmargs '(
                          "-Xmx16G"
                          "-XX:+UseParallelGC"
                          "-XX:GCTimeRatio=4"
                          "-XX:AdaptiveSizePolicyWeight=90"
                          "-Dsun.zip.disableMemoryMapping=true"
                          "-Xlog:disable"
                          )
        lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.48.0/jdt-language-server-1.48.0-202506271502.tar.gz"
        lsp-java-jdt-ls-android-support-enabled nil

        ;; Performance optimizations for Java
        lsp-java-autobuild-enabled t         ;; disable auto-build for better performance
        ;; lsp-java-folding-range-enabled nil     ;; disable code folding calculation
        lsp-java-import-gradle-enabled t
        ;; lsp-java-selection-enabled nil         ;; disable selection ranges
        lsp-java-trace-server "off"            ;; disable server tracing
        lsp-java-references-code-lens-enabled nil ;; disable references code lens
        lsp-java-format-enabled nil            ;; disable formatting if using external formatter
        lsp-java-signature-help-enabled nil    ;; disable signature help popup

        lsp-java-progress-reports-enabled nil
        lsp-ui-sideline-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-workspace-status-enable nil

        lsp-bash-highlight-parsing-errors t
        lsp-signature-auto-activate nil
        
        ;; Additional performance optimizations
        lsp-enable-imenu nil                    ;; Use native imenu instead
        lsp-enable-indentation nil               ;; Use Emacs indentation

        ;; UI optimizations
        lsp-progress-via-spinner nil            ;; Disable progress spinner
        lsp-eldoc-render-all nil))              ;; Don't render all eldoc

(after! java-mode
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil))

(after! dired
  (setq delete-by-moving-to-trash t
        dired-listing-switches "-lat")  ; sort by date
  (add-hook! 'dired-mode-hook #'dired-hide-details-mode))

(after! treemacs
  (treemacs-project-follow-mode nil)
  (treemacs-git-mode 'deferred)
  (setq treemacs-collapse-dirs 10
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-file-event-delay 5000
        treemacs-file-follow-delay 0.2
        treemacs-indentation 1
        treemacs-git-integration t))

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t))

(use-package! d2-mode
  :config
  (setq d2-output-format ".png"))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq! copilot-idle-delay 1))

(use-package! claude-code-ide
  :config
  (claude-code-ide-emacs-tools-setup)
  
  ;; Claude Code IDE key bindings under SPC o c (open > claude)
  (map! :leader
        :prefix ("o c" . "claude")
        :desc "Start Claude session" "c" #'claude-code-ide
        :desc "Continue conversation" "C" #'claude-code-ide-continue
        :desc "Resume session" "r" #'claude-code-ide-resume
        :desc "Stop Claude session" "q" #'claude-code-ide-stop
        :desc "Switch to Claude buffer" "b" #'claude-code-ide-switch-to-buffer
        :desc "List all sessions" "l" #'claude-code-ide-list-sessions
        :desc "Toggle Claude window" "t" #'claude-code-ide-toggle
        :desc "Send selection to Claude" "s" #'claude-code-ide-insert-at-mentioned
        :desc "Send prompt from minibuffer" "p" #'claude-code-ide-send-prompt
        :desc "Insert newline in Claude" "n" #'claude-code-ide-insert-newline
        :desc "Send escape key" "e" #'claude-code-ide-send-escape
        :desc "Main menu (transient)" "m" #'claude-code-ide-menu
        :desc "Check CLI status" "?" #'claude-code-ide-check-status
        :desc "Show debug buffer" "d" #'claude-code-ide-show-debug
        :desc "Clear debug buffer" "D" #'claude-code-ide-clear-debug))

