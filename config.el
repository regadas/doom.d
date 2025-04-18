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
      doom-font (font-spec :family "TX-02" :size 15))

(setq mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t)

(after! modus-themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-fringes 'subtle
        modus-themes-prompts '(extrabold italic)))

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"               ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always  ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

;; better pixel scrolling with ultra-scroll
(use-package! ultra-scroll
  :custom
  (auto-hscroll-mode t)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (scroll-step 0)
  :config
  (ultra-scroll-mode 1))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/brain-dump")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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
  (setq vterm-max-scrollback 1000
        vterm-timer-delay 0.03)
  (define-key vterm-mode-map [deletechar] #'vterm-send-delete))

;; highlight undoed text
(use-package! undo-hl
  :hook ((text-mode . undo-hl-mode)
         (prog-mode . undo-hl-mode)))

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
        projectile-enable-caching nil))

(setq-default TeX-engine 'xetex
              pdf-latex-command "xelatex")

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^ghe\\.spotify\\.net$" :type "github") 'append))

(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
(setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)

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

(use-package! dimmer
  ;; :hook (prog-mode . dimmer-mode)
  :config
  (dimmer-configure-company-box)
  (dimmer-configure-org)
  (dimmer-configure-magit)
  (dimmer-configure-which-key)
  (setq dimmer-fraction 0.3))

;; copilot.el
(use-package! copilot
  :hook ((prog-mode yaml-mode text-mode) . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (add-to-list 'warning-suppress-types '(copilot)))

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

(add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))
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
        lsp-use-plists t
        lsp-log-io nil
        lsp-auto-guess-root t
        lsp-enable-file-watchers nil
        lsp-idle-delay 0.5
        lsp-response-timeout 1

        lsp-java-maven-download-sources t
        lsp-java-completion-guess-method-arguments nil
        lsp-java-compile-null-analysis-mode "automatic"
        lsp-java-server-config-dir "/Users/regadas/.emacs.d/.local/etc/java-config"
        lsp-java-vmargs '(
                          "-Xmx16G"
                          "-XX:+UseZGC"
                          "-XX:+UseStringDeduplication"
                          "-Dlog.level=ERROR"
                          "-Djava.import.generatesMetadataFilesAtProjectRoot=false"
                          "-Dosgi.checkConfiguration=true"
                          "-Dosgi.sharedConfiguration.area=/Users/regadas/.emacs.d/.local/etc/lsp/eclipse.jdt.ls/config_mac"
                          )
        lsp-java-jdt-download-url "http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
        lsp-java-jdt-ls-prefer-native-command t
        lsp-java-jdt-ls-android-support-enabled nil

        ;; Performance optimizations for Java
        lsp-java-autobuild-enabled nil         ;; disable auto-build for better performance
        lsp-java-completion-max-results 20     ;; limit completion results 
        lsp-java-folding-range-enabled nil     ;; disable code folding calculation
        lsp-java-import-gradle-enabled t
        lsp-java-selection-enabled nil         ;; disable selection ranges
        lsp-java-trace-server "off"            ;; disable server tracing
        lsp-java-references-code-lens-enabled nil ;; disable references code lens
        
        lsp-bash-highlight-parsing-errors t)

  (with-eval-after-load 'lsp-rust
    (require 'dap-cpptools))

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(after! lsp-ui
  (setq lsp-ui-doc-enable nil))     ; redundant with K

(after! go-mode
  (if (modulep! +lsp)
      (add-hook 'go-mode-hook #'lsp-deferred)
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; (use-package! ellama
;;   :init
;;   (setopt ellama-language "English")
;;   (require 'llm-ollama)
;;   (setopt ellama-provider
;;           (make-llm-ollama
;;            :chat-model "codellama:7b-instruct" :embedding-model "codellama:7b-instruct")))

(after! dired
  (setq delete-by-moving-to-trash t
        dired-listing-switches "-lat")  ; sort by date
  (add-hook! 'dired-mode-hook #'dired-hide-details-mode))

(after! treemacs
  (treemacs-project-follow-mode t)
  (treemacs-git-mode 'deferred)
  (setq treemacs-collapse-dirs 20))

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t))

(use-package! d2-mode
  :config
  (setq d2-output-format ".png"))


(use-package! gptel
  :config
  (setq gptel-model "gpt-4o"))
