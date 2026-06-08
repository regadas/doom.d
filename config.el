;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Core -----------------------------------------------------------------------

;; Garbage Collection Optimizations
(add-hook! 'doom-after-init-hook
  (setq gc-cons-threshold (* 100 1024 1024)  ; 100MB
        gc-cons-percentage 0.1))

(add-function :after after-focus-change-function
              (lambda () (unless (frame-focus-state) (garbage-collect))))

(setq user-full-name "Filipe Regadas"
      user-mail-address "oss@regadas.email")

(setq auth-sources '("~/.authinfo.gpg"))

(setq doom-theme 'modus-operandi
      doom-font (font-spec :family "Iosevka SS14" :size 16))

(setq mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t)

(after! modus-themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-fringes 'subtle
        modus-themes-prompts '(extrabold italic)))

(setq inhibit-compacting-font-caches t
      auto-save-default t
      idle-update-delay 1.0
      jit-lock-defer-time 0.05)

(setq display-line-numbers-type 'relative)

;;; Editor / UI ----------------------------------------------------------------

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(after! evil
  (setq-default evil-escape-key-sequence "jj"))

(map! (:when (modulep! :tools lookup)
        :nv "gh"   #'+lookup/documentation))

(use-package! zoom
  :config
  (setq zoom-size '(0.678 . 0.678)))

(after! embark
  (set-popup-rule! "^\\*Embark Export" :ignore t))

(after! dired
  (setq delete-by-moving-to-trash t
        dired-listing-switches "-lat")  ; sort by date
  (add-hook! 'dired-mode-hook #'dired-hide-details-mode))

;; (after! treemacs
;;   ;; `treemacs-project-follow-mode' lives in a separate Treemacs feature, and
;;   ;; passing nil toggles minor modes; use -1 to disable it explicitly.
;;   (when (require 'treemacs-project-follow-mode nil t)
;;     (treemacs-project-follow-mode -1))
;;   (treemacs-git-mode 'deferred)
;;   (setq treemacs-collapse-dirs 10
;;         treemacs-silent-refresh t
;;         treemacs-silent-filewatch t
;;         treemacs-file-event-delay 2000
;;         treemacs-file-follow-delay 0.2
;;         treemacs-indentation 1))

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-hud t               ; top-of-window position indicator
        doom-modeline-buffer-encoding 'nondefault)  ; only show when non-utf-8

  ;; Enable features
  (doom-modeline-def-modeline 'main
    '(bar modals workspace-name window-number matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(misc-info grip github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs)))

(use-package! nyan-mode
  :after doom-modeline
  :config
  (setq nyan-wavy-trail t)
  (nyan-mode 1))

(after! corfu
  (setq corfu-auto-delay 0.3))

(after! diff-hl
  (setq diff-hl-flydiff-delay 2
        diff-hl-draw-borders nil))

;;; Projects / VCS -------------------------------------------------------------

(after! projectile
  (setq projectile-project-search-path '("~/projects/" "~/projects/spotify" "~/projects/experiments")
        projectile-project-root-files-bottom-up '(".projectile" ".git")
        projectile-enable-caching t))

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^ghe\\.spotify\\.net$" :type "github") 'append))

(after! forge
  (push '("ghe.spotify.net" "ghe.spotify.net/api/v3" "ghe.spotify.net" forge-github-repository)
        forge-alist))

;; --- Monorepo performance: lightweight magit for large repos ---
(defvar +my/magit-large-repo-dirs nil
  "List of directory paths where magit should use lightweight settings.")

(defun +my/magit-large-repo-p ()
  "Return non-nil if `default-directory' is inside a registered large repo."
  (and +my/magit-large-repo-dirs
       (cl-some (lambda (dir)
                  (file-in-directory-p
                   (file-truename default-directory)
                   (file-truename (expand-file-name dir))))
                +my/magit-large-repo-dirs)))

(defun +my/magit-lightweight-status-h ()
  "Strip expensive sections in large repos."
  (when (+my/magit-large-repo-p)
    (setq-local magit-revision-insert-related-refs nil)
    (setq-local magit-status-headers-hook
                '(magit-insert-error-header
                  magit-insert-diff-filter-header
                  magit-insert-head-branch-header
                  magit-insert-upstream-branch-header
                  magit-insert-push-branch-header))
    (setq-local magit-status-sections-hook
                '(magit-insert-status-headers
                  magit-insert-merge-log
                  magit-insert-rebase-sequence
                  magit-insert-am-sequence
                  magit-insert-sequencer-sequence
                  magit-insert-bisect-output
                  magit-insert-bisect-rest
                  magit-insert-bisect-log
                  magit-insert-untracked-files
                  magit-insert-unstaged-changes
                  magit-insert-staged-changes
                  magit-insert-stashes))))

(after! magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-worktrees
                          'magit-insert-status-headers t)

  (add-hook 'magit-status-mode-hook #'+my/magit-lightweight-status-h)

  ;; Suppress auto-refresh of status buffer from other buffers in large repos.
  ;; magit-refresh-status-buffer is read in the calling buffer's scope, so
  ;; setq-local in the status buffer has no effect — use advice instead.
  (defadvice! +my/magit-maybe-skip-status-refresh-a (orig &rest args)
    :around #'magit-refresh
    (if (and (not (derived-mode-p 'magit-status-mode))
             (when-let ((buf (magit-get-mode-buffer 'magit-status-mode)))
               (with-current-buffer buf (+my/magit-large-repo-p))))
        (let ((magit-refresh-status-buffer nil))
          (apply orig args))
      (apply orig args)))

  (setq magit-diff-highlight-indentation nil
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil
        magit-diff-refine-hunk nil))

;;; LSP ------------------------------------------------------------------------

(after! lsp-mode
  ;; Performance: disable heavy features
  (setq lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-semantic-tokens-enable nil
        lsp-enable-folding nil
        lsp-enable-links nil
        lsp-enable-file-watchers nil
        lsp-enable-on-type-formatting nil
        lsp-enable-imenu nil
        lsp-enable-indentation nil
        lsp-eldoc-enable-hover nil
        lsp-eldoc-render-all nil
        lsp-signature-auto-activate nil
        lsp-progress-via-spinner nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-ui-sideline-enable nil

        ;; Keep enabled (lightweight and useful)
        lsp-enable-symbol-highlighting t
        lsp-completion-show-detail t
        lsp-completion-show-kind t

        ;; Core settings
        lsp-use-plists t
        ;; lsp-log-io nil
        lsp-auto-guess-root t
        lsp-idle-delay 0.25
        lsp-response-timeout 10
        lsp-keep-workspace-alive nil
        lsp-copilot-enabled nil
        lsp-completion-enable-additional-text-edit nil
        lsp-bash-highlight-parsing-errors t

        ;; Typescript LSP
        lsp-clients-typescript-max-ts-server-memory 8192

        ;; Java LSP
        lsp-java-vmargs '("-Xms1G"
                          "-Xmx8G"
                          "-XX:+UseParallelGC"
                          "-XX:GCTimeRatio=4"
                          "-XX:AdaptiveSizePolicyWeight=90"
                          "-Dsun.zip.disableMemoryMapping=true")
        lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.57.0/jdt-language-server-1.57.0-202602261110.tar.gz"
        lsp-java-jdt-ls-android-support-enabled nil
        lsp-java-completion-max-results 30
        lsp-java-maven-download-sources t
        lsp-java-import-gradle-enabled t
        lsp-java-autobuild-enabled t
        lsp-java-trace-server "off"
        lsp-java-references-code-lens-enabled nil
        lsp-java-format-enabled nil
        lsp-java-signature-help-enabled nil
        lsp-java-progress-reports-enabled nil))

;;; Languages ------------------------------------------------------------------

;; Java
(after! lsp-java
  ;; --- Workspace-dir helpers (must be defined first) -------------------------

  (defun my/lsp-java--workspace-dir-for-root (root)
    "Return the per-project jdtls workspace directory for ROOT.
Includes the JDT LS version in the hash so upgrading the server
automatically gets a fresh workspace instead of reusing stale metadata."
    (let* ((canonical-root (directory-file-name (file-truename root)))
           (version (and (boundp 'lsp-java-jdt-download-url)
                         (when (string-match "/\\([0-9][0-9.]*\\)/" lsp-java-jdt-download-url)
                           (match-string 1 lsp-java-jdt-download-url))))
           (hash (md5 (concat canonical-root "\0" (or version "unknown")))))
      (expand-file-name hash
                        (expand-file-name "jdtls-workspaces"
                                          lsp-server-install-dir))))

  (defun my/lsp-java--workspace-dir ()
    "Return the per-project jdtls workspace directory for the current project."
    (my/lsp-java--workspace-dir-for-root
     (or (projectile-project-root) default-directory)))

  (defun my/lsp-java--remove-stale-lock (ws-dir)
    "Remove a stale .metadata/.lock file in WS-DIR if no jdtls owns it."
    (let ((lock (expand-file-name ".metadata/.lock" ws-dir)))
      (when (and (file-exists-p lock)
                 (not (seq-some (lambda (ws)
                                  (string-prefix-p ws-dir (lsp--workspace-root ws)))
                                (lsp--session-workspaces (lsp-session)))))
        (delete-file lock)
        (message "Removed stale lock: %s" lock))))

  ;; --- Client patches --------------------------------------------------------
  ;;
  ;; 1. Disable multi-root so lsp-mode starts a SEPARATE jdtls per project root
  ;;    instead of adding new folders to an existing instance.
  ;; 2. Override :library-folders-fn so each jdtls gets its own cache dir
  ;;    (the default shares ~/.emacs.d/workspace/.cache/ across all instances).
  ;; 3. Wrap :initialization-options so the LSP initialize handshake sends only
  ;;    the current project root as workspaceFolders, not the global session list.

  (when-let ((jdtls-client (gethash 'jdtls lsp-clients)))
    ;; (1) Disable multi-root
    (setf (lsp--client-multi-root jdtls-client) nil)

    ;; (2) Per-project library/cache folder
    (setf (lsp--client-library-folders-fn jdtls-client)
          (lambda (workspace)
            (list (expand-file-name
                   ".cache/"
                   (my/lsp-java--workspace-dir-for-root
                    (lsp--workspace-root workspace))))))

    ;; (3) Isolated workspaceFolders in initialization-options
    (let ((orig-init-fn (lsp--client-initialization-options jdtls-client)))
      (setf (lsp--client-initialization-options jdtls-client)
            (lambda ()
              (let ((opts (funcall orig-init-fn)))
                (plist-put opts :workspaceFolders
                           (vector (lsp--path-to-uri
                                    (or (projectile-project-root)
                                        default-directory)))))))))

  ;; --- Filename fix ----------------------------------------------------------
  ;; lsp-java--get-filename can produce cache names without a .java extension
  ;; (second regex branch), so Emacs opens them in fundamental-mode and LSP
  ;; never attaches.  Ensure the name always ends in .java.
  (when (fboundp 'lsp-java--get-filename)
    (defadvice! +lsp-java--get-filename-a (orig-fn &rest args)
      :around #'lsp-java--get-filename
      (let ((name (apply orig-fn args)))
        (if (and name (not (string-suffix-p ".java" name)))
            (concat name ".java")
          name))))

  ;; --- Workspace-folders isolation -------------------------------------------
  ;; The stock lsp-java--workspace-folders returns ALL session folders, leaking
  ;; every project's root to every jdtls instance.  Return only the current
  ;; workspace's root so jdtls never sees unrelated projects.
  (defadvice! my/lsp-java--workspace-folders-a (workspace)
    "Return only WORKSPACE's own root folder."
    :override #'lsp-java--workspace-folders
    (if workspace
        (list (lsp--workspace-root workspace))
      (list (or (projectile-project-root) default-directory))))

  ;; --- Command builder advice ------------------------------------------------
  ;; Bind both lsp-java-workspace-dir and lsp-java-workspace-cache-dir to
  ;; per-project paths around the server command construction.
  (when (fboundp 'lsp-java--ls-command)
    (defadvice! my/lsp-java--ls-command-a (orig-fn)
      "Bind `lsp-java-workspace-dir' and `lsp-java-workspace-cache-dir'
to per-project paths and remove any stale lock beforehand."
      :around #'lsp-java--ls-command
      (let* ((lsp-java-workspace-dir (my/lsp-java--workspace-dir))
             (lsp-java-workspace-cache-dir (expand-file-name ".cache/"
                                                             lsp-java-workspace-dir)))
        (when (file-directory-p lsp-java-workspace-dir)
          (my/lsp-java--remove-stale-lock lsp-java-workspace-dir))
        (funcall orig-fn))))

  ;; --- Interactive clean command ---------------------------------------------
  (defun my/lsp-java-clean-workspace ()
    "Delete the current project's jdtls workspace and restart LSP.
Use this when jdtls fails to start due to a corrupted workspace."
    (interactive)
    (let ((ws-dir (my/lsp-java--workspace-dir)))
      (unless (file-directory-p ws-dir)
        (user-error "No jdtls workspace directory found at %s" ws-dir))
      (when (y-or-n-p (format "Delete jdtls workspace %s?" ws-dir))
        (delete-directory ws-dir t)
        (message "Deleted %s. Reopen a Java file to restart jdtls." ws-dir)
        (when (bound-and-true-p lsp-mode)
          (lsp-disconnect))))))

(add-hook! '(java-mode-hook java-ts-mode-hook)
  (setq-local c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil))

(after! dap-mode
  (setq dap-java--var-format "\"$%s\""))

;; Avro
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.avpr\\'" . json-mode))

(define-generic-mode 'avro-idl-mode
  '("//" ("/*" . "*/"))
  '("protocol" "record" "error" "enum" "union" "fixed" "import" "idl" "schema"
    "null" "void" "boolean" "int" "long" "float" "double" "bytes" "string"
    "array" "map" "throws" "oneway" "namespace" "order" "aliases" "doc"
    "date" "time_ms" "timestamp_ms" "decimal" "uuid")
  '(("@[A-Za-z_][A-Za-z0-9_]*" . font-lock-preprocessor-face)
    ("`[^`]+`" . font-lock-variable-name-face)
    ("\\b[A-Z][A-Za-z0-9_]*\\b" . font-lock-type-face))
  '("\\.avdl\\'")
  nil
  "Major mode for Apache Avro IDL files.")

;; Bazel
(after! bazel
  (setq bazel-buildifier-before-save t))

;; Dhall
(use-package! dhall-mode
  :defer t
  :init
  (add-hook! 'dhall-mode-local-vars-hook :append #'lsp!)
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

;; PR Review
(use-package! pr-review
  :commands (pr-review pr-review-open-url)
  :config
  (setq pr-review-forges-alist
        '(("ghe.spotify.net" . (github "ghe.spotify.net/api/v3" nil))
          ("github.com" . (github nil nil))
          ("gitlab.com" . (gitlab nil nil)))
        ;; Use gh CLI token (classic OAuth with repo scope) — fine-grained
        ;; tokens don't support the /compare/ endpoint on GHE 3.19.
        pr-review-ghub-auth-name
        (string-trim (shell-command-to-string
                      "gh auth token --hostname ghe.spotify.net"))))

;; PR Inbox
(use-package! pr-inbox
  :commands (pr-inbox))

;; SQL
(use-package! sql-bigquery
  :after sql)

(after! sql
  (set-formatter! 'sqlfluff
    '("sqlfluff" "format" "-")
    :modes '(sql-mode)))

;; D2
(use-package! d2-mode
  :config
  (setq d2-output-format ".png"))

(after! d2-mode
  (reformatter-define d2-format
    :program "d2"
    :stdin nil
    :stdout nil
    :args (list "fmt" input-file)
    :lighter " D2Fmt"))

;; Org
(setq org-directory "~/projects/brain-dump")

(use-package! ox-awesomecv
  :after org)

(after! org
  (setq org-latex-compiler "xelatex"))

;; Markdown
(after! markdown-mode
  (setq markdown-hide-markup t))

(after! grip-mode
  (setq grip-command 'mdopen)

  (map! :map markdown-mode-map
        :localleader
        "p" #'grip-browse-preview))

;;; Tools ----------------------------------------------------------------------

(after! vterm
  (setq vterm-timer-delay 0.03
        vterm-disable-underline t
        vterm-disable-bold t)
  (define-key vterm-mode-map [deletechar] #'vterm-send-delete))

(defun +ghostel/project-popup (&optional arg)
  "Toggle a project-root Ghostel terminal in a Doom popup.

This uses a project-scoped popup buffer, separate from `ghostel-project',
so `SPC o G' can still open a normal project terminal.  With prefix ARG,
pass ARG through to `ghostel'."
  (interactive "P")
  (require 'ghostel)
  (let* ((default-directory (project-root (project-current t)))
         (ghostel-buffer-name (project-prefixed-buffer-name
                               (concat (string-trim ghostel-buffer-name "*" "*")
                                       "-popup")))
         (buffer (ghostel--find-buffer-by-identity ghostel-buffer-name))
         (window (and buffer (get-buffer-window buffer))))
    (if (and window (not arg))
        (delete-window window)
      (let ((display-buffer-overriding-action
             '(+popup-buffer
               (side . bottom)
               (vslot . -5)
               (window-height . 0.35)
               (window-parameters
                (ttl . nil)
                (quit . nil)
                (select . t)
                (modeline . nil)))))
        (ghostel arg)))))

(defun +ghostel/project-new ()
  "Open a fresh project-root Ghostel terminal in the current window."
  (interactive)
  (require 'ghostel)
  ;; `ghostel-project' reuses the project terminal by default; a
  ;; non-numeric arg asks Ghostel for a fresh buffer instead.
  (ghostel-project t))

(use-package! ghostel
  :defer t
  :commands (ghostel
             ghostel-project)
  :init
  (map! :leader
        :desc "Toggle Ghostel project popup" "o g" #'+ghostel/project-popup
        :desc "Open new Ghostel project terminal" "o G" #'+ghostel/project-new))

(use-package! evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(use-package! kubel
  :defer t
  :commands (kubel)
  :config (kubel-vterm-setup))

;; accept completion from copilot and fallback to corfu
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word))
  :config
  (setq copilot-idle-delay 0.5))

(use-package! consult-gh
  :after consult
  :config
  (consult-gh-enable-default-keybindings)
  (setq consult-gh-default-clone-directory "~/projects/"))

(use-package! claude-code-ide
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-use-side-window t
        claude-code-ide-terminal-backend 'vterm
        claude-code-ide-cli-extra-flags "--enable-auto-mode")
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

;; (use-package! vterm-anti-flicker-filter)
;; (after! claude-code-ide
;;   (setq claude-code-ide-vterm-anti-flicker t
;;         claude-code-ide-vterm-render-delay 0))

(use-package! agent-shell
  :defer t
  :commands (agent-shell
             agent-shell-anthropic-start-claude-code
             agent-shell-google-start-gemini
             agent-shell-openai-start-codex
             agent-shell-pi-start-agent)
  :config
  (setq agent-shell-show-welcome-message nil
        agent-shell-preferred-agent-config 'claude-code)
  (require 'acp))

(use-package! agent-review
  :defer t
  :commands (agent-review)
  :after agent-shell)

(map! :leader
      :prefix ("o c a" . "agent-shell")
      :desc "Start agent-shell"        "a" #'agent-shell
      :desc "Start Claude Code (ACP)"  "c" #'agent-shell-anthropic-start-claude-code
      :desc "Start Gemini (ACP)"       "g" #'agent-shell-google-start-gemini
      :desc "Start Codex (ACP)"        "x" #'agent-shell-openai-start-codex
      :desc "Start Pi (ACP)"           "p" #'agent-shell-pi-start-agent
      :desc "Review git changes"       "r" #'agent-review)

(defun +link-hint-open-link-choose ()
  "Select a link with link-hint, then choose to open in webkit or brwser."
  (interactive)
  (link-hint-copy-link)
  (let ((url (current-kill 0)))
    (pcase (read-char-choice (format "[w]ebkit [b]rowser: ") '(?w ?b))
      (?w (xwidget-webkit-browse-url url))
      (?b (browse-url url)))))

(map! :leader
      :desc "Open link" "s l" #'+link-hint-open-link-choose)
