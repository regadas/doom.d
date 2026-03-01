;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; Garbage Collection Optimizations
;; After startup, restore reasonable GC threshold
(add-hook! 'doom-after-init-hook
  (setq gc-cons-threshold (* 100 1024 1024)  ; 100MB
        gc-cons-percentage 0.1))

;; GC when Emacs loses focus
(add-function :after after-focus-change-function
  (lambda () (unless (frame-focus-state) (garbage-collect))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Filipe Regadas"
      user-mail-address "oss@regadas.email")

(setq auth-sources '("~/.authinfo.gpg"))

(setq doom-theme 'modus-operandi
      doom-font (font-spec :family "Iosevka Nerd Font" :size 16))

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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/brain-dump")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(use-package! zoom
  :config
  (setq zoom-size '(0.678 . 0.678)))

(after! embark
  (set-popup-rule! "^\\*Embark Export" :ignore t))

(after! vterm
  (setq vterm-max-scrollback 5000
        vterm-timer-delay 0.03)
  (define-key vterm-mode-map [deletechar] #'vterm-send-delete))

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(after! dap-mode
  (setq dap-java--var-format "\"$%s\""))

(map! (:when (modulep! :tools lookup)
        :nv "gh"   #'+lookup/documentation
        ))

(after! evil
  (setq-default evil-escape-key-sequence "jj"))

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

(use-package! kubel
  :defer t
  :commands (kubel)
  :config (kubel-vterm-setup))

(after! sql
  ;; Configure sqlfluff formatter for SQL (BigQuery dialect)
  (set-formatter! 'sqlfluff
    '("sqlfluff" "format" "-")
    :modes '(sql-mode)))

(after! d2-mode
  (reformatter-define d2-format
    :program "d2"
    :stdin nil
    :stdout nil
    :args (list "fmt" input-file)
    :lighter " D2Fmt"))

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
        lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.56.0/jdt-language-server-1.56.0-202601291528.tar.gz"
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

;; Per-project jdtls workspace directories to allow multiple instances.
;; Disable multi-root so lsp-mode starts a separate jdtls per project root,
;; and give each instance its own -data dir to avoid lock conflicts.
(after! lsp-java
  (setf (cl-struct-slot-value 'lsp--client 'multi-root
                              (gethash 'jdtls lsp-clients))
        nil)

  (defun my/lsp-java--workspace-dir ()
    "Return the per-project jdtls workspace directory.
Includes the JDT LS version in the hash so upgrading the server
automatically gets a fresh workspace instead of reusing stale metadata."
    (let* ((root (directory-file-name
                  (file-truename (or (projectile-project-root) default-directory))))
           (version (and (boundp 'lsp-java-jdt-download-url)
                         (when (string-match "/\\([0-9][0-9.]*\\)/" lsp-java-jdt-download-url)
                           (match-string 1 lsp-java-jdt-download-url))))
           (hash (md5 (concat root "\0" (or version "unknown")))))
      (expand-file-name hash
                        (expand-file-name "jdtls-workspaces"
                                          lsp-server-install-dir))))

  (defun my/lsp-java--remove-stale-lock (ws-dir)
    "Remove a stale .metadata/.lock file in WS-DIR if no jdtls owns it."
    (let ((lock (expand-file-name ".metadata/.lock" ws-dir)))
      (when (and (file-exists-p lock)
                 (not (seq-some (lambda (ws)
                                  (string-prefix-p ws-dir (lsp--workspace-root ws)))
                                (lsp--session-workspaces (lsp-session)))))
        (delete-file lock)
        (message "Removed stale lock: %s" lock))))

  (add-hook 'java-mode-hook
            (defun my/lsp-java-set-workspace-dir ()
              "Set a per-project jdtls workspace directory."
              (setq-local lsp-java-workspace-dir (my/lsp-java--workspace-dir))
              (when (file-directory-p lsp-java-workspace-dir)
                (my/lsp-java--remove-stale-lock lsp-java-workspace-dir))))

  (defun my/lsp-java-clean-workspace ()
    "Delete the current project's jdtls workspace and restart LSP.
Use this when jdtls fails to start due to a corrupted workspace."
    (interactive)
    (let ((ws-dir (or lsp-java-workspace-dir (my/lsp-java--workspace-dir))))
      (unless (file-directory-p ws-dir)
        (user-error "No jdtls workspace directory found at %s" ws-dir))
      (when (y-or-n-p (format "Delete jdtls workspace %s?" ws-dir))
        (delete-directory ws-dir t)
        (message "Deleted %s. Reopen a Java file to restart jdtls." ws-dir)
        (when (bound-and-true-p lsp-mode)
          (lsp-disconnect))))))

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
        treemacs-file-event-delay 2000
        treemacs-file-follow-delay 0.2
        treemacs-indentation 1))

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t))

(use-package! d2-mode
  :config
  (setq d2-output-format ".png"))

;; accept completion from copilot and fallback to corfu
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word))
  :config
  (setq copilot-idle-delay 0.5))

(use-package! claude-code-ide
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-use-side-window t
        claude-code-ide-terminal-backend 'vterm)
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

(after! grip-mode
  (setq grip-command 'mdopen)

  (map! :map markdown-mode-map
        :localleader
        "p" #'grip-browse-preview))

(after! corfu
  (setq corfu-auto-delay 0.3))

(after! diff-hl
  (setq diff-hl-flydiff-delay 2
        diff-hl-draw-borders nil))

(defun +link-hint-open-link-choose ()
  "Select a link with link-hint, then choose to open in webkit or browser."
  (interactive)
  (link-hint-copy-link)
  (let ((url (current-kill 0)))
    (pcase (read-char-choice (format "[w]ebkit [b]rowser: ") '(?w ?b))
      (?w (xwidget-webkit-browse-url url))
      (?b (browse-url url)))))

(map! :leader
      :desc "Open link" "s l" #'+link-hint-open-link-choose)

;;; Magit worktree → workspace + project integration
(after! (:and magit persp-mode)
  (defun +my/worktree-setup ()
    "Add worktree as a known project, create a workspace, and switch to it."
    (let ((dir (directory-file-name default-directory))
          (name (file-name-nondirectory (directory-file-name default-directory))))
      (projectile-add-known-project dir)
      (if (+workspace-exists-p name)
          (+workspace-switch name)
        (+workspace-switch name t))
      (projectile-switch-project-by-name dir)))

  (defun +my/worktree-teardown (worktree &rest _)
    "Remove worktree project and workspace."
    (let* ((dir (directory-file-name worktree))
           (name (file-name-nondirectory dir)))
      (projectile-remove-known-project dir)
      (when (+workspace-exists-p name)
        (+workspace-delete name))))

  (defadvice! +my/worktree-checkout-a (&rest _)
    "Set up workspace and project after `magit-worktree-checkout'."
    :after #'magit-worktree-checkout
    (+my/worktree-setup))

  (defadvice! +my/worktree-branch-a (&rest _)
    "Set up workspace and project after `magit-worktree-branch'."
    :after #'magit-worktree-branch
    (+my/worktree-setup))

  (defadvice! +my/worktree-status-a (&rest _)
    "Set up workspace and project after `magit-worktree-status'."
    :after #'magit-worktree-status
    (+my/worktree-setup))

  (defadvice! +my/worktree-delete-a (worktree &rest _)
    "Clean up workspace and project after `magit-worktree-delete'."
    :after #'magit-worktree-delete
    (+my/worktree-teardown worktree)))
