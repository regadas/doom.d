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
        projectile-enable-caching t
        projectile-project-name-function #'+my/project-name)

  (defun +my/discover-worktrees ()
    "Scan projects in `projectile-project-search-path' for git worktrees
and register them as known projectile projects."
    (interactive)
    (dolist (search-dir projectile-project-search-path)
      (let ((dir (file-name-as-directory (expand-file-name search-dir))))
        (when (file-directory-p dir)
          (dolist (entry (directory-files dir t "\\`[^.]"))
            (when (and (file-directory-p entry)
                       (file-exists-p (expand-file-name ".git" entry)))
              (with-temp-buffer
                (let ((exit-code (call-process "git" nil t nil
                                               "-C" entry "worktree" "list" "--porcelain")))
                  (if (zerop exit-code)
                      (dolist (line (split-string (buffer-string) "\n"))
                        (when (string-match "\\`worktree \\(.+\\)" line)
                          (let ((wt-path (match-string 1 line)))
                            (when (and (file-directory-p wt-path)
                                       (not (equal (file-truename wt-path)
                                                   (file-truename entry))))
                              (projectile-add-known-project wt-path)))))
                    (message "+my/discover-worktrees: git failed (exit %d) in %s" exit-code entry))))))))))

  (defadvice! +my/discover-worktrees-a (&rest _)
    "Also discover worktrees when discovering projects."
    :after #'projectile-discover-projects-in-search-path
    (+my/discover-worktrees)))

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^ghe\\.spotify\\.net$" :type "github") 'append))

(after! forge
  (push '("ghe.spotify.net" "ghe.spotify.net/api/v3" "ghe.spotify.net" forge-github-repository)
        forge-alist))

;;; Worktree helpers (used by both projectile and magit integration)
(defun +my/worktree-parent-project (dir)
  "Return the parent project name for a worktree at DIR.
In a worktree, .git is a file containing \"gitdir: /path/to/parent/.git/worktrees/NAME\".
Parse it to extract the parent project name."
  (let ((gitfile (expand-file-name ".git" dir)))
    (when (file-regular-p gitfile)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents gitfile)
          (when (re-search-forward "gitdir: \\(.+\\)" nil t)
            (let ((gitdir (string-trim (match-string 1))))
              (when (string-match "/\\.git/worktrees/" gitdir)
                (file-name-nondirectory
                 (substring gitdir 0 (match-beginning 0)))))))))))

(defun +my/project-name (project-root)
  "Return project name for PROJECT-ROOT, prefixed with parent for worktrees."
  (let* ((dir (directory-file-name project-root))
         (name (file-name-nondirectory dir))
         (parent (+my/worktree-parent-project dir)))
    (if parent
        (format "%s/%s" parent name)
      name)))

(after! magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-worktrees
                          'magit-insert-status-headers t))

;;; Magit worktree → workspace + project integration
(after! (:and magit persp-mode)
  (defun +my/worktree-setup (worktree-path)
    "Add WORKTREE-PATH as a known project, create a workspace, and switch to it."
    (let* ((dir (file-name-as-directory (expand-file-name worktree-path)))
           (name (+my/project-name dir)))
      (projectile-add-known-project dir)
      (+workspace-switch name (not (+workspace-exists-p name)))
      (projectile-switch-project-by-name dir)))

  (defvar +my/--pending-teardown nil
    "Plist (:name :project-entry) captured before worktree deletion.")

  (defun +my/worktree-teardown ()
    "Remove worktree project and workspace using data captured by :before advice."
    (if-let* ((info +my/--pending-teardown)
              (name (plist-get info :name))
              (entry (plist-get info :project-entry)))
        (progn
          (setq projectile-known-projects (delete entry projectile-known-projects))
          (projectile-save-known-projects)
          (when (+workspace-exists-p name)
            (+workspace-delete name))
          (setq +my/--pending-teardown nil))
      (message "+my/worktree-teardown: no pending teardown data")))

  (defadvice! +my/worktree-checkout-a (path &rest _)
    "Set up workspace and project after `magit-worktree-checkout'."
    :after #'magit-worktree-checkout
    (+my/worktree-setup path))

  (defadvice! +my/worktree-branch-a (path &rest _)
    "Set up workspace and project after `magit-worktree-branch'."
    :after #'magit-worktree-branch
    (+my/worktree-setup path))

  (defadvice! +my/worktree-status-a (worktree &rest _)
    "Set up workspace and project after `magit-worktree-status'."
    :after #'magit-worktree-status
    (when (file-regular-p (expand-file-name ".git" worktree))
      (+my/worktree-setup worktree)))

  (defadvice! +my/worktree-delete-before-a (worktree &rest _)
    "Capture workspace name and projectile entry before directory is deleted."
    :before #'magit-worktree-delete
    (let* ((name (+my/project-name worktree))
           (abbrev (abbreviate-file-name
                    (file-name-as-directory (expand-file-name worktree))))
           (entry (cl-find abbrev projectile-known-projects :test #'string=)))
      (unless entry
        (message "+my/worktree-delete: %s not found in projectile (looked for %s)" worktree abbrev))
      (setq +my/--pending-teardown
            (list :name name :project-entry (or entry abbrev)))))

  (defadvice! +my/worktree-delete-a (&rest _)
    "Clean up workspace and project after `magit-worktree-delete'."
    :after #'magit-worktree-delete
    (+my/worktree-teardown)))

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
  (setf (cl-struct-slot-value 'lsp--client 'multi-root
                              (gethash 'jdtls lsp-clients))
        nil)

  ;; lsp-java--get-filename can produce cache names without a .java extension
  ;; (second regex branch), so Emacs opens them in fundamental-mode and LSP
  ;; never attaches.  Ensure the name always ends in .java.
  (defadvice! +lsp-java--get-filename-a (orig-fn &rest args)
    :around #'lsp-java--get-filename
    (let ((name (apply orig-fn args)))
      (if (and name (not (string-suffix-p ".java" name)))
          (concat name ".java")
        name)))

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

  ;; Advise the command builder so the per-project workspace dir is used
  ;; at server-start time, avoiding the hook-ordering issue where
  ;; java-mode-hook fires before lsp-java is loaded.
  (defadvice! my/lsp-java--ls-command-a (orig-fn)
    "Bind `lsp-java-workspace-dir' to a per-project path around the
server command construction, and remove any stale lock beforehand."
    :around #'lsp-java--ls-command
    (let ((lsp-java-workspace-dir (my/lsp-java--workspace-dir)))
      (when (file-directory-p lsp-java-workspace-dir)
        (my/lsp-java--remove-stale-lock lsp-java-workspace-dir))
      (funcall orig-fn)))

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

(after! java-mode
  (setq c-basic-offset 4
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

(use-package! agent-shell
  :defer t
  :commands (agent-shell
             agent-shell-anthropic-start-claude-code
             agent-shell-google-start-gemini)
  :config
  (setq agent-shell-show-welcome-message nil
        agent-shell-preferred-agent-config 'claude-code)
  (require 'acp))

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
