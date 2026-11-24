;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Core -----------------------------------------------------------------------

(setq user-full-name "Filipe Regadas"
      user-mail-address "oss@regadas.email")

(setq auth-sources '("~/.authinfo.gpg"))

(setq doom-theme 'modus-operandi
      ;; doom-font (font-spec :family "Aporetic Sans Mono" :size 15)
      doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 15))

(setq mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t)

(after! modus-themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t))

(setq inhibit-compacting-font-caches t
      auto-save-default t
      jit-lock-defer-time 0.05)

(setq display-line-numbers-type 'relative)

(after! image
  (setq image-use-external-converter t))

(after! image-converter
  (setq image-converter 'imagemagick)
  (setf (alist-get 'imagemagick image-converter--converters)
        '(:command "magick"
          :probe ("-list" "format"))))

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

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-hud t               ; top-of-window position indicator
        doom-modeline-buffer-encoding 'nondefault))  ; only show when non-utf-8

(use-package! nyan-mode
  :after doom-modeline
  :config
  (nyan-mode 1))

(after! corfu
  (setq corfu-auto-delay 0.3))

(after! diff-hl
  (setq diff-hl-flydiff-delay 2
        diff-hl-draw-borders nil)

  ;; Modus styles diff-hl with background colors, but vc-gutter's +pretty flag
  ;; makes those backgrounds transparent.  Use the corresponding Modus colors
  ;; as fringe bitmap foregrounds instead.
  (defun +my/diff-hl-use-modus-fringe-colors-h (&rest _)
    (let* ((faces '((diff-hl-insert . bg-added-fringe)
                    (diff-hl-change . bg-changed-fringe)
                    (diff-hl-delete . bg-removed-fringe)))
           (modus-theme (and (fboundp 'modus-themes-get-current-theme)
                             (modus-themes-get-current-theme))))
      (if (and modus-theme
               (fboundp 'modus-themes-get-color-value)
               (eq (car custom-enabled-themes) modus-theme))
          (dolist (face faces)
            (face-spec-set
             (car face)
             `((t :foreground
                  ,(modus-themes-get-color-value (cdr face) :with-overrides)))
             'face-override-spec))
        (dolist (face faces)
          (face-spec-set (car face) nil 'face-override-spec)))
      (when (fboundp '+vc-gutter-make-diff-hl-faces-transparent-h)
        (+vc-gutter-make-diff-hl-faces-transparent-h))))

  (add-hook 'doom-load-theme-hook #'+my/diff-hl-use-modus-fringe-colors-h)
  (add-hook 'disable-theme-functions #'+my/diff-hl-use-modus-fringe-colors-h)
  (+my/diff-hl-use-modus-fringe-colors-h))

;;; Projects / VCS -------------------------------------------------------------

(after! projectile
  (setq projectile-project-search-path '("~/src/" "~/src/spotify" "~/src/experiments")
        projectile-project-root-files-bottom-up '(".projectile" ".git")
        projectile-enable-caching t))

(after! git-link
  (add-to-list 'git-link-remote-alist
               '("^ghe\\.spotify\\.net$" git-link-github))
  (add-to-list 'git-link-commit-remote-alist
               '("^ghe\\.spotify\\.net$" git-link-commit-github))
  (add-to-list 'git-link-homepage-remote-alist
               '("^ghe\\.spotify\\.net$" git-link-homepage-github)))

(after! forge
  (push '("ghe.spotify.net" "ghe.spotify.net/api/v3" "ghe.spotify.net" forge-github-repository)
        forge-alist))

;; --- Monorepo performance: lightweight magit for large repos ---
(defvar +my/magit-large-repo-dirs '("~/src/spotify/services-pilot-worktrees/")
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
        lsp-semantic-tokens-enable nil
        lsp-enable-links nil
        lsp-enable-file-watchers nil ; jdtls re-enabled selectively, see after! lsp-java
        lsp-file-watch-threshold 10000
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
        ;; lsp-log-io nil
        lsp-auto-guess-root t
        lsp-idle-delay 0.25
        lsp-response-timeout 10
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
        lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.60.0/jdt-language-server-1.60.0-202606262232.tar.gz"
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
    "Remove a stale .metadata/.lock file in WS-DIR if no jdtls owns it.
Limit: only THIS Emacs session's workspaces are visible; a lock held
by another Emacs instance cannot be detected (and will be removed)."
    (let ((lock (expand-file-name ".metadata/.lock" ws-dir)))
      (when (and (file-exists-p lock)
                 (not (seq-some (lambda (ws)
                                  (and (eq (lsp--client-server-id
                                            (lsp--workspace-client ws))
                                           'jdtls)
                                       (string= ws-dir
                                                (my/lsp-java--workspace-dir-for-root
                                                 (lsp--workspace-root ws)))))
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

  ;; NOTE: mutate slots via `cl-struct-slot-value' rather than the per-slot
  ;; accessors (e.g. `(setf (lsp--client-multi-root c) ...)').  The accessor
  ;; `setf' relies on the accessor's cl-defstruct compiler-macro being
  ;; resolvable at macro-expansion time; during the *cold* load of lsp-java
  ;; (when this `after!' body first runs via `eval-after-load') that resolution
  ;; can fail, so `gv' falls back to the non-existent named setter and signals
  ;;   Symbol's function definition is void: \(setf lsp--client-multi-root\)
  ;; `cl-struct-slot-value' has a `gv-expander' baked into cl-macs (which is
  ;; always loaded before any `setf' can expand), so it is resolvable on every
  ;; macro-expansion and avoids the race entirely.
  ;; A failing slot patch must not silently skip the rest of this body (every
  ;; project server would then share one workspace) — warn and carry on.
  (condition-case err
      (when-let ((jdtls-client (gethash 'jdtls lsp-clients)))
        ;; (1) Disable multi-root
        (setf (cl-struct-slot-value 'lsp--client 'multi-root jdtls-client) nil)

        ;; (2) Per-project library/cache folder
        (setf (cl-struct-slot-value 'lsp--client 'library-folders-fn jdtls-client)
              (lambda (workspace)
                (list (expand-file-name
                       ".cache/"
                       (my/lsp-java--workspace-dir-for-root
                        (lsp--workspace-root workspace))))))

        ;; (3) Isolated workspaceFolders in initialization-options
        (let ((orig-init-fn (lsp--client-initialization-options jdtls-client)))
          (setf (cl-struct-slot-value 'lsp--client 'initialization-options jdtls-client)
                (lambda ()
                  (let ((opts (funcall orig-init-fn)))
                    (plist-put opts :workspaceFolders
                               (vector (lsp--path-to-uri
                                        (or (projectile-project-root)
                                            default-directory)))))))))
    (error (warn "jdtls client patch failed: %S" err)))

  ;; --- Dependency buffer attachment fix -------------------------------------
  ;; `lsp' normally tries a library workspace only when the file is outside all
  ;; persisted project roots.  Our per-project cache lives under ~/.emacs.d,
  ;; which may itself be a persisted root, so dependency buffers otherwise
  ;; start a new jdtls there instead of reusing the originating project.
  (defun my/lsp-java--library-workspace-for-file (session file)
    "Return the active jdtls workspace in SESSION whose library contains FILE."
    (when (and session file)
      (seq-find
       (lambda (workspace)
         (let* ((client (lsp--workspace-client workspace))
                (library-folders-fn
                 (lsp--client-library-folders-fn client)))
           (and (eq (lsp--client-server-id client) 'jdtls)
                (lsp--supports-buffer? client)
                library-folders-fn
                (seq-some (lambda (folder)
                            (lsp-f-ancestor-of? folder file))
                          (funcall library-folders-fn workspace)))))
       (lsp--session-workspaces session))))

  (defvar my/lsp-java--opening-library-workspace nil
    "The jdtls library workspace selected while `lsp' opens a dependency file.")

  (defadvice! my/lsp-java--prefer-library-workspace-a (orig-fn &rest args)
    :around #'lsp
    (let ((my/lsp-java--opening-library-workspace
           (and buffer-file-name
                (my/lsp-java--library-workspace-for-file
                 (lsp-session) buffer-file-name))))
      (apply orig-fn args)))

  (defadvice! my/lsp-java--ignore-session-root-for-library-a
    (orig-fn session file &rest args)
    :around #'lsp-find-session-folder
    (if (and my/lsp-java--opening-library-workspace
             buffer-file-name
             file
             (lsp-f-same? file buffer-file-name))
        nil
      (apply orig-fn session file args)))

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

  ;; --- jdt:// URI resolution fix ----------------------------------------------
  ;; Stock `lsp-java--resolve-uri' breaks go-to-definition into dependencies:
  ;;   1. It reads/writes the GLOBAL `lsp-java-workspace-cache-dir' (the
  ;;      ls-command advice below only rebinds it during server startup), so
  ;;      every project and every jdtls version share one cache -- and
  ;;      `lsp-java--get-filename' has no jar/version in the name, so
  ;;      different dependency versions collide on the same file.
  ;;   2. It never refreshes an existing cache file, so after a rebuild or
  ;;      dependency bump jdtls' line numbers point into stale text
  ;;      (right file, wrong line).
  ;;   3. (lsp-find-workspace 'jdtls nil) grabs the FIRST jdtls instance in
  ;;      the session, which with multi-root disabled can be another
  ;;      project's server (wrong classpath).
  (defvar my/lsp-java--resolve-uri-times (make-hash-table :test #'equal)
    "URI -> last `java/classFileContents' fetch time, to avoid re-fetch storms.")

  (defadvice! my/lsp-java--resolve-uri-a (uri)
    "Resolve jdt:// URI into the per-project cache dir, refreshing stale content."
    :override #'lsp-java--resolve-uri
    (let* ((workspace
            (or (and (bound-and-true-p lsp--cur-workspace)
                     (eq (lsp--client-server-id
                          (lsp--workspace-client lsp--cur-workspace))
                         'jdtls)
                     lsp--cur-workspace)
                (seq-find
                 (lambda (ws)
                   (eq (lsp--client-server-id (lsp--workspace-client ws))
                       'jdtls))
                 (bound-and-true-p lsp--buffer-workspaces))
                (and buffer-file-name
                     (lsp-find-workspace 'jdtls buffer-file-name))
                (lsp-find-workspace 'jdtls default-directory)
                (lsp-find-workspace 'jdtls nil)))
           (root (or (and workspace (lsp--workspace-root workspace))
                     (projectile-project-root)
                     default-directory))
           (cache-dir (expand-file-name
                       ".cache/" (my/lsp-java--workspace-dir-for-root root)))
           ;; `lsp-java--get-filename' drops the JAR/version component, so
           ;; two versions of one class would share a single cache file;
           ;; putting the URI's md5 in a per-URI subdirectory keeps the
           ;; visible filename readable while staying unique per URI.
           (file-location (expand-file-name
                           (lsp-java--get-filename uri)
                           (expand-file-name (md5 uri) cache-dir)))
           (last-fetch (gethash uri my/lsp-java--resolve-uri-times))
           (fresh (and last-fetch
                       (file-readable-p file-location)
                       (< (float-time (time-subtract nil last-fetch)) 60))))
      (unless fresh
        (condition-case err
            (let ((content (with-lsp-workspace workspace
                             (lsp-send-request
                              (lsp-make-request
                               "java/classFileContents" (list :uri uri))))))
              (lsp-java--ensure-dir (file-name-directory file-location))
              (unless (and (file-readable-p file-location)
                           (string= content
                                    (with-temp-buffer
                                      (insert-file-contents file-location)
                                      (buffer-string))))
                (with-temp-file file-location (insert content))
                (with-temp-file (lsp-java--get-metadata-location file-location)
                  (insert uri))
                ;; Revert any live buffer so it shows the fresh content.
                (when-let ((buf (get-file-buffer file-location)))
                  (with-current-buffer buf
                    (revert-buffer :ignore-auto :noconfirm :preserve-modes))))
              (puthash uri (current-time) my/lsp-java--resolve-uri-times))
          (error
           ;; Fall back to the cached copy if the request failed.
           (unless (file-readable-p file-location)
             (signal (car err) (cdr err))))))
      file-location))

  ;; --- File watchers for jdtls only -------------------------------------------
  ;; jdtls registers workspace/didChangeWatchedFiles watchers (*.java, pom.xml,
  ;; *.gradle, .project...).  With `lsp-enable-file-watchers' nil that
  ;; registration is silently dropped, so jdtls never hears about external
  ;; changes (builds, git pull, codegen) and its index goes stale until a full
  ;; rebuild.  Honor watcher registrations for jdtls; all other servers keep
  ;; the global nil.
  (defadvice! my/lsp--enable-watchers-for-jdtls-a (orig-fn &rest args)
    :around #'lsp--server-register-capability
    (let ((lsp-enable-file-watchers
           (or lsp-enable-file-watchers
               (and lsp--cur-workspace
                    (eq (lsp--client-server-id
                         (lsp--workspace-client lsp--cur-workspace))
                        'jdtls)))))
      (apply orig-fn args)))

  ;; --- Command builder advice ------------------------------------------------
  ;; Bind both lsp-java-workspace-dir and lsp-java-workspace-cache-dir to
  ;; per-project paths around the server command construction.
  (if (fboundp 'lsp-java--ls-command)
      (defadvice! my/lsp-java--ls-command-a (orig-fn)
        "Bind `lsp-java-workspace-dir' and `lsp-java-workspace-cache-dir'
to per-project paths and remove any stale lock beforehand."
        :around #'lsp-java--ls-command
        (let* ((lsp-java-workspace-dir (my/lsp-java--workspace-dir))
               (lsp-java-workspace-cache-dir (expand-file-name ".cache/"
                                                               lsp-java-workspace-dir)))
          (when (file-directory-p lsp-java-workspace-dir)
            (my/lsp-java--remove-stale-lock lsp-java-workspace-dir))
          (funcall orig-fn)))
    (warn "lsp-java--ls-command missing; per-project jdtls workspace advice not installed"))

  ;; --- Interactive clean command ---------------------------------------------
  (defun my/lsp-java-clean-workspace ()
    "Delete the current project's jdtls workspace, first shutting down
any live jdtls that uses it.  A fresh jdtls starts on the next Java
file open.  Use this when jdtls fails to start due to a corrupted
workspace."
    (interactive)
    (let ((ws-dir (my/lsp-java--workspace-dir)))
      (unless (file-directory-p ws-dir)
        (user-error "No jdtls workspace directory found at %s" ws-dir))
      (when (y-or-n-p (format "Delete jdtls workspace %s?" ws-dir))
        (let ((live-ws
               (seq-filter
                (lambda (ws)
                  (and (eq (lsp--client-server-id
                            (lsp--workspace-client ws))
                           'jdtls)
                       (string= ws-dir
                                (my/lsp-java--workspace-dir-for-root
                                 (lsp--workspace-root ws)))))
                (lsp--session-workspaces (lsp-session)))))
          (dolist (ws live-ws)
            (lsp-workspace-shutdown ws))
          ;; Bounded wait for the server processes to exit: deleting under a
          ;; live jdtls corrupts its workspace mid-delete, so abort if any
          ;; process survived the wait.
          (let ((deadline (+ (float-time) 10)))
            (while (and live-ws
                        (< (float-time) deadline)
                        (seq-some (lambda (ws)
                                    (process-live-p (lsp--workspace-proc ws)))
                                  live-ws))
              (accept-process-output nil 0.1)))
          (when (seq-some (lambda (ws)
                            (process-live-p (lsp--workspace-proc ws)))
                          live-ws)
            (user-error "jdtls did not exit within 10s; workspace not deleted — kill it manually and retry"))
          (delete-directory ws-dir t)
          (message "Deleted %s. A fresh jdtls starts on the next Java file open." ws-dir))
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
  (reformatter-define dhall-freeze-all
    :program dhall-command
    :args '("freeze" "--all")
    :lighter " DhFreezeAll")
  (map! :map dhall-mode-map
        :localleader
        "l" #'dhall-lint-buffer
        "ff" #'dhall-freeze-buffer
        "fa" #'dhall-freeze-all-buffer))

;; PR Review
(use-package! pr-review
  :commands (pr-review pr-review-open-url)
  :config
  (setq pr-review-forges-alist
        '(("ghe.spotify.net" . (github "ghe.spotify.net/api/v3" nil))
          ("github.com" . (github nil nil))
          ("gitlab.com" . (gitlab nil nil))))
  ;; Use gh CLI token (classic OAuth with repo scope) — fine-grained
  ;; tokens don't support the /compare/ endpoint on GHE 3.19.  The
  ;; token must be sent to ghe.spotify.net ONLY; every other forge
  ;; uses normal ghub auth-source credentials.
  (defvar my/pr-review-ghe-token
    (let ((token (with-temp-buffer
                   ;; call-process signals file-error when gh itself is
                   ;; missing, and returns a signal-description STRING
                   ;; (not a number) when the child dies by a signal — so
                   ;; use (eq 0 …), which tolerates both, never signals,
                   ;; and only accepts a genuine zero exit status.
                   (when (eq 0 (condition-case nil
                                   (call-process "gh" nil '(t nil) nil
                                                 "auth" "token"
                                                 "--hostname" "ghe.spotify.net")
                                 (error -1)))
                     (string-trim (buffer-string))))))
      (unless (string-empty-p (or token "")) token))
    "gh CLI token for ghe.spotify.net, or nil when gh is missing, fails,
or prints no token (GHE then falls back to auth-source too).")
  (defadvice! my/pr-review--ghub-auth-ghe-only-a (orig-fn)
    "Send the gh CLI token only on ghe.spotify.net requests.
All other hosts — and GHE itself when the token is unavailable —
keep ghub's normal auth-source lookup via `pr-review-ghub-auth-name'."
    :around #'pr-review--ghub-common-request-args
    (let* ((args (funcall orig-fn))
           (host (plist-get args :host)))
      (if (and my/pr-review-ghe-token
               host
               (string-prefix-p "ghe.spotify.net" host))
          (plist-put args :auth my/pr-review-ghe-token)
        args))))

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
  (require 'reformatter)
  (reformatter-define d2-format
    :program "d2"
    :stdin nil
    :stdout nil
    :args (list "fmt" input-file)
    :lighter " D2Fmt"))

;; Org
(setq org-directory "~/src/brain-dump")

(use-package! ox-awesomecv
  :after org)

(after! org
  (setq org-latex-compiler "xelatex"))

;; Markdown
(after! markdown-mode
  (setq markdown-hide-markup t))

(after! grip-mode
  (setq grip-command 'mdopen)

  (defun my/grip-wait-for-server-a (&rest _)
    "Wait until mdopen is listening before sending xwidget to its URL."
    (when (eq grip--command 'mdopen)
      (let ((deadline (+ (float-time) grip-sleep-time))
            connection)
        (while (and (not connection)
                    (< (float-time) deadline))
          (setq connection
                (condition-case nil
                    (open-network-stream
                     "grip-server-probe" nil grip-preview-host grip--port)
                  (file-error nil)))
          (unless connection
            (sleep-for 0.05)))
        (when connection
          (delete-process connection)))))

  ;; `grip-mode' opens xwidget before mdopen is always ready.  The failed
  ;; navigation leaves xwidget displaying the preceding Markdown file.
  (advice-remove #'grip--preview-1 #'my/grip-wait-for-server-a)
  (advice-add #'grip--preview-1 :before #'my/grip-wait-for-server-a)

  ;; Keep xwidget previews in a stable right-hand side window.
  (set-popup-rule! "^\\*xwidget-webkit:"
    :side 'right
    :size 0.45
    :select t
    :quit nil
    :ttl nil)

  ;; Browse an existing preview instead of toggling its process off.
  (map! :map markdown-mode-map
        :localleader
        "p" #'grip-browse-preview))


;;; Tools ----------------------------------------------------------------------

(defun +ghostel/add-to-workspace-h ()
  "Add the current Ghostel buffer to the active Doom workspace."
  (when (and (bound-and-true-p persp-mode)
             (fboundp 'persp-add-buffer)
             (fboundp 'get-current-persp)
             (get-current-persp))
    (persp-add-buffer (current-buffer) (get-current-persp) nil nil)))

(after! ghostel
  ;; Avoid native-module re-entrant redraws from Doom's real-buffer handling.
  (setq doom-real-buffer-modes (delq 'ghostel-mode doom-real-buffer-modes))
  (add-hook 'ghostel-mode-hook #'+ghostel/add-to-workspace-h))

(use-package! kubel
  :commands (kubel))

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
  (setq consult-gh-default-clone-directory "~/src/"))

(use-package! agent-shell
  :defer t
  :commands (agent-shell
             agent-shell-anthropic-start-claude-code
             agent-shell-google-start-gemini
             agent-shell-openai-start-codex
             agent-shell-pi-start-agent
             agent-shell-switch-buffer
             agent-shell-new-worktree-shell
             agent-shell-fork
             agent-shell-send-dwim
             agent-shell-toggle)
  :config
  (setq agent-shell-show-welcome-message nil
        agent-shell-header-style 'text
        agent-shell-preferred-agent-config 'pi
        ;; Only deliberate context (files, active region). Drop implicit
        ;; `error'/`line' at-point sources — they inject incidental noise
        ;; (e.g. markdownlint diagnostics) into new shells.
        agent-shell-context-sources '(files region)
        ;; Reopen a picked session with its full transcript replayed; the
        ;; default `minimal' uses session/resume: title only, no history.
        agent-shell-session-restore-verbosity 'full
        ;; Compact one-line usage at turn end + cumulative cost in header.
        agent-shell-show-usage-at-turn-end t
        agent-shell-usage-expand-by-default t
        agent-shell-usage-format-function #'agent-shell-usage-one-line-format
        agent-shell-show-cost-indicator t
        ;; Avoid slow project file scans on `@' in large repos; `/' still pops
        ;; and `@' stays available on demand via `completion-at-point'.
        agent-shell-file-completion-enabled nil
        ;; Static ASCII text avoids blank/fallback frames shifting header height.
        agent-shell-busy-indicator-frames "(busy)")
  ;; Claim new shells for the workspace they're created in. Shell buffers
  ;; are often only ever shown via `display-buffer' (never switched to),
  ;; so Doom's `doom-switch-buffer-hook' misses them and they end up in
  ;; no persp.
  (when (modulep! :ui workspaces)
    (add-hook 'agent-shell-mode-hook #'+workspaces-add-current-buffer-h))
  ;; Switch to the workspace owning the selected shell before displaying it,
  ;; so `switch-to-buffer' doesn't pull another workspace's shell into this one.
  (defadvice! my/agent-shell-switch-buffer-workspace-a (&rest _)
    "Switch to the workspace containing the chosen agent-shell buffer."
    :override #'agent-shell-switch-buffer
    (let* ((shell-buffer (agent-shell--read-shell-buffer
                          :prompt "Switch to agent-shell buffer: "))
           (target-buffer (or (when (bound-and-true-p agent-shell-prefer-viewport-interaction)
                                (agent-shell-viewport--buffer
                                 :shell-buffer shell-buffer
                                 :existing-only t))
                              shell-buffer)))
      (when (and (bound-and-true-p persp-mode)
                 (not (+workspace-contains-buffer-p shell-buffer)))
        (when-let* ((ws (cl-find-if (lambda (w) (+workspace-contains-buffer-p shell-buffer w))
                                    (+workspace-list))))
          (+workspace-switch (persp-name ws))))
      (if-let* ((win (get-buffer-window target-buffer)))
          (select-window win)
        (switch-to-buffer target-buffer)))))

(use-package! agent-shell-macext
  :after agent-shell
  :hook (agent-shell-mode . agent-shell-macext-setup)
  :custom
  (agent-shell-macext-file-copy-policy 'auto)
  (agent-shell-macext-notifications t)
  (agent-shell-macext-notify-current-buffer nil))

(use-package! agent-shell-manager
  :defer t
  :commands (agent-shell-manager-toggle)
  :after agent-shell
  :config
  (setq agent-shell-manager-side 'bottom)
  ;; Keep Doom's leader and window bindings available in the manager while
  ;; preserving its native management keys in Evil normal state.
  (evil-set-initial-state 'agent-shell-manager-mode 'normal)
  (map! :map agent-shell-manager-mode-map
        :n "RET"     #'agent-shell-manager-goto
        :n "g"       #'agent-shell-manager-refresh
        :n "k"       #'agent-shell-manager-kill
        :n "c"       #'agent-shell-manager-new
        :n "r"       #'agent-shell-manager-restart
        :n "d"       #'agent-shell-manager-delete-killed
        :n "m"       #'agent-shell-manager-set-mode
        :n "M"       #'agent-shell-manager-set-model
        :n "t"       #'agent-shell-manager-view-traffic
        :n "l"       #'agent-shell-manager-toggle-logging
        :n "C-c C-c" #'agent-shell-manager-interrupt
        :n "q"       #'quit-window)
  ;; Surface session titles right after Status (index 1 stays Status so
  ;; `agent-shell-manager--entries' killed-to-bottom sort keeps working).
  (defvar-local my/agent-shell-manager--subscribed nil)
  (defun my/agent-shell-manager--watch-shell (&optional buf)
    "Refresh the manager immediately on status/title/model events in BUF."
    (let ((buf (or buf (current-buffer))))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (derived-mode-p 'agent-shell-mode)
                        (boundp 'agent-shell--state)
                        (not my/agent-shell-manager--subscribed))))
        (with-current-buffer buf
          (setq my/agent-shell-manager--subscribed t)
          (agent-shell-subscribe-to
           :shell-buffer buf
           :on-event
           (lambda (ev)
             (when (memq (map-elt ev :event)
                         '(input-submitted turn-complete permission-request
                           permission-response config-option-update
                           session-title-changed init-finished error clean-up))
               (run-at-time 0 nil #'agent-shell-manager-refresh))))))))
  (add-hook 'agent-shell-mode-hook #'my/agent-shell-manager--watch-shell)
  (add-hook! 'agent-shell-manager-mode-hook
    (setq tabulated-list-format
          [("Buffer" 32 t)
           ("Status" 11 t)
           ("Title" 45 t)
           ("Mode" 12 t)
           ("Model" 24 t)
           ("Perms" 7 t)
           ("Path" 20 t)])
    (tabulated-list-init-header)
    ;; Upstream wipes buffer-locals before cancelling its old timer on re-entry.
    (dolist (tm (copy-sequence timer-list))
      (when (and (eq (timer--function tm) #'agent-shell-manager-refresh)
                 (not (eq tm agent-shell-manager--refresh-timer)))
        (cancel-timer tm)))
    (dolist (buf (agent-shell-buffers))
      (my/agent-shell-manager--watch-shell buf)))
  ;; Upstream treats any non-empty `:tool-calls' (even completed ones) as
  ;; "working" and returns "unknown" before `:session :id' is set. Delegate
  ;; live status to `agent-shell-status' instead.
  (defadvice! my/agent-shell-manager-get-status-a (buffer)
    "Derive BUFFER status from live processes and `agent-shell-status'."
    :override #'agent-shell-manager--get-status
    (with-current-buffer buffer
      (if (not (boundp 'agent-shell--state))
          "unknown"
        (let* ((state agent-shell--state)
               (acp-proc (map-nested-elt state '(:client :process)))
               (acp-alive (and (processp acp-proc) (process-live-p acp-proc)))
               (comint-proc (get-buffer-process (current-buffer)))
               (comint-alive (and (processp comint-proc) (process-live-p comint-proc))))
          (cond
           ((or (not comint-alive)
                (and (map-elt state :client) (not acp-alive)))
            "killed")
           ((not (map-nested-elt state '(:session :id)))
            "initializing")
           (t
            (pcase (agent-shell-status)
              ('blocked "waiting")
              ('busy "working")
              ('ready "ready")
              (_ "unknown"))))))))
  ;; Upstream only reads legacy `:session :model-id'/`:mode-id'; modern ACP
  ;; agents (like Pi) report these via `:config-options'.
  (defadvice! my/agent-shell-manager-get-model-a (buffer)
    "Return current model name for BUFFER, supporting ACP `:config-options'."
    :override #'agent-shell-manager--get-model-id
    (with-current-buffer buffer
      (or (and (boundp 'agent-shell--state)
               (agent-shell-get-model-name agent-shell--state))
          "-")))
  (defadvice! my/agent-shell-manager-get-mode-a (buffer)
    "Return current session mode name for BUFFER, supporting ACP `:config-options'."
    :override #'agent-shell-manager--get-session-mode
    (with-current-buffer buffer
      (or (and (boundp 'agent-shell--state)
               (agent-shell-get-mode-name agent-shell--state))
          "-")))
  (defadvice! my/agent-shell-manager-entries-title-a (entries)
    "Insert each buffer's session title as the third column."
    :filter-return #'agent-shell-manager--entries
    (mapcar (lambda (entry)
              (let* ((buf (car entry))
                     (cols (cadr entry))
                     (raw (and (buffer-live-p buf)
                               (with-current-buffer buf
                                 (and (boundp 'agent-shell--state)
                                      (map-nested-elt agent-shell--state
                                                      '(:session :title))))))
                     (title (if (and (stringp raw)
                                     (not (string-empty-p (string-trim raw))))
                                (propertize (car (split-string (string-trim raw) "\n" t))
                                            'face 'agent-shell-session-title)
                              "-")))
                (list buf (vconcat (seq-subseq cols 0 2)
                                   (vector title)
                                   (seq-subseq cols 2)))))
            entries))
  ;; Switch to the workspace owning the selected shell before visiting it,
  ;; since `agent-shell-manager-goto' only inspects windows in the current one.
  (defadvice! my/agent-shell-manager-goto-workspace-a (fn &rest args)
    "Switch to the workspace containing the shell buffer before visiting it."
    :around #'agent-shell-manager-goto
    (let ((buffer (tabulated-list-get-id)))
      (if-let* (((and buffer
                      (buffer-live-p buffer)
                      (bound-and-true-p persp-mode)
                      (not (+workspace-contains-buffer-p buffer))))
                (workspace
                 (cl-find-if (lambda (ws) (+workspace-contains-buffer-p buffer ws))
                             (+workspace-list))))
          (progn
            (agent-shell-manager--hide-window)
            (+workspace-switch (persp-name workspace))
            (if-let* ((win (get-buffer-window buffer)))
                (select-window win)
              (if-let* ((agent-win
                         (cl-find-if
                          (lambda (w)
                            (with-current-buffer (window-buffer w)
                              (derived-mode-p 'agent-shell-mode)))
                          (window-list))))
                  (progn
                    (set-window-buffer agent-win buffer)
                    (select-window agent-win))
                (agent-shell--display-buffer buffer))))
        (apply fn args)))))

(use-package! agent-review
  :defer t
  :commands (agent-review)
  :after agent-shell)

(map! :leader
      :prefix ("o c a" . "agent-shell")
      :desc "Start agent-shell"        "a" #'agent-shell
      :desc "Switch buffer"            "b" #'agent-shell-switch-buffer
      :desc "Start Claude Code (ACP)"  "c" #'agent-shell-anthropic-start-claude-code
      :desc "Fork session"             "f" #'agent-shell-fork
      :desc "Start Gemini (ACP)"       "g" #'agent-shell-google-start-gemini
      :desc "Toggle shell manager"     "m" #'agent-shell-manager-toggle
      :desc "Start Pi (ACP)"           "p" #'agent-shell-pi-start-agent
      :desc "Review git changes"       "r" #'agent-review
      :desc "Send DWIM"                "s" #'agent-shell-send-dwim
      :desc "Toggle shell"             "t" #'agent-shell-toggle
      :desc "New worktree shell"       "w" #'agent-shell-new-worktree-shell
      :desc "Start Codex (ACP)"        "x" #'agent-shell-openai-start-codex)

(use-package! rainbow-csv
  :hook ((csv-mode . rainbow-csv-mode)
         (tsv-mode . rainbow-csv-mode)))

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
