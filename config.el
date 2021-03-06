;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Filipe Regadas"
      user-mail-address "filiperegadas@gmail.com")

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
      doom-font (font-spec :family "Iosevka Term" :size 16)
      doom-themes-enable-bold t
      doom-themes-enable-italic t)

(setq-default line-spacing 3)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; Prevents some cases of Emacs flickering
;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil
      scroll-conservatively 101)

(setq company-idle-delay 0.2)
;; company-minimum-prefix-length 0)

(auto-save-visited-mode +1)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

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

(after! treemacs
  (setq treemacs-git-mode 'deferred
        treemacs-collapse-dirs 20))

(after! treemacs-lsp
  (lsp-treemacs-sync-mode 1))

(after! projectile
  (setq projectile-project-search-path '("~/projects/" "~/projects/spotify" "~/projects/experiments")
        projectile-project-root-files-bottom-up '(".projectile" ".git")))

(setq-default TeX-engine 'xetex
              pdf-latex-command "xelatex")

;; dhall-mode highlight the syntax and run dhall format on save
(use-package! dhall-mode
  :config
  (setq
   ;; uncomment the next line to disable automatic format
   ;; dhall-format-at-save nil

   ;; comment the next line to use unicode syntax
   dhall-format-arguments (\` ("--ascii"))

   ;; header-line is obsoleted by lsp-mode
   dhall-use-header-line nil)
  (set-repl-handler! 'dhall-mode #'dhall-repl-show)
  (map! :after dhall-mode
        :map dhall-mode-map
        :localleader
        "l" #'dhall-lint
        "f" #'dhall-freeze
        "t" #'dhall-buffer-type-show))

;; lsp-mode provides the lsp client and it configure flymake to explain errors
(use-package! lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((dhall-mode . lsp))
  :commands lsp)

(use-package! kubernetes
  :init (progn
          (setq kubernetes-overview-custom-views-alist '((overview . (context statefulsets deployments services)))))
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(after! go-mode
  (if (featurep! +lsp)
      (add-hook 'go-mode-hook #'lsp-deferred)
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)))

(after! lsp-mode
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-domains
               '("ghe.spotify.net" . "github")))
