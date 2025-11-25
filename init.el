;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(setq auto-save-default nil
      backup-by-copying t
      backup-directory-alist `(("." . "~/.saves"))
      custom-file (concat user-emacs-directory "custom.el")
      default-cursor-type 'bar
      delete-by-moving-to-trash t
      dired-use-ls-dired t
      find-program "gfind"
      gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      help-window-select t
      inhibit-startup-message t
      insert-directory-program "gls"
      load-prefer-newer t
      mac-command-modifier 'meta
      mac-option-modifier 'super
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      ns-right-command-modifier 'super
      org-blank-before-new-entry nil
      require-final-newline t
      ring-bell-function 'ignore
      trash-directory "~/.Trash/emacs"
      use-package-verbose t
      visible-bell t)

(setq-default column-number-mode t)
(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)

(if (file-exists-p custom-file)
    (load custom-file))

;; Functionality
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(windmove-default-keybindings)
(winner-mode 1)

;; Appearance
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 200))
(set-face-attribute 'default nil :family "Monaco" :height 150)
(set-face-attribute 'region nil :background "#67930F" :foreground "#FFFFFF")

;; Hooks
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'use-package)
(require 'diminish)

(use-package core-fns
  :load-path "./lisp"
  :config
  ;; Bindings
  (global-set-key (kbd "C-c d") 'custom-duplicate-line)
  (global-set-key (kbd "C-c n") 'dev-notes)
  (global-set-key (kbd "C-a")   'custom-move-beginning-of-line)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  (global-set-key (kbd "C-c R") 'revert-all-buffers)
  (global-set-key (kbd "C-x M-d") 'lsp-ui-doc-show-at-point))

(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-galaxy t))

(use-package iimage :ensure t)
(use-package flycheck :ensure t :diminish flycheck-mode)
(use-package flycheck-joker :ensure t)
(use-package flycheck-clj-kondo :ensure t)
(use-package sudo-edit :ensure t)
(use-package direnv :ensure t)
(use-package browse-at-remote :ensure t)
(use-package restclient :ensure t)
(use-package company :ensure t :diminish company-mode)
(use-package rainbow-delimiters :ensure t)
(use-package yaml-mode :ensure t)
;; (use-package treemacs-projectile :ensure t)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :hook ((clojure-mode . smartparens-mode)
	 (emacs-lisp-mode . smartparens-mode)
	 (lisp-mode . smartparens-mode)
	 (eval-expression-minibuffer-setup . smartparens-mode))
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package ido-completing-read+ :ensure t)
(use-package flx-ido :ensure t)
(use-package ido-vertical-mode :ensure t)
(use-package ido
  :ensure t
  :init
  (setq ido-enable-prefix nil
	ido-enable-flex-matching t
	ido-max-prospects 10
	ido-auto-merge-work-directories-length -1
	ido-use-faces nil
	ido-vertical-define-keys 'C-n-C-p-up-down-left-right
	org-completion-use-ido t)
  :config
  (ido-mode 1)
  (ido-ubiquitous-mode 1)
  (ido-vertical-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode +1)
  (add-to-list 'ido-ignore-files "\\.DS_Store"))

(use-package amx
  :ensure t
  :config
  (amx-initialize)
  :init
  (global-set-key (kbd "M-x") 'amx))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
	uniquify-separator "/"
	uniquify-after-kill-buffer-p t
	uniquify-ignore-buffers-re "^\\*"))

;; (use-package projectile
;;   :ensure t
;;   :diminish projectile-mode
;;   :init
;;   (setq projectile-enable-caching t
;; 	projectile-mode-line-lighter "P"
;; 	persp-initial-frame-name "m")
;;   :config
;;   (projectile-global-mode)
;;   (global-set-key (kbd "s-f") 'projectile-find-file))

(use-package cider
  :ensure t
  :init
  (setq nrepl-hide-special-buffers t
	;;cider-redirect-server-output-to-repl t
	;;cider-interactive-eval-output-destination 'output-buffer
	cider-auto-mode nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-wrap-history t
        cider-prompt-save-file-on-load nil
        cider-repl-use-clojure-font-lock t
        cider-overlays-use-font-lock t
	cider-clojure-cli-global-options "-A:perf"
	cider-test-defining-forms '("deftest" "defspec" "defflow" "defschematest")))

(use-package inf-clojure
  :ensure t
  ;; :init
  ;; (setq inf-clojure-auto-mode nil)
  )

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package highlight-numbers
  :diminish highlight-numbers-mode
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package clojure-mode
  :ensure t
  :init
  (require 'smartparens-clojure)
  (add-to-list 'auto-mode-alist '("\\.repl\\'" . clojure-mode))
  :hook ((clojure-mode . rainbow-delimiters-mode)
	 (clojure-mode . turn-on-eldoc-mode)
	 (clojure-mode . flycheck-mode)
	 ;; (clojure-mode . lsp)
	 (clojure-mode . cider-mode)
	 ))

(use-package magit
  :ensure t
  :config
  (define-prefix-command 'magit-map)
  (global-set-key (kbd "C-c g") 'magit-map)
  (define-key magit-map (kbd "s") 'magit-status)
  (define-key magit-map (kbd "g") 'custom-git-grep)
  (define-key magit-map (kbd "b") 'magit-blame)
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package magit-ido
  :ensure t
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package discover-my-major
  :ensure t
  :config
  (global-set-key (kbd "C-h C-m") 'discover-my-major))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :diminish lsp-lens-mode
  :init
  (setq lsp-auto-guess-root t
	company-minimum-prefix-length 1
	lsp-lens-enable t
	lsp-signature-auto-activate nil
	lsp-headerline-breadcrumb-enable nil
	;;treemacs-space-between-root-nodes nil
	))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 0.75
	lsp-ui-doc-show-with-cursor nil
	lsp-ui-doc-show-with-mouse nil)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] 'lsp-ui-peek-find-references))

;; (use-package treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list
;;   :config
;;   (global-set-key (kbd "C-x t t") 'treemacs))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; (use-package lsp-treemacs
;;   :ensure t
;;   :config
;;   (lsp-treemacs-sync-mode 1))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook ((lsp-mode . yas-minor-mode)))

(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)
