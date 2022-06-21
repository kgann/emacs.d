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
(require 'use-package)
(require 'diminish)

(setq auto-save-default nil)
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq default-cursor-type 'bar)
(setq delete-by-moving-to-trash t)
(setq dired-use-ls-dired t)
(setq find-program "gfind")
(setq gc-cons-threshold 50000000)
(setq help-window-select t)
(setq inhibit-startup-message t)
(setq insert-directory-program "gls")
;; (setq linum-format " %d ")
(setq load-prefer-newer t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq org-blank-before-new-entry nil)
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq trash-directory "~/.Trash/emacs")
(setq use-package-verbose t)
(setq visible-bell t)
(setq-default column-number-mode t)
(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
  (load custom-file))

;; Functionality
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(global-linum-mode 1)
(show-paren-mode 1)
(windmove-default-keybindings)
(winner-mode 1)

;; Appearance
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 200))
(set-cursor-color "#ffffff")
(set-face-attribute 'default nil :family "Monaco" :height 140)
(set-face-attribute 'region nil :background "#67930F" :foreground "#C1F161")

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package neotree
  :config
  (global-set-key (kbd "C-x t t") 'neotree-toggle))

(use-package core-fns
  :load-path "./lisp"
  :config
  ;; Bindings
  (global-set-key (kbd "C-c d") 'custom-duplicate-line)
  (global-set-key (kbd "C-c n") 'dev-notes)
  (global-set-key (kbd "C-a") 'custom-move-beginning-of-line)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  (global-set-key (kbd "C-c R") 'revert-all-buffers))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package iimage :ensure t)
(use-package restclient :ensure t)
(use-package smartparens
  :ensure t
  :hook ((clojure-mode . smartparens-mode)
	 (emacs-lisp-mode . smartparens-mode)
	 (lisp-mode . smartparens-mode)
	 (eval-expression-minibuffer-setup . smartparens-mode))
  :config
  (require 'smartparens-config)
  ;;(sp-use-paredit-bindings)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  ;;(set-face-background 'sp-show-pair-match-face "#272822")
  )

(use-package flycheck :ensure t)
;; (use-package flycheck-joker :ensure t)
(use-package flycheck-clj-kondo :ensure t)

(use-package ido-completing-read+ :ensure t)
(use-package flx-ido :ensure t)
(use-package ido-vertical-mode :ensure t)
(use-package ido
  :ensure t
  :init
  (setq ido-enable-prefix nil)
  (setq ido-enable-flex-matching t)
  (setq ido-max-prospects 10)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-faces nil)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (setq org-completion-use-ido t)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  :config
  (ido-mode 1)
  (ido-ubiquitous-mode 1)
  (ido-vertical-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode +1)
  (add-to-list 'ido-ignore-files "\\.DS_Store"))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package whitespace
  :diminish whitespace-mode
  :init
  (setq whitespace-line-column 100) ;; limit line length
  (setq whitespace-style '(face tabs trailing lines-tail))
  :config
  (set-face-attribute 'whitespace-line nil :underline "#C73A82"))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)
  (setq projectile-mode-line-lighter "P")
  (setq persp-initial-frame-name "m")
  (setq ns-right-command-modifier 'super)
  :config
  (projectile-global-mode)
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD")
  (global-set-key (kbd "s-f") 'projectile-find-file))

(use-package sudo-edit
  :ensure t)

(use-package direnv
  :ensure t
  ;;:config
  ;;(add-to-list 'direnv-non-file-modes 'cider-repl-mode)
  )

(use-package browse-at-remote
  :ensure t)

(use-package cider
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
;; (advice-add 'cider-jack-in-clj
  ;; 	      :before (lambda (&rest args)
  ;; 			(direnv-update-directory-environment)))
  ;; (setq cider-clojure-cli-global-options "-A:dev-defaults:dev -C:fake-id -J-Xmx4000m")
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-wrap-history t)
  (setq cider-prompt-save-file-on-load nil)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-overlays-use-font-lock t))

;; (use-package volatile-highlights
;;   :ensure t
;;   :diminish volatile-highlights-mode
;;   :config
;;   (volatile-highlights-mode t))

(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  :init
  (setq ac-ignore-case nil))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package amx
  :ensure t
  :config
  (amx-initialize)
  :init
  (global-set-key (kbd "M-x") 'amx))

;; (use-package makefile-executor
;;   :ensure t
;;   :init
;;   (advice-add 'makefile-executor-execute-target
;; 	      :before (lambda (&rest args)
;; 			(direnv-update-directory-environment)))
;;   :config
;;   (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(use-package rainbow-delimiters
  :ensure t)

(use-package clojure-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.joke\\'" . clojure-mode))
  :config
  (require 'flycheck-clj-kondo)
  (require 'smartparens-clojure)
  :hook ((clojure-mode . rainbow-delimiters-mode)
	 (clojure-mode . turn-on-eldoc-mode)
	 (clojure-mode . flycheck-mode)))

(use-package yaml-mode
  :ensure t)

(use-package magit
  :ensure t
  :config
  (define-prefix-command 'magit-map)
  (global-set-key (kbd "C-c g") 'magit-map)
  (define-key magit-map (kbd "s") 'magit-status)
  (define-key magit-map (kbd "g") 'custom-git-grep)
  (define-key magit-map (kbd "b") 'magit-blame)
  (setq magit-last-seen-setup-instructions "1.4.0"))

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

(use-package ace-window
  :ensure t
  :init
  (setq aw-scope 'frame)
  :config
  (global-set-key [remap other-window] 'ace-window))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-auto-guess-root t))

(use-package dart-mode
  :ensure t
  :hook (dart-mode . lsp)
  :config
  (add-hook 'before-save-hook
	    (lambda ()
	      (when (eq major-mode 'dart-mode)
		(dart-format))))
  :custom
  (dart-sdk-path "/usr/local/flutter/bin/cache/dart-sdk/"))

(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :commands open-ios-simulator
  :hook (after-save . flutter-hot-reload)
  :init
  (with-eval-after-load 'flutter
    (defun flutter-project-get-root ()
      (projectile-project-root)))
  :custom
  (flutter-sdk-path "/usr/local/flutter/"))

(use-package yasnippet
  :ensure t
  :diminish yasnippet
  :config
  (yas-global-mode 1))

(use-package clojure-snippets
  :ensure t)

(use-package pos-tip
  :ensure t
  :commands pos-tip)

(use-package company
  :ensure t)

;; (use-package company-lsp
;;   :ensure t
;;   :init
;;   (setq company-lsp-cache-candidates t)
;;   (setq company-lsp-filter-candidates t)
;;   :commands company-lsp
;;   :bind ("C-<return>" . company-lsp))

;; (use-package company-yankpad
;;   :load-path "./vendor")

;; (use-package company-dart
;;   :load-path "./vendor"
;;   :after (dart-mode company-yankpad)
;;   :config
;;   (add-hook 'dart-mode-hook
;; 	    (lambda ()
;; 	      (set (make-local-variable 'company-backends)
;; 		   '(company-dart (company-dabbrev company-yankpad))))))

(put 'erase-buffer 'disabled nil)
