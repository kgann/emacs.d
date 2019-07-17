;; custom
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
  (load custom-file))

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
(require 'use-package)
(setq use-package-verbose t)

;; Appearance
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 200))
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(global-linum-mode 1)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(set-cursor-color "#ffffff")
(set-face-attribute 'default nil :family "Monaco" :height 140)
(set-face-attribute 'region nil :background "#67930F" :foreground "#C1F161")
(setq auto-save-default nil)
(setq aw-scope 'frame)
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq default-cursor-type 'bar)
(setq delete-by-moving-to-trash t)
(setq dired-use-ls-dired t)
(setq find-program "gfind")
(setq gc-cons-threshold 20000000)
(setq help-window-select t)
(setq inhibit-startup-message t)
(setq insert-directory-program "gls")
(setq linum-format " %d ")
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq org-blank-before-new-entry nil)
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq trash-directory "~/.Trash/emacs")
(setq visible-bell t)
(setq-default column-number-mode t)
(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)
(show-paren-mode 1)
(windmove-default-keybindings)
(winner-mode 1)

;; fns
(defun custom-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun custom-duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

(defun custom-git-grep (&optional term)
  "Uses vc-git-grep to search project root for `term'"
  (interactive (list
                (read-string (format "Search for (default `%s'): " (thing-at-point 'word))
                             nil nil (thing-at-point 'word))))
  (vc-git-grep term "*" (projectile-project-root)))

(defun dev-notes ()
  (interactive)
  (find-file (expand-file-name
	      (concat "~/Documents/dev-notes/"
		      (format-time-string "%m-%d-%Y")
		      ".org"))))

(defun prev-dev-notes ()
  (interactive)
  (let* ((day (* 24 3600))
	 (now (current-time)))
    (find-file (expand-file-name
		(concat "~/Documents/dev-notes/"
			(format-time-string "%m-%d-%Y" (time-subtract now day))
			".org")))))

(defun plantuml-render-buffer ()
  (interactive)
  (let ((tmp (getenv "TMPDIR"))
   	(out (concat tmp (file-name-sans-extension buffer-file-name) ".png")))
    (message "PLANTUML Start rendering")
    (shell-command (concat "java -jar ~/plantuml.jar " buffer-file-name))
    (message (concat "PLANTUML Rendered: " (buffer-name)))
    (with-temp-buffer
      (insert-file-contents out))))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package diminish :ensure t)
(use-package iimage :ensure t)
(use-package restclient :ensure t)
(use-package paredit
  :ensure t
  :hook ((clojure-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)
	 (lisp-mode . paredit-mode)
	 (eval-expression-minibuffer-setup . paredit-mode)))

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
  :init
  (setq whitespace-line-column 100) ;; limit line length
  (setq whitespace-style '(face tabs trailing lines-tail))
  :config
  (set-face-attribute 'whitespace-line nil :underline "#C73A82")
  (diminish 'whitespace-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-mode-line-lighter "P")
  (setq persp-initial-frame-name "m")
  :config
  (projectile-global-mode)
  (global-set-key (kbd "s-f") 'projectile-find-file)
  (diminish 'projectile-mode))

(use-package cider
  :ensure t
  :init
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-repl-wrap-history t)
  (setq cider-prompt-save-file-on-load nil)
  (setq cider-repl-use-clojure-font-lock t))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t)
  (diminish 'volatile-highlights-mode))

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

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :init
  (global-set-key (kbd "M-x") 'smex))

(use-package rainbow-delimiters
  :ensure t)

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . rainbow-delimiters-mode)
	 (clojure-mode . turn-on-eldoc-mode)))

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
  :config
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode))

(use-package ace-window
  :ensure t
  :config
  (global-set-key [remap other-window] 'ace-window))

(global-set-key (kbd "C-c d") 'custom-duplicate-line)
(global-set-key (kbd "C-c n") 'dev-notes)
(global-set-key (kbd "C-a") 'custom-move-beginning-of-line)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c R") 'revert-all-buffers)

;; flutter
(defun open-ios-simulator ()
  (interactive)
  (shell-command "flutter emulators --launch apple_ios_simulator"))

(use-package pos-tip
  :commands pos-tip)

(use-package company-lsp
  :commands company-lsp)

(use-package lsp-mode
  :commands lsp)

(use-package dart-mode
  :hook (dart-mode . lsp)
  :after lsp
  :custom
  (dart-format-on-save t)
  (dart-sdk-path "/usr/local/flutter/bin/cache/dart-sdk/"))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :commands open-ios-simulator
  :hook (after-save . flutter-hot-reload)
  :custom
  (flutter-sdk-path "/usr/local/flutter/"))

;; (require 'company-dart)
;; (require 'company-yankpad)

;; (add-hook 'dart-mode-hook (lambda ()
;;  (set (make-local-variable 'company-backends)
;;   '(company-dart (company-dabbrev company-yankpad)))))
