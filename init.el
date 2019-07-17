;; Load path
(add-to-list 'load-path (concat user-emacs-directory "setup"))
(add-to-list 'load-path (concat user-emacs-directory "vendor"))

;; config changes made through the customise UI will be stored here
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
  (load custom-file))

;; Splash
(setq inhibit-startup-message t)

;; Turn off mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Setup path
(require 'setup-path)

;; required packages
(defvar required-packages '(better-defaults
                            ace-window
                            auto-complete
                            cider
                            clojure-mode
			    company-lsp
                            ;;cljdoc
			    dart-mode
                            dash
                            diff-hl
                            diminish
                            discover-my-major
                            expand-region
			    flutter
                            flx-ido
                            ido-completing-read+
                            ido-vertical-mode
			    lsp-mode
                            magit
                            multiple-cursors
                            monokai-theme
                            pixie-mode
                            perspective
                            projectile
			    pos-tip
                            rainbow-delimiters
                            restclient
                            smartparens
                            smart-mode-line
                            smex
                            solarized-theme
                            undo-tree
			    use-package
                            volatile-highlights
			    yasnippet))

(require 'setup-required-packages)

;; Appearance
(require 'appearance)

;; core fns
(require 'core-fns)

;; diminish
(require 'diminish)

;; plantuml
(require 'setup-plantuml-helpers)

;; OSX settings
(when (eq system-type 'darwin)
  (require 'osx))

;; Wind Move
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; winner
(winner-mode 1)

;; http://emacswiki.org/emacs/DeleteSelectionMode
(delete-selection-mode 1)

;; restclient-mode
(require 'restclient)

;; diff-hl
;(global-diff-hl-mode 1)

;; emacs backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; autosave
(setq auto-save-default nil)

;; smartparens
(require 'setup-smartparens)

;; ido
(require 'setup-ido)

;; uniquify
(require 'setup-uniquify)

;; whitespace
(require 'setup-whitespace)

;; projectile
(require 'setup-projectile)

;; cider
(require 'setup-cider)

;; volatile highlights
(require 'setup-volatile-highlights)

;; cljdoc
;;(require 'cljdoc)

;; auto-complete
(require 'setup-autocomplete)

;; smart-mode-line
(require 'setup-smart-mode-line)

;; smex
(require 'smex)
(smex-initialize)

;; hooks
(require 'setup-hooks)

;; global key bindings
(require 'setup-global-bindings)

;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; undo-tree
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; org mode
(setq org-blank-before-new-entry nil)

;; program overrides
(setq find-program "gfind")
(setq insert-directory-program "gls")
(setq dired-use-ls-dired t)

;; ace window
(setq aw-scope 'frame)

;; allow y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; GC tuning
(setq gc-cons-threshold 20000000)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; flutter
(defun open-ios-simulator ()
  (interactive)
  (shell-command "flutter emulators --launch apple_ios_simulator") )

(defun flutter-mode-after-save-hook ()
  (when (eq major-mode 'dart-mode)
    (flutter-hot-reload)))

(add-hook 'after-save-hook #'flutter-mode-after-save-hook)

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
  :custom
  (flutter-sdk-path "/usr/local/flutter/"))

(setq dart-format-on-save t)

(require 'company-dart)
(require 'company-yankpad)

(add-hook 'dart-mode-hook (lambda ()
 (set (make-local-variable 'company-backends)
  '(company-dart (company-dabbrev company-yankpad)))))
