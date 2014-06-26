;; Load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "setup"))

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
                            clojure-mode
                            clojure-test-mode
                            rainbow-delimiters
                            cider
                            smartparens
                            flx-ido
                            ido-vertical-mode
                            ido-ubiquitous
                            smex
                            ace-window
                            diminish
                            undo-tree
                            projectile
                            perspective))

(require 'setup-required-packages)

;; Appearance
(require 'appearance)

;; core fns
(require 'core-fns)

;; diminish
(require 'diminish)

;; OSX settings
(when (eq system-type 'darwin)
  (require 'osx))

;; Wind Move
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; winner
(winner-mode 1)

;; emacs backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

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

;; hooks
(require 'setup-hooks)

;; global key bindings
(require 'setup-global-bindings)

;; undo-tree
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; program overrides
(setq find-program "gfind")
(setq insert-directory-program "gls")
(setq dired-use-ls-dired t)

;; allow y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; GC tuning
(setq gc-cons-threshold 20000000)
