;; Load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "setup"))
(add-to-list 'load-path (concat user-emacs-directory "vendor"))

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
                            ack-and-a-half
                            auto-complete
                            cider
                            clojure-mode
                            clojure-test-mode
                            cljdoc
                            diff-hl
                            diminish
                            discover-my-major
                            expand-region
                            flx-ido
                            ido-ubiquitous
                            ido-vertical-mode
                            magit
                            perspective
                            projectile
                            rainbow-delimiters
                            smartparens
                            smex
                            solarized-theme
                            sr-speedbar
                            undo-tree
                            volatile-highlights))

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

;; diff-hl
;(global-diff-hl-mode 1)

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

;; cider
(require 'setup-cider)

;; volatile highlights
(require 'setup-volatile-highlights)

;; sr-speedbar
(require 'setup-sr-speedbar)

;; cljdoc
(require 'cljdoc)

;; auto-complete
(require 'setup-autocomplete)

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
