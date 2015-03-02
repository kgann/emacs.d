;; Theme
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "themes"))

;; Solarized settings
;; (setq solarized-distinct-fringe-background t)
;; ;(setq solarized-high-contrast-mode-line t)
;; (setq solarized-emphasize-indicators t)
;; (setq solarized-use-less-bold t)
;; (setq x-underline-at-descent-line t)
;; (load-theme 'solarized-dark t)

;(load-theme 'blackboard t)
;(load-theme 'noctilux t)
;(load-theme 'zenburn t)
(load-theme 'monokai t)

;; Bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Cursor
(setq default-cursor-type 'bar)
(set-cursor-color "#ffffff")

;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Font
(set-face-attribute 'default nil :family "Monaco" :height 140)

;; Window
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 200))

;; Highlight current line
(global-hl-line-mode 1)

;; Lines / Columns
(setq-default column-number-mode t)
(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)

;; Rename mode line
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "clojure-mode" clojure-mode "clj")

(provide 'appearance)
;; appearance ends here
