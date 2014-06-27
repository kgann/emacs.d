;; trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Clojure mode
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

;; Cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; linum
(add-hook 'prog-mode-hook 'linum-mode)

;; whitespace
(add-hook 'prog-mode-hook 'whitespace-mode)

;; diff-hl
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(provide 'setup-hooks)
;; setup-hooks ends here
