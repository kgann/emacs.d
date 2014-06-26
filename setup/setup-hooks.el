;; trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Clojure mode
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; Cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; linum
(add-hook 'prog-mode-hook 'linum-mode)

;; eldoc
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)

;; whitespace
(add-hook 'prog-mode-hook 'whitespace-mode)

(provide 'setup-hooks)
;; setup-hooks ends here
