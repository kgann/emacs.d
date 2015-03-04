;; trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Clojure mode
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook (lambda ()
                               (define-clojure-indent
                                 ;; Compojure
                                 (defroutes 'defun)
                                 (GET 2)
                                 (POST 2)
                                 (PUT 2)
                                 (DELETE 2)
                                 (HEAD 2)
                                 (ANY 2)
                                 (context 2)

                                 ;; Desk API
                                 (authorize 'defun)
                                 (etaggable 'defun)

                                 ;; Midje
                                 (fact 'defun)
                                 (facts 'defun)
                                 (against-background 'defun)
                                 (provided 'defun))))

;; Cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)

;; linum
(add-hook 'prog-mode-hook 'linum-mode)

;; whitespace
(add-hook 'prog-mode-hook 'whitespace-mode)

;; diff-hl
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(provide 'setup-hooks)
;; setup-hooks ends here
