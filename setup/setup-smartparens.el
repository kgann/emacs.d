(require 'smartparens-config)

(sp-use-paredit-bindings)
(sp-use-smartparens-bindings)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; Solarized
;(set-face-background 'sp-show-pair-match-face "#022B35")

;; Monokai
(set-face-background 'sp-show-pair-match-face "#272822")

(provide 'setup-smartparens)
;; setup-smartparens ends here
