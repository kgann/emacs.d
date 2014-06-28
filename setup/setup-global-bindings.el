(global-set-key (kbd "C-c d") 'custom-duplicate-line)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-a") 'custom-move-beginning-of-line)

(global-set-key [remap other-window] 'ace-window)

(global-set-key (kbd "C-c b") 'magit-blame-mode)

(global-set-key (kbd "C-h C-m") 'discover-my-major)

(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

(global-set-key (kbd "C-@") 'er/expand-region)

(provide 'setup-global-bindings)
;; setup-global-bindings ends here
