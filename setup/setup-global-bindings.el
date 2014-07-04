(global-set-key (kbd "C-c d") 'custom-duplicate-line)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-a") 'custom-move-beginning-of-line)

(global-set-key [remap other-window] 'ace-window)

(global-set-key (kbd "C-c b") 'magit-blame-mode)

(global-set-key (kbd "C-h C-m") 'discover-my-major)

(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

(global-set-key (kbd "C-@") 'er/expand-region)

(global-set-key (kbd "C-c r") 'revert-buffer)

;; Resize
(global-set-key (kbd "C-S-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-s-<down>") 'shrink-window)
(global-set-key (kbd "C-S-s-<up>") 'enlarge-window)

(provide 'setup-global-bindings)
;; setup-global-bindings ends here
