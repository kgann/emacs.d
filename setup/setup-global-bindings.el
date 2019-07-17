(global-set-key (kbd "C-c d") 'custom-duplicate-line)

(global-set-key (kbd "C-c n") 'dev-notes)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-a") 'custom-move-beginning-of-line)

(global-set-key [remap other-window] 'ace-window)

(global-set-key (kbd "C-h C-m") 'discover-my-major)

(global-set-key (kbd "s-S") 'sr-speedbar-toggle)
(global-set-key (kbd "s-s") 'nv-speedbar-open-current-buffer-in-tree)

(global-set-key (kbd "C-@") 'er/expand-region)

(global-set-key (kbd "C-c r") 'revert-buffer)

(global-set-key (kbd "C-c R") 'revert-all-buffers)

;; Git/Magit
(define-prefix-command 'magit-map)
(global-set-key (kbd "C-c g") 'magit-map)
(define-key magit-map (kbd "s") 'magit-status)
(define-key magit-map (kbd "g") 'custom-git-grep)
(define-key magit-map (kbd "b") 'magit-blame)

;; Resize
(global-set-key (kbd "C-S-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-s-<down>") 'shrink-window)
(global-set-key (kbd "C-S-s-<up>") 'enlarge-window)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; term
(global-set-key (kbd "C-c t") 'visit-term-buffer)

(provide 'setup-global-bindings)
;; setup-global-bindings ends here
