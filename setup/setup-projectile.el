(projectile-global-mode)

(setq projectile-enable-caching t)
(setq projectile-mode-line-lighter " P")

(global-set-key (kbd "s-f") 'projectile-find-file)

(provide 'setup-projectile)
;; setup-projectile ends here
