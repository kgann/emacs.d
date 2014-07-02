(projectile-global-mode)

(setq projectile-enable-caching t)
(setq projectile-mode-line-lighter "P")

(global-set-key (kbd "s-f") 'projectile-find-file)

(persp-mode)
(require 'persp-projectile)

(diminish 'projectile-mode)

; mode-line-highlight

(provide 'setup-projectile)
;; setup-projectile ends here
