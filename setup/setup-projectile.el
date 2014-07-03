(projectile-global-mode)

(setq projectile-enable-caching t)
(setq projectile-mode-line-lighter "P")

(global-set-key (kbd "s-f") 'projectile-find-file)

;(setq persp-initial-frame-name (projectile-project-name))
(setq persp-initial-frame-name "m")
(persp-mode)
(require 'persp-projectile)

(diminish 'projectile-mode)

(provide 'setup-projectile)
;; setup-projectile ends here
