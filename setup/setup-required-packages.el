(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(defvar packages '(better-defaults
                   clojure-mode
                   clojure-test-mode
                   rainbow-delimiters
                   cider
                   smartparens
                   flx-ido
                   ido-vertical-mode
                   ido-ubiquitous
                   smex
                   diminish
                   undo-tree
                   projectile
                   perspective))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'setup-required-packages)
;; setup-required-packages ends here
