(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'setup-required-packages)
;; setup-required-packages ends here
