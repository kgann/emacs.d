(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'setup-required-packages)
;; setup-required-packages ends here
