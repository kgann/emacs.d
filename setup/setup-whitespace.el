(require 'whitespace)

(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs trailing lines-tail))

(set-face-attribute 'whitespace-line nil
                     :underline "OrangeRed4"
                     :background "#0C1021"
                     :foreground "#F8F8F8")

(diminish 'whitespace-mode)

(provide 'setup-whitespace)
;; setup-whitespace ends here
