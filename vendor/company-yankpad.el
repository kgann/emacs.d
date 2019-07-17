;;; company-yankpad.el --- Completion for yankpad -*- lexical-binding: t; -*-

;; Author: Sidart Kurias
;; URL:
;; Version: 0.1
;; Package-Requires: ((yankpad "1.5") (company-mode))
;; Keywords: language

;; Copyright (C) 2017 Sidart Kurias.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides auto completion and expansion for yankpad snippets that use keyword
;; completion. To use this package define a backend as

;;         (add-hook 'dart-mode-hook (lambda ()
;;	      (set (make-local-variable 'company-backends)
;;                 '(company-dart (company-yankpad company-dabbrev)))))

;; Known bugs:
;;
;;

;;; Code:

(defun company-yankpad--name-or-key (arg fn)
  "Return candidates that match the string entered.
ARG is what the user has entered and expects a match for.
FN is the function that will extract either name or key."
  (delq nil
	(mapcar
	 (lambda (c) (let ((snip (split-string (car c)  yankpad-expand-separator)))
		  (if (string-prefix-p arg (car snip) t)
		      (progn
			(if (string-match yankpad-expand-separator (car c))
			    (set-text-properties 0 1 '(type keyword) (car snip))
			  (set-text-properties 0 1 '(type name) (car snip)))
			(funcall fn snip)))))
	(yankpad-active-snippets))))

(defun company-yankpad--prefix (prefix)
  "Check if PREFIX is part of an active snippet.
If PREFIX is not part of an active snippet inform company mode to use another
backend"
  (car (delq nil (mapcar
		  (lambda (c) (let ((snip (split-string (car c)  yankpad-expand-separator)))
                       (if (string-prefix-p prefix (car snip) t) prefix)))
		  (yankpad-active-snippets)))))

(defun company-yankpad (command &optional arg &rest ignored)
  "Company backend for yankpad."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-yankpad))
    (prefix (company-yankpad--prefix (company-grab-symbol)))
    (annotation (car (company-yankpad--name-or-key
		      arg
		      (lambda (snippet) (mapconcat 'identity (cdr snippet) " ")))))
    (candidates (company-yankpad--name-or-key arg (lambda (snippet) (car snippet))))
    (post-completion (let ((type (get-text-property 0 'type arg)))
     		       (if (equal type 'keyword)
     		       	   (yankpad-expand)
     			 (let ((word (word-at-point))
    			       (bounds (bounds-of-thing-at-point 'word)))
     			   (delete-region (car bounds) (cdr bounds))
     			   (yankpad-insert-from-current-category arg)))))
    (duplicates t)))

(provide 'company-yankpad)

;;; company-yankpad.el ends here
