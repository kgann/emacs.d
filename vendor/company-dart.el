;;; company-dart.el --- Autocompletion for Dart files -*- lexical-binding: t; -*-

;; Author: Sidart Kurias
;; Version: 0.1
;; Package-Requires: ((dart-mode "0.20") (company) (pos-tip "0.4.6"))
;; Keywords: language

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

;; Add something similar to your init file
;; (add-hook 'dart-mode-hook (lambda ()
;;    (set (make-local-variable 'company-backends)
;;      '(company-dart (company-dabbrev company-yasnippet)))))
;;
;; Dart completion will be invoked only after the "." character has been typed.
;; You can manually invoke completion by binding (company-dart)
;; to any key you like. Hitting F1 while in the completion list will show the
;; documentation for that candidate.
;;
;; A good source for snippets
;; https://github.com/JEG2/dotfiles/tree/master/emacs.d/jeg2/snippets/dart-mode/
;;
;; https://github.com/expez/company-quickhelp. Shows complete documentation as
;; a popup.
;;

;;; Code:
(require 'dart-mode)
(require 'company)
(require 'pos-tip)

(defun dart--company-prepare-candidates (response)
  "Build completion from the parsed data received from the analysis server.

Argument RESPONSE contains the candidates, documentation, parameters to be displayed."
  (-when-let* ((completions (cdr (assq 'results (assq 'params response)))))
    (mapcar
     (lambda (completion)
       (let ((docSummary (assoc 'docSummary completion))
	     (parameters  (assoc 'parameters (assoc 'element completion)))
	     (docComplete  (assoc 'docComplete completion))
	     (candidate (cdr (assq 'completion completion))))
	 (propertize  candidate
		      (car parameters) (cdr parameters)
		      (car docSummary) (cdr docSummary)
		      (car docComplete) (cdr docComplete))))
     completions)))


(defun dart--get-completions (callback buffer)
  "Ask the analysis server for suggestions.

Argument CALLBACK is the function passed by  ‘company-mode’.
Argument BUFFER the buffer containing the dart file."

  (dart--analysis-server-send
   "completion.getSuggestions"
   `((file . ,(buffer-file-name))
     (offset . ,(point)))
   (lambda (response)
     ;;set the dart-completion-callback on dart-mode, so that it will in turn
     ;;execute company mode callback.
     (setq dart-completion-callback
	   (lambda (resp)
	     (-when-let* ((candidates (dart--company-prepare-candidates
				       resp)))
	       (with-current-buffer buffer
		 (funcall callback  candidates))))))))

(defun dart--completion-annotation (s)
  "Show method parameters as annotations"
  (get-text-property 0 'parameters s))

(defun dart--completion-meta (s)
  "Show summary documentation."
  (get-text-property 0 'docSummary s))

(defun dart--completion-doc (s)
  "Show complete documentation in the help buffer."
  (--when-let (get-text-property 0 'docComplete s)
    (company-doc-buffer it)))

(defun dart--company-prefix ()
  (let ((sym (company-grab-symbol-cons "\\." 1)))
    (if (consp sym) sym nil)))

;;;###autoload
(defun company-dart (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dart))
    (prefix (and (derived-mode-p 'dart-mode)
		 (dart--company-prefix)))
    (candidates
     (cons :async
	   (lambda (callback)
	     (dart--get-completions callback (current-buffer)))))
    (duplicates t)
    (annotation (dart--completion-annotation arg))
    (doc-buffer (dart--completion-doc arg))
    (meta (dart--completion-meta arg))
    (post-completion (let ((anno (dart--completion-annotation arg))
			   (meta (dart--completion-meta arg)))
		       (when (> (length anno) 0)
			 ;;not a getter
		       (insert "()"))
    		       (when (> (length anno) 2)
			 ;; > 2 implies non empty argument list
			   (backward-char))
		       (pos-tip-show (format "%s\n%s" anno meta) nil nil
				     nil -1)))))

(provide 'company-dart)
