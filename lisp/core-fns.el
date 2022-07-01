(defun custom-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun custom-duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (term (getenv "SHELL")))
    (switch-to-buffer-other-window "*term*")
    (persp-add-buffer "*term*")))

(defun custom-git-grep (&optional term)
  "Uses vc-git-grep to search project root for `term'"
  (interactive (list
                (read-string (format "Search for (default `%s'): " (thing-at-point 'word))
                             nil nil (thing-at-point 'word))))
  (vc-git-grep term "*" (projectile-project-root)))

(defun dev-notes ()
  (interactive)
  (let ((notes (format-time-string "%m-%d-%Y")))
    (find-file (expand-file-name (concat "~/Documents/dev-notes/" notes ".org")))))

(defun lsp-ui-doc-show-at-point (arg)
  (interactive "P")
  (progn
    (setq lsp-ui-doc-delay 0.1)
    (setq lsp-ui-doc-position 'at-point)
    (lsp-ui-doc-show)))

(provide 'core-fns)
;; core fns
