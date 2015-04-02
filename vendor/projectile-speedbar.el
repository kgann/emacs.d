(require 'speedbar)
(require 'sr-speedbar)

(defvar helm-alive-p nil)

(defun nv-find-project-root ()
  (setq nv-current-dir (file-truename buffer-file-name))
  (while (not (file-exists-p (concat nv-current-dir ".git")))
    (setq nv-current-dir (file-name-directory (substring nv-current-dir 0 -1))))
  (concat nv-current-dir ""))

(defun nv-speedbar-project-refresh (root-dir)
  "Refresh the context of speedbar based on project root"
  (when (and (not (equal root-dir sr-speedbar-last-refresh-dictionary))
             (not (sr-speedbar-window-p)))
    (setq sr-speedbar-last-refresh-dictionary root-dir))
  (setq default-directory root-dir)
  (speedbar-refresh))

(defun nv-open-current-project-in-speedbar (root-dir)
  "Refresh speedbar to show current project in tree"
  (if (not (sr-speedbar-exist-p))
      (sr-speedbar-toggle))
  (nv-speedbar-project-refresh root-dir))

(defun nv-speedbar-expand-line-list (&optional arg)
  (when arg
    (re-search-forward (concat " " (car arg) "$"))
    (speedbar-expand-line (car arg))
    (speedbar-next 1)
    (nv-speedbar-expand-line-list (cdr arg))))

(defun nv-speedbar-open-current-buffer-in-tree ()
  (interactive)
  (let* ((root-dir (nv-find-project-root))
         (original-buffer-file-directory (file-name-directory (buffer-file-name)))
         (relative-buffer-path (car (cdr (split-string original-buffer-file-directory root-dir))))
         (parents (butlast (split-string relative-buffer-path "/")))
         (original-window (get-buffer-window)))
    (save-excursion
      (nv-open-current-project-in-speedbar root-dir)
      (select-window (get-buffer-window speedbar-buffer))
      (beginning-of-buffer)
      (nv-speedbar-expand-line-list parents)
      (if (not (eq original-window (get-buffer-window speedbar-buffer)))
          (select-window original-window)
        (other-window 1)))))

(provide 'projectile-speedbar)
