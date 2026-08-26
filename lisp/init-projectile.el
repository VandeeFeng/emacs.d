;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  (setq-default projectile-async-indexing t
                projectile-enable-caching t)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))

(defun my/projectile-switch-project-dired ()
  "Switch project and open with dired instead of finding a file."
  (interactive)
  (let ((projectile-switch-project-action #'projectile-dired))
    (projectile-switch-project)))

(defun my/projectile-switch-open-project-dired ()
  "Switch to open project and open with dired."
  (interactive)
  (let ((projectile-switch-project-action #'projectile-dired))
    (projectile-switch-open-project)))

(provide 'init-projectile)
;;; init-projectile.el ends here
