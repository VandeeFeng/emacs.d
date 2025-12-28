;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; (when (maybe-require-package 'diredfl)
;;   (with-eval-after-load 'dired
;;     (diredfl-global-mode)
;;     (require 'dired-x)))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(when (maybe-require-package 'diff-hl)
  (with-eval-after-load 'dired
    (require 'dired-x)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "S-<left>") #'dired-up-directory)
  (define-key dired-mode-map (kbd "S-<right>") #'dired-find-file)

  (defun my/dired-enter-edit-mode ()
    "Toggle dired read-only mode and switch to Evil insert state."
    (interactive)
    (dired-toggle-read-only)
    (when (fboundp 'evil-insert-state)
      (evil-insert-state)))

  (define-key dired-mode-map (kbd "i") #'my/dired-enter-edit-mode)
  (define-key dired-mode-map (kbd "a") #'my/dired-enter-edit-mode)
  )

(provide 'init-dired)
;;; init-dired.el ends here
