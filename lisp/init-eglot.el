;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot))

(maybe-require-package 'eldoc-box)
(with-eval-after-load 'eglot
  ;; automatically shutdown eglot when buffers are killed
  (setq eglot-autoshutdown t)

  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-help-at-point))
;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)

(provide 'init-eglot)
;;; init-eglot.el ends here
