;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Increase Process Output Buffer for LSP
;; The default read-process-output-max is 64KB, which is still quite conservative. Modern LSP servers like rust-analyzer or clangd routinely send multi-megabyte responses. Bumping this reduces the number of read calls Emacs has to make:
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

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
