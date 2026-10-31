;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://rust-analyzer.github.io/book/other_editors.html#eglot
(require 'eglot)
;; emacsclient 在使用 eglot 调用 projetile 的时候，index 会卡住
;; 先临时使用 project.el 的 backend
(defun my/rust-eglot-ensure ()
  "Start Eglot with the built-in VC project backend preferred."
  (setq-local project-find-functions
              (cons #'project-try-vc
                    (remove #'project-try-vc project-find-functions)))
  (eglot-ensure))

(add-hook 'rust-mode-hook #'my/rust-eglot-ensure)
(add-hook 'rust-ts-mode-hook #'my/rust-eglot-ensure)

;; enable clippy
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '((rust-ts-mode rust-mode) .
;;                  ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

;; 之前运行 rust fmt 和在 eglot 里自动 rustfmt 有差异的原因是没有指定 rustfmt 的 style edition
;; 已经在 ~/.config/rustfmt/rustfmt.toml 里设置好了
;; Auto-format rust buffers using rustfmt before saving
(when (maybe-require-package 'reformatter)
  (if (executable-find "rustfmt")
      (reformatter-define rustfmt :program "rustfmt" :args '("--emit" "stdout"))
    (message "WARNING: rustfmt not found. Please install rustfmt to enable Rust formatting.")))
(when (executable-find "rustfmt")
  (add-hook 'rust-mode-hook 'rustfmt-on-save-mode))

(when (maybe-require-package 'rust-mode)
  (when (maybe-require-package 'flycheck-rust)
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(provide 'init-rust)
;;; init-rust.el ends here
