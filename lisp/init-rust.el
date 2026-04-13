;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://rust-analyzer.github.io/book/other_editors.html#eglot
(require 'eglot)
(add-hook 'rust-mode-hook 'eglot-ensure)
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
