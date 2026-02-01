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

;; 很奇怪的是，这个设置不能正确的获取我的 rust 版本 ,fmt 的格式化也和在
;; 终端里直接使用 cargo fmt 不同，但是 nvim 里使用
;; rust-analyzer 是正常的
;; 官方文件是这样说的：Eglot does not support the rust-analyzer extensions to the language-server protocol and does not aim to do so in the future. The eglot-x package adds experimental support for those LSP extensions.

;; 之前运行 rust fmt 和在 eglot 里自动 rustfmt 有差异的原因是没有指定 rustfmt 的 style edition
;; 已经在 ~/.config/rustfmt/rustfmt.toml 里设置好了
;; Auto-format rust buffers using rustfmt before saving
(defun rust-eglot-format-on-save ()
  "Add eglot-format-buffer to before-save-hook, but only for this buffer."
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))
(add-hook 'rust-mode-hook #'rust-eglot-format-on-save)

(when (maybe-require-package 'rust-mode)
  (when (maybe-require-package 'flycheck-rust)
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

;; ;; Configure eglot to coexist with flymake backends
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-stay-out-of 'flymake))
;; (defun manually-activate-eglot-flymake ()
;;   "Manually activate eglot's flymake backend alongside other backends."
;;   (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
;;   (flymake-mode 1))
;; (add-hook 'eglot-managed-mode-hook #'manually-activate-eglot-flymake nil t)

(provide 'init-rust)
;;; init-rust.el ends here
