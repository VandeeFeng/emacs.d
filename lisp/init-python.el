;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; debug
;; (setq gud-pdb-command-name "python -m pdb")

;; ruff
(maybe-require-package 'ruff-format)
(add-hook 'python-mode-hook 'eglot-ensure)
(if (executable-find "ruff")
    (add-hook 'python-mode-hook 'ruff-format-on-save-mode)
  (message "WARNING: ruff not found. Please install ruff to enable Python formatting."))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'flymake)
  (if (executable-find "ty")
      (add-to-list 'eglot-server-programs
                   '((python-mode python-ts-mode) . ("ty" "server")))
    (message "WARNING: ty not found. Please install ty for Python LSP support."))
  ;;              '(python-mode . ("ruff" "server")))
  )

;; via: https://stackoverflow.com/questions/79555604/run-ruff-in-emacs
;; 这个要求 LSP server 必须在初始化时声明 documentFormattingProvider
;; 通过 LSP 协议发送 textDocument/formatting 请求
;; Format python buffers using eglot before saving.major-mode hook, which then adds a buffer-local hook.
;; (defun python-eglot-format-on-save ()
;;   "Add eglot-format-buffer to before-save-hook, but only for this buffer."
;;   (add-hook 'before-save-hook #'eglot-format-buffer nil t))
;; (add-hook 'python-mode-hook #'python-eglot-format-on-save)

(defun ruff-check ()
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if current-file
        (async-shell-command
         (format "ruff check --select ALL %s" (shell-quote-argument current-file))))))

;; (defun ruff-fix ()
;;   (interactive)
;;   (let ((current-file (buffer-file-name)))
;;     (if current-file
;;         (progn
;;           (shell-command
;;            (format "ruff check --select ALL --fix %s" (shell-quote-argument current-file)))
;;           (revert-buffer t t t)))))

(reformatter-define ruff-check
  :program ruff-format-command
  :args (list "check" "--output-format" "text"
              "--stdin-filename" (or (buffer-file-name) input-file))
  :lighter " RuffCheck")

(reformatter-define ruff-fix
  :program ruff-format-command
  :args (list "check" "--fix-only" "--stdin-filename" (or (buffer-file-name) input-file))
  :lighter " RuffFix")


;; I use nix + direnv instead of virtualenv/pyenv/pyvenv, and it is an
;; approach which extends to other languages too. I recorded a
;; screencast about this: https://www.youtube.com/watch?v=TbIHRHy7_JM


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

(require-package 'pip-requirements)

(when (maybe-require-package 'flymake-ruff)
  (defun sanityinc/flymake-ruff-maybe-enable ()
    (when (executable-find "ruff")
      (flymake-ruff-load)))
  (add-hook 'python-mode-hook 'sanityinc/flymake-ruff-maybe-enable))

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("\\(poetry\\|uv\\)\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml"))
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files "pyproject.toml"))

(provide 'init-python)
;;; init-python.el ends here
