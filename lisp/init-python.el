;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; debug
;; (setq gud-pdb-command-name "python -m pdb")

;; ruff
;; via: https://stackoverflow.com/questions/79555604/run-ruff-in-emacs
(add-hook 'python-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ruff" "server"))))

;; Format python buffers using eglot before saving.major-mode hook, which then adds a buffer-local hook.
(defun python-eglot-format-on-save ()
  "Add eglot-format-buffer to before-save-hook, but only for this buffer."
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))
(add-hook 'python-mode-hook #'python-eglot-format-on-save)

;; (maybe-require-package 'ruff-format)
;; (add-hook 'python-mode-hook 'ruff-format-on-save-mode)

(defun ruff-check ()
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if current-file
        (async-shell-command
         (format "ruff check --select ALL %s" (shell-quote-argument current-file))
         )
      )
    )
  )

(defun ruff-fix ()
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if current-file
        (progn
          (shell-command
           (format "ruff check --select ALL --fix %s" (shell-quote-argument current-file))
           )
          (revert-buffer t t t)
          )
      )
    )
  )

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

(maybe-require-package 'ruff-format)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("\\(poetry\\|uv\\)\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))

(provide 'init-python)
;;; init-python.el ends here
