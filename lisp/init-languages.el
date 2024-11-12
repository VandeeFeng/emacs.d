;;; languages.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; debug
;; (setq gud-pdb-command-name "python -m pdb")

;;-------------------------------------------------------------------------
;;golang
(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)
(setq lsp-gopls-codelens nil)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;;-------------------------------------------------------------------------
;;Python
;;(setq python-shell-virtualenv-root "~/miniconda3/envs") ;;设置虚拟环境

;; (add-hook 'python-mode-hook 'anaconda-mode)
(use-package conda
  :ensure t
  :defer t
  :config
  (setq conda-anaconda-home "~/miniconda3/")
  (defun conda-setup ()
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell)
    (conda-env-autoactivate-mode t)
    (setq conda-env-home-directory "~/miniconda3/"))

  (use-package eshell
    :hook (eshell-mode . conda-setup))

  (use-package python
    :hook (python-mode . conda-setup)))

;;(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)) ;; 默认使用 python-ts-mode，需要安装 python 的 treesitter
;; ;;python black
;; (after! python
;;   :preface
;;   (defun +python-make-fstring ()
;;     "Change string to fstring"
;;     (interactive)
;;     (when (nth 3 (syntax-ppss))
;;       (let ((p (point)))
;;         (goto-char (nth 8 (syntax-ppss)))
;;         (insert "f")
;;         (goto-char p)
;;         (forward-char))))
;;   (defun +python-format-buffer ()
;;     "Format python buffer with black"
;;     (interactive)
;;     (python-black-buffer))
;;   :bind
;;   (map! :map python-mode-map
;;         "C-c x s" #'+python-make-fstring
;;         "C-c x f" #'+python-format-buffer))
;;
;;

;;jupter
;;https://github.com/emacs-jupyter/jupyter
(use-package jupyter
  ;; :elpaca t
  :ensure t
  :defer t
  :custom
  (org-babel-jupyter-override-src-block "python")
  :config
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")
                                                       (:kernel . "base"))))

(provide 'init-languages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-languages.el ends here

