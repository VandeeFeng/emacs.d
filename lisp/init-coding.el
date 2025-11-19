;;; coding.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; harper
;; eglot 一次只能在一个 mode 里链接一个 lsp。。。
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(rust-mode . ("harper-ls" "--stdio")))
;;   (add-to-list 'eglot-server-programs
;;                '(python-mode . ("harper-ls" "--stdio")))
;;   (add-to-list 'eglot-server-programs
;;                '(org-mode . ("harper-ls" "--stdio"))))

;; (setq-default eglot-workspace-configuration
;;               '(:harper-ls (:linters (:SpellCheck t
;;                                                   :SentenceCapitalization :json-false))))


(provide 'init-coding)
;;; init-coding.el ends here

