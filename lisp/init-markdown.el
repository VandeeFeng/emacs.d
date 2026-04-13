;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'markdown-mode)
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode))
  ;;(add-hook 'markdown-mode-hook 'variable-pitch-mode) 这样会让代码块之外的正文使用等比例字体，看起来更书面
  )


(provide 'init-markdown)
;;; init-markdown.el ends here
