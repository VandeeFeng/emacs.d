;;; init-vc.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;; Most version control packages are configured separately: see
;; init-git.el, for example.

;;; Code:

;; diff-hl 配色美化
(custom-set-faces
 '(diff-hl-insert ((t (:background nil :foreground "green1"))))
 '(diff-hl-delete ((t (:background nil :foreground "#ff3030"))))
 '(diff-hl-change ((t (:background nil :foreground "#ffc125")))))

(when (maybe-require-package 'diff-hl)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-init-hook 'global-diff-hl-mode)

  (with-eval-after-load 'diff-hl
    (define-key diff-hl-mode-map (kbd "<left-fringe> <mouse-1>") 'diff-hl-diff-goto-hunk)
    (define-key diff-hl-mode-map (kbd "M-C-]") 'diff-hl-next-hunk)
    (define-key diff-hl-mode-map (kbd "M-C-[") 'diff-hl-previous-hunk)))

(provide 'init-vc)
;;; init-vc.el ends here
