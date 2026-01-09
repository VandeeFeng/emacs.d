;;; init-terminals.el --- Terminal emulators          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; vterm
(require 'vterm)

(defun vterm-minibuffer ()
  "Open vterm in minibuffer and enter insert state."
  (interactive)
  (let ((height (/ (frame-height) 3)))  ; 设置高度为框架高度的1/3
    (with-temp-buffer
      (let ((window (split-window-vertically (- height))))
        (select-window window)
        (vterm)
        ;; 确保 evil-mode 已加载后进入 insert 状态
        (when (bound-and-true-p evil-mode)
          (evil-insert-state))))))

;; 定义一个更简单的命令别名
(defalias 'vt 'vterm-minibuffer)

;; 自定义 vterm 在 minibuffer 中的行为
(with-eval-after-load 'vterm
  (evil-define-key '(normal insert) vterm-mode-map (kbd "C-y") #'vterm-yank)
  (evil-define-key '(normal) vterm-mode-map (kbd "p") #'vterm-yank)
  (evil-define-key '(normal) vterm-mode-map (kbd "u") #'vterm-undo)
  (evil-define-key 'normal vterm-mode-map (kbd "q") #'delete-window)

  ;; 为 vterm-mode 添加 hook，确保在打开时进入 insert 状态
  (add-hook 'vterm-mode-hook
            (lambda ()
              (when (bound-and-true-p evil-mode)
                (evil-insert-state)))))

;; 消除主题对终端的颜色影响
(add-hook 'vterm-mode-hook
          (lambda ()
            ;; same as my ghostty
            (set-face-attribute 'vterm-color-red nil
                                :foreground "#CD5C5C" :background "#CD5C5C")
            (set-face-attribute 'vterm-color-green nil
                                :foreground "#86AF80" :background "#86AF80")
            (set-face-attribute 'vterm-color-yellow nil
                                :foreground "#E8AE5B" :background "#E8AE5B")
            (set-face-attribute 'vterm-color-blue nil
                                :foreground "#6495ED" :background "#6495ED")
            (set-face-attribute 'vterm-color-magenta nil
                                :foreground "#DEB887" :background "#DEB887")
            (set-face-attribute 'vterm-color-cyan nil
                                :foreground "#B0C4DE" :background "#B0C4DE")
            (set-face-attribute 'vterm-color-white nil
                                :foreground "#BBAA99" :background "#BBAA99")))


(provide 'init-terminals)
;;; init-terminals.el ends here
