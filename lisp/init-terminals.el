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
            ;; One Dark 主题配色
            (set-face-attribute 'vterm-color-black nil
                                :foreground "#282c34" :background "#282c34")
            (set-face-attribute 'vterm-color-red nil
                                :foreground "#e06c75" :background "#e06c75")
            (set-face-attribute 'vterm-color-green nil
                                :foreground "#98c379" :background "#98c379")
            (set-face-attribute 'vterm-color-yellow nil
                                :foreground "#e5c07b" :background "#e5c07b")
            (set-face-attribute 'vterm-color-blue nil
                                :foreground "#61afef" :background "#61afef")
            (set-face-attribute 'vterm-color-magenta nil
                                :foreground "#c678dd" :background "#c678dd")
            (set-face-attribute 'vterm-color-cyan nil
                                :foreground "#56b6c2" :background "#56b6c2")
            (set-face-attribute 'vterm-color-white nil
                                :foreground "#abb2bf" :background "#abb2bf")))

(when (maybe-require-package 'eat)
  (defun sanityinc/on-eat-exit (process)
    (when (zerop (process-exit-status process))
      (kill-buffer)
      (unless (eq (selected-window) (next-window))
        (delete-window))))
  (add-hook 'eat-exit-hook 'sanityinc/on-eat-exit)

  (with-eval-after-load 'eat
    (custom-set-variables
     `(eat-semi-char-non-bound-keys
       (quote ,(cons [?\e ?w] (cl-remove [?\e ?w] eat-semi-char-non-bound-keys :test 'equal))))))

  (defcustom sanityinc/eat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "t") 'eat-other-window)
      map)
    "Prefix map for commands that create and manipulate eat buffers.")
  (fset 'sanityinc/eat-map sanityinc/eat-map)

  (global-set-key (kbd "C-c t") 'sanityinc/eat-map))



(provide 'init-terminals)
;;; init-terminals.el ends here
