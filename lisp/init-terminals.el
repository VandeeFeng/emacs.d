;;; init-terminals.el --- Terminal emulators          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; https://github.com/dakra/ghostel
(require-package 'ghostel)

(defun ghostel-minibuffer ()
  "Open ghostel in minibuffer."
  (interactive)
  (let ((height (/ (frame-height) 3)))
    (with-temp-buffer
      (let ((window (split-window-vertically (- height))))
        (select-window window)
        (ghostel)
        (when (bound-and-true-p evil-mode)
          (evil-insert-state))))))

(defalias 'gt 'ghostel-minibuffer)

(with-eval-after-load 'ghostel
  (evil-define-key '(normal insert) ghostel-mode-map (kbd "C-y") #'ghostel-yank)
  (evil-define-key '(normal) ghostel-mode-map (kbd "p") #'ghostel-yank)
  (evil-define-key '(normal) ghostel-mode-map (kbd "u") #'ghostel-undo)
  (evil-define-key 'normal ghostel-mode-map (kbd "q") #'delete-window)
  ;; https://dakra.github.io/ghostel/#shell-integration
  ;; prevent zoxide error report
  ;; add
  ;; if [[ "${INSIDE_EMACS%%,*}" = 'ghostel' ]]; then
  ;; source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.bash"
  ;; fi
  ;; before eval "$(zoxide init --cmd cd bash)"
  (setq ghostel-shell-integration nil)

  (add-hook 'ghostel-mode-hook
            (lambda ()
              (when (bound-and-true-p evil-mode)
                (evil-insert-state))))

  (add-hook 'ghostel-mode-hook
            (lambda ()
              ;; same as my ghostty
              (set-face-attribute 'ghostel-color-red nil
                                  :foreground "#CD5C5C" :background "#CD5C5C")
              (set-face-attribute 'ghostel-color-green nil
                                  :foreground "#86AF80" :background "#86AF80")
              (set-face-attribute 'ghostel-color-yellow nil
                                  :foreground "#E8AE5B" :background "#E8AE5B")
              (set-face-attribute 'ghostel-color-blue nil
                                  :foreground "#6495ED" :background "#6495ED")
              (set-face-attribute 'ghostel-color-magenta nil
                                  :foreground "#DEB887" :background "#DEB887")
              (set-face-attribute 'ghostel-color-cyan nil
                                  :foreground "#B0C4DE" :background "#B0C4DE")
              (set-face-attribute 'ghostel-color-white nil
                                  :foreground "#BBAA99" :background "#BBAA99")))
  )

(provide 'init-terminals)
;;; init-terminals.el ends here
