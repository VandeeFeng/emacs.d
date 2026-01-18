;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; (when (maybe-require-package 'diredfl)
;;   (with-eval-after-load 'dired
;;     (diredfl-global-mode)
;;     (require 'dired-x)))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(when (maybe-require-package 'diff-hl)
  (with-eval-after-load 'dired
    (require 'dired-x)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)

  ;; Close dired buffer after opening a file
  (defun my/dired-find-file-and-kill ()
    "Open file in dired and kill the dired buffer when entering a directory."
    (interactive)
    (let ((current-buffer (current-buffer))
          (dired-dir (dired-current-directory)))
      (dired-find-file)
      (when (and (not (eq current-buffer (current-buffer)))
                 (derived-mode-p 'dired-mode))
        (message "Closed dired buffer: %s" dired-dir)
        (kill-buffer current-buffer))))

  (define-key dired-mode-map (kbd "RET") 'my/dired-find-file-and-kill)
  (define-key dired-mode-map (kbd "S-<right>") 'my/dired-find-file-and-kill)
  (define-key dired-mode-map (kbd "S-<left>") #'dired-up-directory)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "+") 'dired-create-empty-file)

  ;; Set wdired-mode initial state to normal
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'wdired-mode 'normal))

  (defun my/dired-enter-edit-mode ()
    "Toggle dired read-only mode and switch to Evil insert state."
    (interactive)
    (dired-toggle-read-only)
    (when (fboundp 'evil-insert-state)
      (evil-insert-state)))

  (defun my/dired-enter-visual-edit-mode ()
    "Toggle dired read-only mode and switch to Evil visual state."
    (interactive)
    (dired-toggle-read-only)
    (when (fboundp 'evil-visual-state)
      (evil-visual-state)))

  (defun my/dired-exit-edit-mode ()
    "Exit dired read-only mode and switch to Evil normal state."
    (interactive)
    (when (derived-mode-p 'wdired-mode)
      (wdired-finish-edit)
      (when (fboundp 'evil-normal-state)
        (evil-normal-state))))

  (define-key dired-mode-map (kbd "i") #'my/dired-enter-edit-mode)
  (define-key dired-mode-map (kbd "a") #'my/dired-enter-edit-mode)
  (define-key dired-mode-map (kbd "v") #'my/dired-enter-visual-edit-mode)

  ;; Exit edit mode with Esc or q
  (add-hook 'wdired-mode-hook
            (lambda ()
              (when (fboundp 'evil-define-key)
                (evil-define-key 'normal wdired-mode-map
                  (kbd "<escape>") #'my/dired-exit-edit-mode
                  (kbd "q") #'my/dired-exit-edit-mode))
              (unless (fboundp 'evil-define-key)
                (local-set-key (kbd "<escape>") #'my/dired-exit-edit-mode)
                (local-set-key (kbd "q") #'my/dired-exit-edit-mode))))
  )

(provide 'init-dired)
;;; init-dired.el ends here
