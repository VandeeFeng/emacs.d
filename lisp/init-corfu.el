;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

;; corfu
(require-package 'corfu)

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 7)        ;; Use scroll margin
  ;; :bind
  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  ;; (completion-preview-mode 1) ;; remove drop down
  ;; :custom
  ;; (orderless-define-completion-style orderless-fast
  ;;   (orderless-style-dispatchers '(orderless-fast-dispatch))
  ;;   (orderless-matching-styles '(orderless-literal orderless-regexp)))

  :config
  (setq corfu-count 6)
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)
                                         #'corfu-send))))
  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (setq-local corfu-auto nil)
  ;;             (corfu-mode)))

  ;; (setq global-corfu-minibuffer
  ;;       (lambda ()
  ;;         (not (or (bound-and-true-p mct--active)
  ;;                  (bound-and-true-p vertico--input)
  ;;                  (eq (current-local-map) read-passwd-map)))))

  ;; Disable ispell completion in text modes (Emacs 30+ adds it by default)
  ;; It will raise a error in corfu
  (add-hook 'text-mode-hook
            (lambda ()
              (remove-hook 'completion-at-point-functions
                           'ispell-completion-at-point t)))

  )

;;A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  ;; Support opening new minibuffers from inside existing minibuffers.
  ;;(enable-recursive-minibuffers t)
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;;(text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  ;; (setq minibuffer-prompt-properties
  ;;       '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))
  )

(use-package orderless
  :demand t
  :config
  ;; Define the dispatcher function first
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         (cons 'orderless-literal-prefix word)))

  ;; Then define the completion style
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  ;; Set global completion styles
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  ;; Set local corfu settings with orderless-fast
  (setq-local corfu-auto        t
              corfu-auto-delay  0.05
              corfu-auto-prefix 4 ;; 设定触发补全字符
              completion-styles '(orderless-fast basic))
  )

(setq tab-always-indent 'complete)
(when (maybe-require-package 'orderless)
  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic))))
(setq completion-category-defaults nil
      completion-category-overrides nil)
(setq completion-cycle-threshold 4)

(when (and (version< "28.1" emacs-version) (maybe-require-package 'corfu))
  (setq-default corfu-auto t)
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (setq-default corfu-quit-no-match 'separator)
  (add-hook 'after-init-hook 'global-corfu-mode)



  (with-eval-after-load 'corfu
    (corfu-popupinfo-mode))

  ;; Make Corfu also work in terminals, without disturbing usual behaviour in GUI
  (when (maybe-require-package 'corfu-terminal)
    (with-eval-after-load 'corfu
      (corfu-terminal-mode)))

  ;; TODO: https://github.com/jdtsmith/kind-icon
  )


(provide 'init-corfu)
;;; init-corfu.el ends here
