;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================
;; global settings
;;==============================

;; When you press C-h f or C-h v, Emacs opens the help buffer but leaves your cursor in the original window. You almost always want to read the help right away, so you end up pressing C-x o every single time. This fixes it:
(setq help-window-select t)

;; Skip Fontification During Input
;; Emacs normally fontifies (syntax-highlights) text even while you’re actively typing. This can cause micro-stutters, especially in tree-sitter modes or large buffers. One setting fixes it:
(setq redisplay-skip-fontification-on-input t)

;; disable SPC for scroll-up-command in message buffer
(with-eval-after-load 'simple
  (define-key messages-buffer-mode-map (kbd "SPC") 'hydra-leader/body))

;; Auto switch to compilation buffer after compile finishes
(add-hook 'compilation-finish-functions
          (lambda (buffer _status)
            (select-window (get-buffer-window buffer))))

;; 设置 rg 为默认的 grep
(setq grep-program "rg")
;; 设置默认 compile 指令
(setq compile-command "")

;; 关闭 warning
;; (setq warning-minimum-level :emergency)

;; 禁止eww生成cookie
;; https://github.com/lujun9972/lujun9972.github.com/blob/source/Emacs%E4%B9%8B%E6%80%92/%E5%A6%82%E4%BD%95%E7%A6%81%E6%AD%A2eww%E7%94%9F%E6%88%90cookie.org
(setq url-cookie-trusted-urls '()        ;不设置白名单
      url-cookie-untrusted-urls '(".*")) ;所有内容都匹配黑名单

(setq-default
 window-combination-resize t
 x-stretch-cursor t
 yas-triggers-in-field t
 )

(setq
 display-line-numbers-type 'relative ;开启相对行号，需要关闭 line-number-mode
 ;; ispell-program-name "/opt/homebrew/bin/ispell" ;ispell
 confirm-kill-emacs nil ;;取消退出确认
 undo-limit 80000000
 auto-save-default t
 word-wrap-by-category t
 all-the-icons-scale-factor 1.0
 )
(global-subword-mode t) ;; 启用 global-subword-mode 后，Emacs 会在全局范围内使用 subword-mode，这意味着在所有的缓冲区中，你都可以进行子词的导航和编辑。这在处理代码或文本时非常有用，特别是当你需要对单个字符或字符组合进行精确编辑时。

;;auto-wrap
(custom-set-variables
 '(global-visual-line-mode t)
 '(global-auto-revert-mode t))

;; SmoothScroll
;; (require 'ultra-scroll)
;; (ultra-scroll-mode 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))
(setq scroll-preserve-screen-position 'always)
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 10000) ;101 , 10000
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
;; -SmoothScroll


;; 显示图片
;;https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E8%AE%BE%E7%BD%AEOrg%E4%B8%AD%E5%9B%BE%E7%89%87%E6%98%BE%E7%A4%BA%E7%9A%84%E5%B0%BA%E5%AF%B8.org
;; (setq org-image-actual-width '(400)) 要在(org-toggle-inline-images)命令之前
;; 或者在文档开头加上 #+ATTR_ORG: :width 600 ，并设置(setq org-image-actual-width nil)

(setq org-startup-with-inline-images t)

(add-hook 'org-mode-hook (lambda ()
                           (setq org-image-actual-width '(400))
                           (org-toggle-inline-images)
                           (when org-startup-with-inline-images
                             (org-display-inline-images t))))


;; 窗口大小设定
;; 霞鹜文楷等宽窗口大小
;; (if (not (eq window-system nil))
;;     (progn
;;       ;; top, left ... must be integer
;;       (add-to-list 'default-frame-alist
;;                    (cons 'top  (/ (x-display-pixel-height) 15))) ;; 调整数字设置距离上下左右的距离
;;       (add-to-list 'default-frame-alist
;;                    (cons 'left (/ (x-display-pixel-width) 6)))
;;       (add-to-list 'default-frame-alist
;;                    (cons 'height (/ (* 4 (x-display-pixel-height))
;;                                     (* 6 (frame-char-height)))))
;;       (add-to-list 'default-frame-alist
;;                    (cons 'width (/ (* 4 (x-display-pixel-width))
;;                                    (* 6 (frame-char-width)))))))


;;------------------------------
;; nonote
;;------------------------------

(global-set-key (kbd "C-c v") #'corfu-ws-complete-vector)
(global-set-key (kbd "C-c r") #'corfu-ws-complete-rag)

;;; nonote ends

;; 在启动时自动运行一次占卜
(require 'gua)
(setq gua-llm-enabled t)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (with-current-buffer "*scratch*"
;;               (goto-char (point-max))
;;               (insert "\n\n;; 今日运势\n")
;;               (gua-divination
;;                "今天运势如何？"
;;                (lambda (result)
;;                  (with-current-buffer "*scratch*"
;;                    (goto-char (point-max))
;;                    (insert result)))))))



;;==============================
;; packages
;;==============================
(require 'hoarder)

(require 'org-hover)

;; pdf
(maybe-require-package 'pdf-tools)

;; empv.el
;; https://github.com/isamert/empv.el
;; (require 'empv)

;; ready-player
;; https://github.com/xenodium/ready-player
;; https://xenodium.com/ready-player-mode/
(use-package ready-player
  :ensure t
  :config
  (ready-player-mode +1))

(require 'video-trimmer)

;; 获取网页标题

;; https://emacs-china.org/t/emacs-firefox-org-link/23661/18
;; https://emacs-china.org/t/org-firefox/15100/4
;; (defun chinhant-grab-mac-link ()
;;   "获得并插入 Chrome 页面的 Markdown 链接."
;;   (interactive)
;;   (insert (grab-mac-link 'safari 'org)))

;; (global-set-key (kbd "C-c m") 'chinhant-grab-mac-link)

(use-package org-cliplink
  :ensure t
  :defer t
  )
(global-set-key (kbd "M-u") 'org-cliplink)


;; rime
;; (require 'rime)
;; (setq rime-share-data-dir "~/.local/share/fcitx5/rime")
;; (setq rime-user-data-dir "~/.config/emacs-rime")
;; (require 'posframe)
;; (setq rime-posframe-properties
;;       (list :background-color "#1D1F21"
;;             :foreground-color "#d1d5db"
;;             ;; :font "WenQuanYi Micro Hei Mono-14"
;;             :internal-border-width 10
;;             :override-redirect t))

;; (setq default-input-method "rime"
;;       rime-show-candidate 'popup)  ;;posframe 会出现 Emacs frame 变色的问题
;; (global-set-key (kbd "C-SPC") 'toggle-input-method)

;; sis
;; https://github.com/laishulu/emacs-smart-input-source
(use-package sis
  :ensure t
  :after evil
  ;; :hook
  ;; ;; enable the /context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  ;; (sis-ism-lazyman-config "british" "rime" 'fcitx5) ;; 使用内置的 rime 的时候
  ;; enable the /cursor color/ mode
  ;;(sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  ;; (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  ;; (sis-global-inline-mode t)
  )

;; evil settings
(use-package evil
  :demand t
  :ensure t
  :init
  (evil-mode)
  :config
  (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo ;; Adds vim-like C-r redo functionality
        evil-clipboard-enable t)
  )


(use-package evil-collection
  :init
  :defer t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t) ;;如果您想在迷你缓冲区中启用 Evil，则必须通过将 evil-collection-setup-minibuffer 自定义为 t 来显式打开它。一些与迷你缓冲区相关的软件包（例如 Helm）依赖于此选项。
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here
  ;; for documentation purposes in case you need it.
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (setq evil-collection-mode-list '(org org-capture calendar dired))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode

  ;; (defvar my-intercept-mode-map (make-sparse-keymap)
  ;;   "High precedence keymap.")

  ;; (define-minor-mode my-intercept-mode
  ;;   "Global minor mode for higher precedence evil keybindings."
  ;;   :global t)

  ;; (my-intercept-mode)

  ;; (dolist (state '(normal visual insert))
  ;;   (evil-make-intercept-map
  ;;    ;; NOTE: This requires an evil version from 2018-03-20 or later
  ;;    (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
  ;;    state))

  ;; (evil-define-key 'normal my-intercept-mode-map
  ;;   (kbd "SPC n f") 'org-roam-node-find)

  (evil-collection-init))



;; Using RETURN to follow links in Org/Evil
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
;;(setq org-return-follows-link  t)



;; buffer
;; 折叠buffer
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (setq ibuffer-hidden-filter-groups (list "Helm" "*Internal*"))
            (ibuffer-update nil t)
            )
          )


;; which-key  https://emacs-china.org/t/doom/13654/5
(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-allow-imprecise-window-fit nil
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil
        which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.01
        which-key-separator " → " ))

;;neotree
(use-package neotree
  :defer t
  :ensure t
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action)
  ;; truncate long file names in neotree
  (add-hook 'neo-after-create-hook
            #'(lambda (_)
                (with-current-buffer (get-buffer neo-buffer-name)
                  (setq truncate-lines t)
                  (setq word-wrap nil)
                  (make-local-variable 'auto-hscroll-mode)
                  (setq auto-hscroll-mode nil)))))

;; company
(use-package company
  :defer 0.1
  :config
  (global-company-mode t)
  (setq-default
   company-idle-delay 0.05
   company-require-match nil
   company-minimum-prefix-length 2

   ;; get only preview
   company-frontends '(company-preview-frontend)
   ;; also get a drop down
   ;; company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
   ))

(use-package company-org-block
  :ensure t
  :after (company org)
  :custom
  (company-org-block-edit-style 'inline) ;; 'auto, 'inline, or 'prompt
  ;; 妈的，一直之前用的 auto,会弹出一个 minibuffer
  :config
  ;; 添加到 company-backends
  (add-to-list 'company-backends 'company-org-block)
  ;; 只在 org-mode 中启用
  :hook (org-mode . (lambda ()
                      (add-to-list (make-local-variable 'company-backends)
                                   'company-org-block))))

;; ;; company - completion backend for cape
;; (use-package company
;;   :ensure t
;;   :config
;;   ;; Configure company backends
;;   (setq company-backends
;;         '(company-capf           ; Use completion-at-point-functions
;;           company-dabbrev        ; Complete from current buffers
;;           company-files          ; File completion
;;           company-elisp          ; Elisp symbols
;;           company-abbrev         ; Abbreviations
;;           ))

;;   ;; Don't enable company-mode globally since we use corfu
;;   ;; Company will only be used as backend via cape
;;   )

;; ;; cape - completion at point extensions
;; (use-package cape
;;   :ensure t
;;   :after company
;;   :init
;;   ;; Add to completion-at-point-functions hook
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
;;   :config
;;   ;; Function to add company backends as capfs
;;   (defun my-setup-company-backends ()
;;     "Convert company backends to capfs and add them to completion-at-point-functions."
;;     (when (bound-and-true-p company-backends)
;;       (setq-local completion-at-point-functions
;;                   (append (mapcar #'cape-company-to-capf company-backends)
;;                           completion-at-point-functions))))

;;   ;; Add company backends for programming modes
;;   (add-hook 'prog-mode-hook #'my-setup-company-backends)
;;   )



;; Misc config - yet to be placed in separate files

(add-auto-mode 'tcl-mode "^Portfile\\'")

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'conf-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'sanityinc/set-mode-for-new-scripts)

(defun sanityinc/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))


(when (maybe-require-package 'info-colors)
  (with-eval-after-load 'info
    (add-hook 'Info-selection-hook 'info-colors-fontify-node)))


;; Handle the prompt pattern for the 1password command-line interface
(with-eval-after-load 'comint
  (setq comint-password-prompt-regexp
        (concat
         comint-password-prompt-regexp
         "\\|^Please enter your password for user .*?:\\s *\\'")))



(when (maybe-require-package 'regex-tool)
  (setq-default regex-tool-backend 'perl))

(with-eval-after-load 're-builder
  ;; Support a slightly more idiomatic quit binding in re-builder
  (define-key reb-mode-map (kbd "C-c C-k") 'reb-quit))

(add-auto-mode 'conf-mode "^Procfile\\'")


(provide 'init-misc)
;;; init-misc.el ends here
