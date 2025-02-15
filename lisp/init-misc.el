;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;-------------------------------------------------------------------------------------------
;;
;; globl settings
;;
;;------------------------------------------------------------------------------------------
;; 在启动时自动运行一次占卜
;; (add-hook 'emacs-startup-hook
;; (lambda ()
;; (with-current-buffer "*scratch*"
;; (goto-char (point-max))
;; (insert "\n\n;; 今日运势\n")
;; (insert (liu-yao-divination "今天运势如何？")))))

;; 启用系统复制粘贴
(global-set-key  (kbd "M-c") 'kill-ring-save) ; Cmd+C 复制 => command+w
(global-set-key (kbd "M-v") 'yank) ; Cmd+V 粘贴 => control+y

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
 ispell-program-name "/opt/homebrew/bin/ispell" ;ispell
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


;; jk 退出 insert
(with-eval-after-load 'evil
  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)))

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


;;---------------------------------------------------------------------------------
;; 显示图片
;;----------------------------------------------------------------------------------
;;https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E8%AE%BE%E7%BD%AEOrg%E4%B8%AD%E5%9B%BE%E7%89%87%E6%98%BE%E7%A4%BA%E7%9A%84%E5%B0%BA%E5%AF%B8.org
;; (setq org-image-actual-width '(400)) 要在(org-toggle-inline-images)命令之前
;; 或者在文档开头加上 #+ATTR_ORG: :width 600 ，并设置(setq org-image-actual-width nil)

(setq org-startup-with-inline-images t)

(add-hook 'org-mode-hook (lambda ()
                           (setq org-image-actual-width '(400))
                           (org-toggle-inline-images)
                           (when org-startup-with-inline-images
                             (org-display-inline-images t))))

;;-------------------------------------------------------------------------------------------
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



;;-------------------------------------------------------------------------------------------
;;
;; packages
;;
;;-------------------------------------------------------------------------------------------

;; 获取网页标题

;; https://emacs-china.org/t/emacs-firefox-org-link/23661/18
;; https://emacs-china.org/t/org-firefox/15100/4
;; (defun chinhant-grab-mac-link ()
;;   "获得并插入 Chrome 页面的 Markdown 链接."
;;   (interactive)
;;   (insert (grab-mac-link 'safari 'org)))

;; (global-set-key (kbd "C-c m") 'chinhant-grab-mac-link)

(use-package snap-indent
  :ensure t
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save t)))

(use-package org-cliplink
  :ensure t
  :defer t
  )
(global-set-key (kbd "M-l") 'org-cliplink)

;; gptel 设置默认ollama 模型
(use-package gptel
  :defer t
  :config
  (setq
   gptel-model "qwen2.5"
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '("qwen2.5:14b")))

  (gptel-make-ollama "Ollama"           ;Any name of your choosing
    :host "localhost:11434"             ;Where it's running
    :stream t                           ;Stream responses
    :models '("qwen2.5:14b"))           ;List of models

  (gptel-make-ollama "Deepseek"           ;Any name of your choosing
    :host "localhost:11434"             ;Where it's running
    :stream t                           ;Stream responses
    :models '("deepseek-r1:14b"))           ;List of models

  )

;; sis
;; https://github.com/laishulu/emacs-smart-input-source
(use-package sis
  :hook
  ;; enable the /context/ and /inline region/ mode for specific buffers
  (((text-mode prog-mode) . sis-context-mode)
   ((text-mode prog-mode) . sis-inline-mode))
  ;; :after evil
  :config
  ;; For MacOS
  (sis-ism-lazyman-config
   ;; English input source may be: "ABC", "US" or another one.
   ;; "com.apple.keylayout.ABC"
   "com.apple.keylayout.ABC"

   ;; Other language input source: "rime", "sogou" or another one.
   ;; "im.rime.inputmethod.Squirrel.Rime"
   "im.rime.inputmethod.Squirrel.Hans")

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
  (setq evil-collection-mode-list '(org org-capture calendar))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode

  (defvar my-intercept-mode-map (make-sparse-keymap)
    "High precedence keymap.")

  (define-minor-mode my-intercept-mode
    "Global minor mode for higher precedence evil keybindings."
    :global t)

  (my-intercept-mode)

  (dolist (state '(normal visual insert))
    (evil-make-intercept-map
     ;; NOTE: This requires an evil version from 2018-03-20 or later
     (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
     state))

  (evil-define-key 'normal my-intercept-mode-map
    (kbd "SPC n f") 'org-roam-node-find)

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
  (company-org-block-edit-style 'auto) ;; 'auto, 'inline, or 'prompt
  :config
  ;; 添加到 company-backends
  (add-to-list 'company-backends 'company-org-block)
  ;; 只在 org-mode 中启用
  :hook (org-mode . (lambda ()
                      (add-to-list (make-local-variable 'company-backends)
                                   'company-org-block))))

;;corfu
;;
(use-package corfu
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
  ;;:bind
  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  ;; :custom
  ;; (orderless-define-completion-style orderless-fast
  ;;   (orderless-style-dispatchers '(orderless-fast-dispatch))
  ;;   (orderless-matching-styles '(orderless-literal orderless-regexp)))

  :config
  (setq corfu-count 10)
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)
                                         #'corfu-send))))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))

  (setq global-corfu-minibuffer
        (lambda ()
          (not (or (bound-and-true-p mct--active)
                   (bound-and-true-p vertico--input)
                   (eq (current-local-map) read-passwd-map)))))
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
  :custom
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))
  :config
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         (cons 'orderless-literal-prefix word)))

  (setq-local corfu-auto        t
              corfu-auto-delay  0 ;; TOO SMALL - NOT RECOMMENDED
              corfu-auto-prefix 1 ;; TOO SMALL - NOT RECOMMENDED
              completion-styles '(orderless-fast basic))

  )


;; vertico
(use-package vertico
  ;;:ensure t
  :init
  (vertico-mode)
  ;; 不同的显示样式配置
  :custom
  ;; 基础配置
  (vertico-count 15)                    ; 显示候选项数量
  (vertico-resize t)                    ; 自动调整大小
  (vertico-cycle t)                     ; 循环滚动
  ;;(vterm-copy-mode t)
  ;; 使用buffer模式
  :config
  (vertico-buffer-mode)
  ;; buffer模式的详细配置
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (side . bottom)
          (window-height . 0.25)         ; 高度占比
          (window-parameters . ((no-other-window . t)
                                (mode-line-format . none)))))
  )

;; vterm
(defun vterm-minibuffer ()
  "Open vterm in minibuffer and enter insert state."
  (interactive)
  (let ((height (/ (frame-height) 3))) ; 设置高度为框架高度的1/3
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
  (setq vterm-min-window-width 30)
  (setq vterm-kill-buffer-on-exit t)  ; 退出时自动关闭buffer

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


;; ----------------------------------------------------------
;; 自动补全
;; ----------------------------------------------------------

;; https://emacs-china.org/t/deepseek-claude-gemini-ollama-minuet-ai-el/28715
;; (use-package minuet
;;   :ensure t
;;   :init
;;   (general-define-key
;;    ;; use completion-in-region for completion
;;    "M-y" #'minuet-completion-region
;;    ;; use overlay for completion
;;    "M-p" #'minuet-previous-suggestion ;; invoke completion or cycle to next completion
;;    "M-n" #'minuet-next-suggestion ;; invoke completion or cycle to previous completion
;;    "M-A" #'minuet-accept-suggestion ;; accept whole completion
;;    "M-a" #'minuet-accept-suggestion-line ;; accept current line completion
;;    "M-e" #'minuet-dismiss-suggestion)

;;   ;; 如需启用自动补全
;;   ;; 注意：即使不启用 minuet-auto-suggestion-mode，也可以手动触发补全
;;   (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

;;   :config
;;   (setq minuet-provider 'openai-fim-compatible)
;;   (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
;;   ;; an arbitrary non-null environment variable as placeholder
;;   (plist-put minuet-openai-fim-compatible-options :name "Ollama")
;;   (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
;;   (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:14b")
;;   )
;; we recommend using use-package to organize your init.el

;; codeium
(use-package codeium
  ;; if you use straight
  ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
  ;; otherwise, make sure that the codeium.el file is on load-path
  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  ;; or on a hook
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

  ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions
  ;;             (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
  ;; an async company-backend is coming soon!

  ;; codeium-completion-at-point is autoloaded, but you can
  ;; optionally set a timer, which might speed up things as the
  ;; codeium local language server takes ~0.2s to start up
  ;; (add-hook 'emacs-startup-hook
  ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

  ;; :defer t ;; lazy loading, if you want
  :config
  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; if you don't want to use customize to save the api-key
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; you can also set a config for a single buffer like this:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; ends ----------------------------------------------------------


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
