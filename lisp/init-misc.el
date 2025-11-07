;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================
;; globl settings
;;==============================

;; use sanityinc/headeries-elisp instead
;; ;; auto insert header of .el files
;; (require 'autoinsert)
;; (auto-insert-mode 1)
;; (setq auto-insert-query nil)
;; (define-auto-insert
;;   "\\.el\\'"
;;   '("Emacs Lisp file header\n"
;;     ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "-*- lexical-binding: t -*-" "\n"
;;     ";; Author: Vandee\n"
;;     ";; Created: " (format-time-string "%Y-%m-%d") "\n"
;;     ";; Keywords: \n"
;;     ";;; Commentary:\n"
;;     ";;; Code:\n\n\n"
;;     ";;; " (file-name-nondirectory buffer-file-name) " ends here\n"))

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

;; 没作用
;; (defun my-modify-syntax-for-chinese ()
;;   "Modify syntax table to treat each Chinese character as a word."
;;   (modify-syntax-entry ?\\ "w" (standard-syntax-table)) ; 避免反斜杠干扰
;;   (dolist (char (number-sequence #x4e00 #x9fff)) ; 汉字的 Unicode 范围
;;     (modify-syntax-entry char "w" (standard-syntax-table))))

;; (add-hook 'evil-local-mode-hook 'my-modify-syntax-for-chinese)

;;------------------------------
;; nonote
;;------------------------------
(require 'hoarder)

;; uvicorn app.main:app --reload
;; (maybe-require-package 'websocket)
(require 'json)
(require 'corfu)


(defun run-uvicorn-server-uv ()
  "Run uv python -m uvicorn app.main:app --reload in a new async shell using absolute path."
  (interactive)
  (let ((default-directory "~/Vandee/Projects/NoNotes/"))
    (async-shell-command
     "uv run uvicorn app.main:app --reload"
     "*Uvicorn Server*")))

;; Common websocket connection function
(defvar corfu-ws-endpoint-rag "ws://localhost:8000/api/v1/ws/complete/rag/emacs-corfu"
  "WebSocket endpoint for RAG-based completion.")

(defvar corfu-ws-endpoint-vector "ws://localhost:8000/api/v1/ws/complete/vector/emacs-corfu"
  "WebSocket endpoint for vector-based completion.")

(defvar corfu-ws--client nil
  "WebSocket client connection.")

(defvar corfu-ws--current-endpoint nil
  "Current WebSocket endpoint in use.")

(defvar corfu-ws--pending nil
  "Callback function to be called when receiving completion response.")

(defvar corfu-ws--debounce-timer nil
  "Timer for debouncing completion requests.")

(defvar corfu-ws--debounce-delay 0.3
  "Delay in seconds for debouncing completion requests.")

(defun corfu-ws-connect (endpoint)
  "Establish websocket connection to ENDPOINT if not already connected."
  (when (and corfu-ws--client
             (websocket-openp corfu-ws--client)
             (not (string= endpoint corfu-ws--current-endpoint)))
    (websocket-close corfu-ws--client)
    (setq corfu-ws--client nil))

  (unless (and corfu-ws--client (websocket-openp corfu-ws--client))
    (setq corfu-ws--client
          (websocket-open
           endpoint
           :on-message (lambda (_ws frame)
                         (let* ((data (websocket-frame-payload frame))
                                (json (json-parse-string data :object-type 'alist))
                                (suggestions (mapcar (lambda (item)
                                                       (alist-get 'text item))
                                                     (alist-get 'suggestions json))))
                           (when corfu-ws--pending
                             (funcall corfu-ws--pending suggestions)
                             (setq corfu-ws--pending nil))))))
    (setq corfu-ws--current-endpoint endpoint)))

(defun corfu-ws-request-debounced (prefix callback endpoint max-results)
  "Debounced version of corfu-ws-request."
  (when corfu-ws--debounce-timer
    (cancel-timer corfu-ws--debounce-timer))
  (setq corfu-ws--debounce-timer
        (run-with-timer corfu-ws--debounce-delay nil
                        #'corfu-ws-request prefix callback endpoint max-results)))

(defun corfu-ws-request (prefix callback endpoint max-results)
  "Send completion request for PREFIX to ENDPOINT, call CALLBACK with results.
Optional MAX-RESULTS limits the number of suggestions (defaults to 5)."
  (corfu-ws-connect endpoint)
  (let* ((request (json-encode `(("text" . ,prefix)
                                 ("cursor_position" . ,(length prefix))
                                 ("max_results" . ,(or max-results 5))))))
    (setq corfu-ws--pending callback)
    (websocket-send-text corfu-ws--client request)))

(defun corfu-ws-capf (endpoint max-results)
  "Create a completion-at-point-function using ENDPOINT and MAX-RESULTS suggestions."
  (let* ((start (save-excursion (skip-syntax-backward "w_") (point)))
         (end (point)))
    (list start end
          (lambda (str pred action)
            (if (eq action 'metadata)
                '(metadata (category . corfu-ws))
              (let ((cont (lambda (cands)
                            (funcall completion-in-region-function start end cands))))
                (if (string= endpoint corfu-ws-endpoint-rag)
                    ;; Use debounced version for RAG completion
                    (corfu-ws-request-debounced (buffer-substring-no-properties start end) cont endpoint max-results)
                  ;; Use normal version for vector completion
                  (corfu-ws-request (buffer-substring-no-properties start end) cont endpoint max-results))
                nil))))))

;; Specific completion functions for different modes
(defun corfu-ws-complete-rag ()
  "Manually trigger RAG-based completion with single result."
  (interactive)
  (let ((completion-at-point-functions (list (lambda () (corfu-ws-capf corfu-ws-endpoint-rag 1)))))
    (completion-at-point)))

(defun corfu-ws-complete-vector ()
  "Manually trigger vector-based completion with multiple results."
  (interactive)
  (let ((completion-at-point-functions (list (lambda () (corfu-ws-capf corfu-ws-endpoint-vector 5)))))
    (completion-at-point)))

;; origin
;; (defun corfu-ws-connect-vector ()
;;   (unless (and corfu-ws--client (websocket-openp corfu-ws--client))
;;     (setq corfu-ws--client
;;           (websocket-open
;;            "ws://localhost:8000/api/v1/ws/complete/rag/emacs-corfu"
;;            :on-message (lambda (_ws frame)
;;                          (let* ((data (websocket-frame-payload frame))
;;                                 (json (json-parse-string data :object-type 'alist))
;;                                 (suggestions (mapcar (lambda (item)
;;                                                        (alist-get 'text item))
;;                                                      (alist-get 'suggestions json))))
;;                            (when corfu-ws--pending
;;                              (funcall corfu-ws--pending suggestions)
;;                              (setq corfu-ws--pending nil))))))))

;; (defun corfu-ws-request-vector (prefix callback)
;;   (corfu-ws-connect-vector)
;;   (let* ((request (json-encode `(("text" . ,prefix)
;;                                  ("cursor_position" . ,(length prefix))
;;                                  ("max_results" . 5)))))
;;     (setq corfu-ws--pending callback)
;;     (websocket-send-text corfu-ws--client request)))


;; ;; (defun corfu-ws-capf ()
;; ;;   "A completion-at-point-function for corfu using websocket backend."
;; ;;   (let* ((end (point))
;; ;;          ;; 获取当前单词的开始位置，用于显示补全
;; ;;          (word-start (save-excursion (skip-syntax-backward "w_") (point)))
;; ;;          ;; 获取行首位置，用于上下文
;; ;;          (line-start (line-beginning-position)))
;; ;;     (list word-start end
;; ;;           (lambda (str pred action)
;; ;;             (if (eq action 'metadata)
;; ;;                 '(metadata (category . corfu-ws))
;; ;;               (let ((cont (lambda (cands)
;; ;;                             (funcall completion-in-region-function word-start end cands))))
;; ;;                 ;; 发送从行首到光标的所有内容
;; ;;                 (corfu-ws--request (buffer-substring-no-properties line-start end) cont)
;; ;;                 nil))))))

;; (defun corfu-ws-capf-vector ()
;;   "A completion-at-point-function for corfu using websocket backend."
;;   (let* ((start (save-excursion (skip-syntax-backward "w_") (point)))
;;          (end (point)))
;;     (list start end
;;           (lambda (str pred action)
;;             (if (eq action 'metadata)
;;                 '(metadata (category . corfu-ws))
;;               (let ((cont (lambda (cands)
;;                             (funcall completion-in-region-function start end cands))))
;;                 (corfu-ws-request-rag (buffer-substring-no-properties start end) cont)
;;                 nil))))))


;; ;; (add-hook 'completion-at-point-functions #'corfu-ws-capf)

;; (defun corfu-ws-complete-vector ()
;;   "Manually trigger websocket completion at point."
;;   (interactive)
;;   (let ((completion-at-point-functions '(corfu-ws-capf-vector)))
;;     (completion-at-point)))


(global-set-key (kbd "C-c v") #'corfu-ws-complete-vector)
(global-set-key (kbd "C-c r") #'corfu-ws-complete-rag)

;;; nonote ends

;; 在启动时自动运行一次占卜
(require 'gua.el)
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

;; 设置默认 compile 指令
(setq compile-command "")

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



;;==============================
;; packages
;;==============================

;; pdf
(maybe-require-package 'pdf-tools)

;; 获取网页标题

;; https://emacs-china.org/t/emacs-firefox-org-link/23661/18
;; https://emacs-china.org/t/org-firefox/15100/4
;; (defun chinhant-grab-mac-link ()
;;   "获得并插入 Chrome 页面的 Markdown 链接."
;;   (interactive)
;;   (insert (grab-mac-link 'safari 'org)))

;; (global-set-key (kbd "C-c m") 'chinhant-grab-mac-link)

;; yasnippet
(use-package yasnippet
  :ensure t
  )

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

(use-package snap-indent
  :ensure t
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save t)))

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
  (setq evil-collection-mode-list '(org org-capture calendar))
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

;; (use-package company-org-block
;;   :ensure t
;;   :after (company org)
;;   :custom
;;   (company-org-block-edit-style 'inline) ;; 'auto, 'inline, or 'prompt
;;   ;; 妈的，一直之前用的 auto,会弹出一个 minibuffer
;;   :config
;;   ;; 添加到 company-backends
;;   (add-to-list 'company-backends 'company-org-block)
;;   ;; 只在 org-mode 中启用
;;   :hook (org-mode . (lambda ()
;;                       (add-to-list (make-local-variable 'company-backends)
;;                                    'company-org-block))))

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
  (corfu-preview-current nil)    ;; Disable current candidate preview
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
