;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;-------------------------------------------------------------------------------------------
;;
;; globl settings
;;
;;-------------------------------------------------------------------------------------------
;;
;;
;; onedark theme

;; (add-to-list 'custom-theme-load-path "~/.config/doom/atom-one-dark-theme/")
;; (load-theme 'atom-one-dark t)
;; (setq doom-theme 'atom-one-dark)

;; SmoothScroll
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
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


;; jk 退出 insert
(with-eval-after-load 'evil
  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)))
;;--------------------------------------------
;; modeline 里的彩虹猫！
;;--------------------------------------------
;; modeline 里的彩虹猫！
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1)
  (setq mode-line-format
        (list
         '(:eval (list (nyan-create)))
         ))
  )


;;
;;取消退出确认
(setq confirm-kill-emacs nil)


;; ispell
(setq ispell-program-name "/opt/homebrew/bin/ispell")
;;
;;auto-wrap
(custom-set-variables
 '(global-visual-line-mode t)
 '(global-auto-revert-mode t))

;; 开启相对行号
;; (setq display-line-numbers-type 'relative)  ;;不起作用
;;
;;
;;;;-------------------------------------------------------------------------------------------
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


(setq-default
 window-combination-resize t
 x-stretch-cursor t
 yas-triggers-in-field t
 )

(setq
 undo-limit 80000000
 auto-save-default t
 scroll-preserve-screen-position 'always
 scroll-margin 2
 word-wrap-by-category t
 all-the-icons-scale-factor 1.0
 )
(global-subword-mode t) ;; 启用 global-subword-mode 后，Emacs 会在全局范围内使用 subword-mode，这意味着在所有的缓冲区中，你都可以进行子词的导航和编辑。这在处理代码或文本时非常有用，特别是当你需要对单个字符或字符组合进行精确编辑时。
;;
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




;; gptel 设置默认ollama 模型
(use-package gptel
  :defer t
  :config
  (setq
   gptel-model "qwen2.5"
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '("qwen2.5")))

  (gptel-make-ollama "Ollama"             ;Any name of your choosing
    :host "localhost:11434"               ;Where it's running
    :stream t                             ;Stream responses
    :models '("qwen2.5:latest"))           ;List of models
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
        evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
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
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (defvar my-intercept-mode-map (make-sparse-keymap)
    "High precedence keymap.")

  (define-minor-mode my-intercept-mode
    "Global minor mode for higher precedence evil keybindings."
    :global t)

  (my-intercept-mode)


  (evil-collection-init))




(use-package evil-tutor)

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



(use-package flycheck
  :config
  (remove-hook 'text-mode-hook 'flyspell-mode)
  (remove-hook 'org-mode-hook 'flyspell-mode)

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
  :ensure t
  :init
  (vertico-mode)
  ;; 不同的显示样式配置
  :custom
  ;; 基础配置
  (vertico-count 15)                    ; 显示候选项数量
  (vertico-resize t)                    ; 自动调整大小
  (vertico-cycle t)                     ; 循环滚动

  ;; 使用buffer模式
  :config
  (vertico-buffer-mode)
  ;; buffer模式的详细配置
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (side . bottom)
          (window-height . 0.3)         ; 高度占比
          (window-parameters . ((no-other-window . t)
                                (mode-line-format . none)))))
  )

;; vterm
(defun vterm-minibuffer ()
  "Open vterm in minibuffer."
  (interactive)
  (let ((height (/ (frame-height) 3))) ; 设置高度为框架高度的1/3
    (with-temp-buffer
      (let ((window (split-window-vertically (- height))))
        (select-window window)
        (vterm)))))

;; 定义一个更简单的命令别名
(defalias 'vt 'vterm-minibuffer)

;; 自定义 vterm 在 minibuffer 中的行为
(with-eval-after-load 'vterm
  (setq vterm-min-window-width 30)
  (setq vterm-kill-buffer-on-exit t)  ; 退出时自动关闭buffer
  )
;;----------------------------------------------
;; org-blog
;; ----------------------------------------------
;; org-static-blog config

(setq org-static-blog-publish-title "Vandee's Blog")
(setq org-static-blog-publish-url "https://www.vandee.art/")
(setq org-static-blog-publish-directory "~/vandee/org-blog/")
(setq org-static-blog-posts-directory "~/vandee/org-blog/posts/")
(setq org-static-blog-drafts-directory "~/vandee/org-blog/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc t)
(setq org-export-with-section-numbers nil)
(setq org-static-blog-use-preview t)
(setq org-static-blog-enable-og-tags t)
;; (setq org-static-blog-rss-max-entries 30) ;; 设置 rss 获取文章的最大数量
;; (setq org-static-blog-index-length 8) ;; 首页包含了最近几篇博客文章，显示在同一个页面上。首页上的条目数量可以通过设置 org-static-blog-index-length 来自定义。
;;        <script src=\"https://lf26-cdn-tos.bytecdntp.com/cdn/expire-1-M/vanilla-lazyload/17.3.1/lazyload.min.js\" type=\"application/javascript\" defer></script>
;; <script src=\"https://testingcf.jsdelivr.net/gh/vandeefeng/gitbox@main/codes/blogsummary.js\"></script>
(setq org-static-blog-page-header
      "<meta name=\"author\" content=\"Vandee\">
       <meta name=\"referrer\" content=\"no-referrer\">
       <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">

       <link rel=\"stylesheet\" href=\"assets/css/style.css\" type=\"text/css\"/>
       <link rel=\"stylesheet\"
             href=\"https://lf26-cdn-tos.bytecdntp.com/cdn/expire-1-M/font-awesome/6.0.0/css/all.min.css\"/>
       <link rel=\"stylesheet\"
             href=\"https://testingcf.jsdelivr.net/npm/@fancyapps/ui@4.0.12/dist/fancybox.css\"/>
       <link rel=\"icon\" type=\"image/x-icon\" href=\"/static/favicon.ico\"/>

       <script src=\"https://lf6-cdn-tos.bytecdntp.com/cdn/expire-1-M/jquery/3.6.0/jquery.min.js\" defer></script>
       <script src=\"https://testingcf.jsdelivr.net/npm/@fancyapps/ui@4.0.12/dist/fancybox.umd.js\" defer></script>
       <script src=\"https://lf3-cdn-tos.bytecdntp.com/cdn/expire-1-M/pangu/4.0.7/pangu.min.js\" defer></script>
       <script defer>
         document.addEventListener(\"DOMContentLoaded\", function () {
           pangu.spacingPage();
         });
       </script>

       <script src=\"assets/js/app.js\" defer></script>
       <script src=\"assets/js/copyCode.js\" defer></script>
       <script src=\"assets/js/search.js\" defer></script>")


;; Preamble for every page (e.g., navigation)
(setq org-static-blog-page-preamble
      (format "
      <header>
      <h1><a href=\"%s\">Vandee's Blog</a></h1>
      <nav>
      <a href=\"%s\">Home</a>
      <a href=\"https://wiki.vandee.art\">Wiki</a>
      <a href=\"archive.html\">Archive</a>
      <a href=\"tags.html\">Tags</a>
      <div id=\"search-container\">
        <input type=\"text\" id=\"search-input\" placeholder=\"Search anywhere...\">
        <i class=\"fas fa-search search-icon\"></i>
      </div>
      </nav>
      </header>"
              org-static-blog-publish-url
              org-static-blog-publish-url))

;; Postamble for every page (e.g., footer)
(setq org-static-blog-page-postamble
      "<div id=\"search-results\"></div>
      <footer>
         <p>
            © 2022-<script>document.write(new Date().getFullYear())</script> Vandee. All rights reserved.
         </p>
        <div class=\"social-links\"></div>
      </footer>

      <a href=\"#top\" aria-label=\"go to top\" title=\"Go to Top (Alt + G)\"
         class=\"top-link\" id=\"top-link\" accesskey=\"g\">
         <i class=\"fa-solid fa-angle-up fa-2xl\"></i>
      </a>

      <script>
        var mybutton = document.getElementById('top-link');
        window.onscroll = function () {
            if (document.body.scrollTop > 800 || document.documentElement.scrollTop > 800) {
                mybutton.style.visibility = 'visible';
                mybutton.style.opacity = '1';
            } else {
                mybutton.style.visibility = 'hidden';
                mybutton.style.opacity = '0';
            }
        };
      </script>")



;; Content for the front page
(setq org-static-blog-index-front-matter
      "<h1>Vandee</h1>
      <p>搞点摄影，喜欢音乐和艺术，保持热爱。</p>
      <p>如果你也喜欢王小波、李志，我们就是朋友。</p>
      <p>Stay foolish, Stay simple.</p>"
      )

(with-eval-after-load 'org-static-blog
  (defun update-post-list (&rest _)
    "Update the post list in post-list.json."
    (let* ((post-list (mapcar 'org-static-blog-get-post-url
                              (org-static-blog-get-post-filenames)))
           (json-encoding-pretty-print t)
           (json-data (json-encode post-list)))
      (with-temp-file (concat org-static-blog-publish-directory "assets/post-list.json")
        (insert json-data))
      (message "Updated post-list.json")))

  (advice-add 'org-static-blog-publish :after #'update-post-list))










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
