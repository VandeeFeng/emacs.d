;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;;
;;;

;;-------------------------------------------------------------------------------
;;
;; global
;;
;;-------------------------------------------------------------------------------


;; org-remoteimg
(require 'org-remoteimg)

(setq url-cache-directory "~/.emacs.d/cache/url"
      org-display-remote-inline-images 'skip) ;; Default to disabling the plugin

;; Toggle function for enabling or disabling org-remoteimg
(defun toggle-org-remoteimg ()
  "Toggle the `org-remoteimg` package based on its current state."
  (interactive)
  (if (eq org-display-remote-inline-images 'skip)
      (progn
        (require 'org-remoteimg) ;; Ensure the plugin is loaded
        (setq org-display-remote-inline-images 'cache) ;; Enable with caching
        (message "org-remoteimg enabled."))
    (setq org-display-remote-inline-images 'skip) ;; Disable plugin
    (message "org-remoteimg disabled.")))




;; org-babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((emacs-lisp . t)
      (C .t)
      (julia . t)
      (python . t)
      (js . t)
      (shell . t)
      (rust . t)
      (jupyter . t)))))
;; ob-X
(use-package ob-rust
  :ensure t)


;; 折叠标题层级
;; https://emacs-china.org/t/org-startup-show2levels/16499
;; 可单独配置 #+STARTUP: show2levels
;;(setq org-startup-folded 'show2levels)

;;(setq org-adapt-indentation t)

;;-------------------------------------------------------------------------------
;;
;; org-roam
;;
;;-------------------------------------------------------------------------------
;;(setq org-roam-dailies-directory "~/Vandee/pkm/Journals/")
;;(setq org-export-with-toc nil) ;;禁止生成toc
(use-package org-roam
  :defer t
  ;;:demand t  ;; Ensure org-roam is loaded by default 如果没有这个后面的zotero链接函数会不起作用，还在解决.这样会导致Emacs在第一次开机启动的时候很慢
  :init
  (setq org-roam-v2-ack t)
  :custom
  ;; (org-roam-dailies-capture-templates
  ;;  '(("d" "daily" plain "* %<%Y-%m-%d>\n** TODO\n- \n** Inbox\n- %?"
  ;;     :if-new (file+head "%<%Y>/%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n"))))
  (org-roam-directory "~/Vandee/pkm/roam/")
  (org-id-locations-file "~/Vandee/pkm/roam/.orgids")
  (org-roam-capture-templates
   `(("n" "note" plain "%?"
      :if-new (file+head "${title}.org"
                         "#+TITLE: ${title}\n#+UID: %<%Y%m%d%H%M%S>\n#+FILETAGS: \n#+TYPE: Article \n#+SOURCE:  %^{source}\n#+DATE: %<%Y-%m-%d>\n")
      :unnarrowed t))
   )
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n c" . org-roam-capture)
         ;;("C-c n j" . org-roam-dailies-capture-today)
         ;;("C-c n n" . my/org-roam-find-notes)
         ;;("C-c n t" . my/org-roam-capture-task)
         ;;("C-c n b" . my/org-roam-capture-inbox)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)

  )

(defun my/org-roam-node-has-tag (node tag)
  "Filter function to check if the given NODE has the specified TAG."
  (member tag (org-roam-node-tags node)))

(defun my/org-roam-node-find-by-tag ()
  "Find and open an Org-roam node based on a specified tag."
  (interactive)
  (let ((tag (read-string "Enter tag: ")))
    (org-roam-node-find nil nil (lambda (node) (my/org-roam-node-has-tag node tag)))))
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; (after! org-roam
;;   ;; org-roam网页摘录
;;   ;; https://www.zmonster.me/2020/06/27/org-roam-introduction.html#orgec47e48
;;   (add-to-list 'org-roam-capture-ref-templates
;;                '("a" "Annotation" plain (function org-roam-capture--get-point)
;;                  "%U ${body}\n"
;;                  :file-name "${slug}"
;;                  :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
;;                  :immediate-finish t
;;                  :unnarrowed t)))



;; (defun my/org-roam-filter-by-tag (tag-name)
;;   (lambda (node)
;;     (member tag-name (org-roam-node-tags node))))

;; (defun my/org-roam-list-notes-by-tag (tag-name)
;;   (interactive)
;;   (mapcar #'org-roam-node-file
;;           (seq-filter
;;            (my/org-roam-filter-by-tag tag-name)
;;            (org-roam-node-list))))




;; (defun my/org-roam-capture-inbox ()
;;   (interactive)
;;   (org-roam-capture- :node (org-roam-node-create)
;;                      :templates '(("i" "inbox" plain "* %?"
;;                                    :if-new (file+head "Inbox.org" "#+TITLE: Inbox\n")))))



;; For users that prefer using a side-window for the org-roam buffer, the following example configuration should provide a good starting point:对于喜欢使用侧窗口作为 org-roam 缓冲区的用户，以下示例配置应该提供一个很好的起点：
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))


;;---------------------------------------------
;;org-agenda
;;--------------------------------------------
;;
;;设置agenda时间线间隔
(with-eval-after-load 'org
  (setq org-agenda-time-grid '((daily today)
                               (800 1000 1200 1400 1600 1800 2000)
                               "......" "-----------------------------------------------------")))

;; 设置TODO状态
(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d@)")))
  (setq org-log-done 'time) ;; 每次当你将一个项从 TODO (not-done) 状态变成任意的 DONE 状态时，那么，它就会自动在标题的下面插入一行下面的内容：CLOSED: [timestamp]  https://emacsist.github.io/emacsist/orgmode/orgmode%E6%89%8B%E5%86%8C%E5%AD%A6%E4%B9%A0%E7%AC%94%E8%AE%B0.html#org6796967
  )

;;-------------------------------------------------------------------------------
;;
;; org
;;
;;-------------------------------------------------------------------------------
;;https://www.zmonster.me/2018/02/28/org-mode-capture.html
;;https://emacs-china.org/t/05-org-as/12092/6
;;一部分已经在config.el里设置，因为要在一开始加载目录,可以添加 (after! package) 又写回来了
;; ;;这样会把目录下包括子文件夹的文件都添加进去https://emacs-china.org/t/org-txt-agenda/13506/5
;; ;;(setq org-agenda-files (directory-files-recursively "~/Vandee/pkm/" "\\.org$"))

(use-package org
  :defer t
  ;; (server-start)
  ;; (require 'org-protocol)
  :config
  ;; 默认开启缩进
  (add-hook 'org-mode-hook #'org-indent-mode)
  ;; 完全禁用下标处理
  (setq org-export-with-sub-superscripts nil)
  (org-link-set-parameters "calibre" :follow
                           (lambda (cpath)
                             (browse-url
                              ;; 我们直接使用 cpath，因为它已经是完整的 Calibre 链接
                              (format "calibre:%s" cpath))))
  ;; (setq org-export-with-toc nil) ;;禁止生成toc
  (org-link-set-parameters "zotero" :follow
                           (lambda (zpath)
                             (browse-url
                              ;; we get the "zotero:"-less url, so we put it back.
                              (format "zotero:%s" zpath))))
  (setq org-agenda-files '("~/Vandee/pkm/org/Journal.org" "~/Vandee/pkm/org/Clips.org" "~/Vandee/pkm/org/Projects.org" "~/vandee/pkm/org/Tasks.org"))
  ;; (setq org-agenda-include-diary t)
  ;; (setq org-agenda-diary-file "~/Vandee/pkm/org/Journal.org")
  (setq org-directory "~/Vandee/pkm/org/")
  (global-set-key (kbd "C-c c") 'org-capture)
  ;;(setq org-default-notes-file "~/Vandee/pkm/inbox.org")
  (setq org-capture-templates nil)

  ;; (add-to-list 'org-capture-templates
  ;;              '("j" "Journal" entry (file+datetree  "~/Vandee/pkm/Journals/Journal.org")
  ;;                "* [[file:%<%Y>/%<%Y-%m-%d>.org][%<%Y-%m-%d>]] - %^{heading} %^g\n %?\n"))
  ;; (add-to-list 'org-capture-templates
  ;;              '("j" "Journal" entry (file+datetree "~/Vandee/pkm/org/Journal.org")
  ;;                "* TODOs\n* Inbox\n- %?"))
  (add-to-list 'org-capture-templates
               '("j" "Journal" entry (file+datetree "~/Vandee/pkm/org/Journal.org")
                 "* Inbox\n- %?"))

  (add-hook 'org-capture-after-finalize-hook
            (lambda ()
              (when (string= (org-capture-get :key) "j")
                (find-file "~/Vandee/pkm/org/Journal.org"))))
  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry (file+datetree "~/Vandee/pkm/org/Inbox.org")
                 "* %U - %^{heading} %^g\n %?\n"))

  (defun my-org-goto-last-todo-headline ()
    "Move point to the last headline in file matching \"* Notes\"."
    (end-of-buffer)
    (re-search-backward "\\* TODOs"))
  ;; task
  (add-to-list 'org-capture-templates '("t" "Tasks"))
  ;; (add-to-list 'org-capture-templates
  ;;              '("tt" "Task" entry (file+function "~/Vandee/pkm/org/Journal.org"
  ;;                                                 my-org-goto-last-todo-headline)
  ;;                "* TODO %i%? \n%T"))
  (add-to-list 'org-capture-templates
               '("tt" "Task" entry (file+headline "~/Vandee/pkm/org/Tasks.org" "Tasks")
                 "* TODO %^{任务名}\n%T\n%a\n"))
  (add-to-list 'org-capture-templates
               '("tp" "Project" entry (file+headline "~/Vandee/pkm/org/Projects.org" "Projects")
                 "* TODO %^{任务名}\n%T"))

  ;; colections
  (add-to-list 'org-capture-templates '("c" "Collections"))
  (add-to-list 'org-capture-templates
               '("cw" "Web Collections" item
                 (file+headline "~/Vandee/pkm/org/Websites.org" "实用")
                 "Intro: %^{Intro}\n\nSource: %^{Source}\n%?"))
  (add-to-list 'org-capture-templates
               '("ct" "Tool Collections" item
                 (file+headline "~/Vandee/pkm/org/Tools.org" "实用")
                 "Intro: %^{Intro}\n\nSource: %^{Source}\n%?"))
  (add-to-list 'org-capture-templates
               '("cc" "Clip Collections" entry
                 (file+headline "~/Vandee/pkm/org/Clips.org" "Clips")
                 "* %^{heading} %^g\n%T\n\nSource: %^{source}\n\n%?"))
  (add-to-list 'org-capture-templates
               '("cC" "Code Collections" entry
                 (file+headline "~/Vandee/pkm/org/Codes.org" "Codes")
                 "* %U - %^{Intro} %^G\n\nSource: %^{source}\n%?"))




  (setq org-tag-alist '((:startgroup . nil)
                        ("Coding" . c)
                        (:grouptags . nil)
                        ("Python" . p)
                        ("JavaScript" . j)
                        (:endgroup . nil)
                        ("LLM" . ?l) ("RAG" . ?r)
                        (:startgroup . nil)
                        ("Vandee" . v)
                        (:endgroup . nil)
                        ("Thoughts" . ?t) ("Quote" . ?q)))

  )
;; (add-to-list 'org-capture-templates
;;              '("m" "Memo" entry
;;                (file+headline "~/Vandee/pkm/org/memo.org" "Memo")
;;                "* %^{heading} %^g\n%T\nSource: %^{source}\n%?"))





;;https://emacs-china.org/t/org-mode-gtd-faq/196/16
;;很多时候我自己的tag都是唯一的，为了能够在不同的文件中使用同一个tag，或者说是自动选择和查看已经有的tag，我是这样设置的：
(setq-default org-complete-tags-always-offer-all-agenda-tags t)

;; 需要这个功能的 Org 笔记在 header 里加入下面一行即可（在笔记的前18行都可以）。 #+last_modified: [ ]

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local time-stamp-active t
                        time-stamp-line-limit 18
                        time-stamp-start "^#\\+last_modified: [ \t]*"
                        time-stamp-end "$"
                        time-stamp-format "\[%Y-%m-%d %a %H:%M:%S\]")
            (add-hook 'before-save-hook 'time-stamp nil 'local)))
;; 这个知识网络的可视化会显示在浏览器中，通过 websocket 与 Emacs 通信。
(with-eval-after-load 'org-roam
  (use-package websocket)
  (use-package org-roam-ui
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil)))

;;--------------------------------------------
;; 使用Company补全org block
;;--------------------------------------------
;; https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E4%BD%BF%E7%94%A8Company%E8%A1%A5%E5%85%A8org%20block.org
;; 需要安装 company-org-block
(require 'map)
(require 'org)
(require 'seq)

(defvar company-org-block-bol-p t "If t, detect completion when at
    begining of line, otherwise detect completion anywhere.")

(defvar company-org--regexp "<\\([^ ]*\\)")

(defun company-org-block (command &optional arg &rest ignored)
  "Complete org babel languages into source blocks."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-block))
    (prefix (when (derived-mode-p 'org-mode)
              (company-org-block--grab-symbol-cons)))
    (candidates (company-org-block--candidates arg))
    (post-completion
     (company-org-block--expand arg))))

(defun company-org-block--candidates (prefix)
  "Return a list of org babel languages matching PREFIX."
  (seq-filter (lambda (language)
                (string-prefix-p prefix language))
              ;; Flatten `org-babel-load-languages' and
              ;; `org-structure-template-alist', join, and sort.
              (seq-sort
               #'string-lessp
               (append
                (mapcar #'prin1-to-string
                        (map-keys org-babel-load-languages))
                (map-values org-structure-template-alist)))))

(defun company-org-block--template-p (template)
  (seq-contains (map-values org-structure-template-alist)
                template))

(defun company-org-block--expand (insertion)
  "Replace INSERTION with actual source block."
  (delete-region (point) (- (point) (1+ ;; Include "<" in length.
                                     (length insertion))))
  (if (company-org-block--template-p insertion)
      (company-org-block--wrap-point insertion
                                     ;; May be multiple words.
                                     ;; Take the first one.
                                     (nth 0 (split-string insertion)))
    (company-org-block--wrap-point (format "src %s" insertion)
                                   "src")))

(defun company-org-block--wrap-point (begin end)
  "Wrap point with block using BEGIN and END. For example:
    ,#+begin_BEGIN
     |
    ,#+end_END"
  (insert (format "#+begin_%s\n" begin))
  (insert (make-string org-edit-src-content-indentation ?\s))
  ;; Saving excursion restores point to location inside code block.
  (save-excursion
    (insert (format "\n#+end_%s" end))))

(defun company-org-block--grab-symbol-cons ()
  "Return cons with symbol and t whenever prefix of < is found.
    For example: \"<e\" -> (\"e\" . t)"
  (when (looking-back (if company-org-block-bol-p
                          (concat "^" company-org--regexp)
                        company-org--regexp)
                      (line-beginning-position))
    (cons (match-string-no-properties 1) t)))



;;-------------------------------------------------------------------------------
;;
;; org 美化
;;
;;-------------------------------------------------------------------------------
;; 设置标题大小

;; 改变 Org-mode 各个级别标题的大小，同时保留主题颜色和样式
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.5 :weight normal))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4 :weight normal))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3 :weight normal))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2 :weight normal))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1 :weight normal))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.05 :weight normal))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0 :weight normal))))
 '(org-level-8 ((t (:inherit outline-8 :height 1.0 :weight normal))))


 ;; 设置文档标题 (#+TITLE:)
 '(org-document-title ((t (:inherit default :weight bold
                                    :height 1.6    ; 文档标题字体大小
                                    :underline nil ; 添加下划线
                                    ))))           ; 标题颜色

 ;; 设置特殊关键字 (#+STARTUP: 等)
 '(org-meta-line ((t (:inherit font-lock-comment-face
                               :height 1.1 ; 关键字字体大小
                               ;; :slant italic     ; 斜体
                               ))))

 ;; 设置正文文字
 '(org-default ((t (:family "Fira Code Light"
                            :height 120
                            :weight normal))))
 )


;; 设置默认字体和大小
(set-face-attribute 'default nil
                    :family "Fira Code Light"
                    :height 130 ; 基础字体大小(pt)
                    :foreground "#e8eaed")
;; F5F5f5 , #d1d5db 原始灰色, 增加白色 #f1f2f4 70%，#edeef1 60% ,#e8eaed 50%
(setq org-hide-emphasis-markers t) ;; 设置行内make up，直接显示*粗体*，/斜体/，=高亮=，~代码~

;; 设置时间戳颜色

(set-face-attribute 'org-date nil
                    :foreground "#61AFEF"     ; 设置前景色
                    :background nil   ; 设置背景色
                    ;; :underline nil           ; 移除下划线
                    :weight 'normal)         ; 设置字重
;; 设置 org-tags 的颜色
(set-face-attribute 'org-tag nil
                    :foreground "#8B949E"     ; 淡灰色
                    :weight 'normal           ; 普通字重
                    :height 0.9               ; 字体大小为默认的90%
                    :inherit nil              ; 不继承其他face的属性
                    :slant 'normal)           ; 正常字体（非斜体）

;;-------------------------------------------------------------------------------
;;
;; org-protocol
;;
;;-------------------------------------------------------------------------------

;;(server-start)
;;(require 'org-protocol)
;; (setq org-protocol-protocol 'org-roam)
;; 盘古
;;https://github.com/coldnew/pangu-spacing
;; (use-package pangu-spacing
;;   :config
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))


;; https://emacs-china.org/t/org-mode/22313
;; 中文标记优化，不用零宽空格在 org-mode 中标记中文的办法
;; (font-lock-add-keywords 'org-mode
;;                         '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
;;                            (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
;;                           ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
;;                            (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
;;                         'append)

;; (with-eval-after-load 'ox
;;   (defun eli-strip-ws-maybe (text _backend _info)
;;     (let* ((text (replace-regexp-in-string
;;                   "\\(\\cc\\) *\n *\\(\\cc\\)"
;;                   "\\1\\2" text));; remove whitespace from line break
;;            ;; remove whitespace from `org-emphasis-alist'
;;            (text (replace-regexp-in-string "\\(\\cc\\) \\(.*?\\) \\(\\cc\\)"
;;                                            "\\1\\2\\3" text))
;;            ;; restore whitespace between English words and Chinese words
;;            (text (replace-regexp-in-string "\\(\\cc\\)\\(\\(?:<[^>]+>\\)?[a-z0-9A-Z-]+\\(?:<[^>]+>\\)?\\)\\(\\cc\\)"
;;                                            "\\1 \\2 \\3" text)))
;;       text))
;;   (add-to-list 'org-export-filter-paragraph-functions #'eli-strip-ws-maybe))
;;



;;https://emacs-china.org/t/orgmode/9740/11
;; 让中文也可以不加空格就使用行内格式

;; (setq org-emphasis-regexp-components '("-[:multibyte:][:space:]('\"{" "-[:multibyte:][:space:].,:!?;'\")}\\[" "[:space:]" "." 1))
;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
;; (org-element-update-syntax)
;;
;;

;; https://emacs-china.org/t/emacs/27274
;; https://remacs.fun/posts/%E5%A4%A7%E6%A8%A1%E5%9E%8B%E6%97%B6%E4%BB%A3%E6%88%91%E4%BB%AC%E6%80%8E%E4%B9%88%E7%8E%A9emacs1.-%E4%B8%AD%E8%8B%B1%E6%96%87%E8%BE%93%E5%85%A5%E6%97%B6%E7%9A%84%E7%A9%BA%E6%A0%BC/
(defun add-space-between-chinese-and-english ()
  "在中英文之间自动添加空格。"
  (let ((current-char (char-before))
        (prev-char (char-before (1- (point)))))
    (when (and current-char prev-char
               (or (and (is-chinese-character prev-char) (is-halfwidth-character current-char))
                   (and (is-halfwidth-character prev-char) (is-chinese-character current-char)))
               (not (eq prev-char ?\s))) ; 检查前一个字符不是空格
      (save-excursion
        (goto-char (1- (point)))
        (insert " ")))))

(defun is-chinese-character (char)
  "判断字符是否为中文字符。"
  (and char (or (and (>= char #x4e00) (<= char #x9fff))
                (and (>= char #x3400) (<= char #x4dbf))
                (and (>= char #x20000) (<= char #x2a6df))
                (and (>= char #x2a700) (<= char #x2b73f))
                (and (>= char #x2b740) (<= char #x2b81f))
                (and (>= char #x2b820) (<= char #x2ceaf)))))

(defun is-halfwidth-character (char)
  "判断字符是否为半角字符，包括英文字母、数字和标点符号。"
  (and char (or (and (>= char ?a) (<= char ?z))
                (and (>= char ?A) (<= char ?Z))
                (and (>= char ?0) (<= char ?9))
                )))

(defun delayed-add-space-between-chinese-and-english ()
  "延迟执行，在中英文之间自动添加空格。"
  (run-with-idle-timer 0 nil 'add-space-between-chinese-and-english))

(define-minor-mode auto-space-mode
  "在中英文之间自动添加空格的模式。"
  :lighter " Auto-Space"
  :global t
  (if auto-space-mode
      (add-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)
    (remove-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)))

(auto-space-mode t)


(defun my-paste-with-space-after-url ()
  "Paste and add a space after a URL if present, and between Chinese and English characters."
  (interactive)
  (let ((orig-point (point)))
    (evil-paste-from-register ?*)  ; Use * register for system clipboard on Mac
    (let ((pasted-text (buffer-substring-no-properties orig-point (point))))
      (when (string-match "\\(https?://\\)" pasted-text)
        (insert " "))
      ;; Apply Chinese-English spacing to the pasted text
      (save-excursion
        (goto-char orig-point)
        (while (< (point) (point-max))
          (add-space-between-chinese-and-english)
          (forward-char))))))

(global-set-key (kbd "C-v") 'my-paste-with-space-after-url)






;; (defun add-space-after-org-link-pasted ()
;;   "在粘贴 Org-mode 链接后自动添加一个英文空格。"
;;   (let ((beg (region-beginning))
;;         (end (region-end)))
;;     (when (save-excursion
;;             (goto-char beg)
;;             (and (re-search-forward "\\[\\[.*?\\]\\[.*?\\]\\]" end t)
;;                  (= (point) end))) ; 检查是否粘贴了链接
;;       (goto-char end)
;;       (insert " "))))

;; (define-minor-mode org-link-space-mode
;;   "在 Org-mode 链接后自动添加空格的模式。"
;;   :init-value nil
;;   :lighter " OrgLinkSpace"
;;   (if org-link-space-mode
;;       (add-hook 'yank-end-hook 'add-space-after-org-link-pasted)
;;     (remove-hook 'yank-end-hook 'add-space-after-org-link-pasted)))

;; ;; 在 Org-mode 中启用该功能
;; (add-hook 'org-mode-hook 'org-link-space-mode)


(provide 'init-org)
;;; init-org.el ends here
