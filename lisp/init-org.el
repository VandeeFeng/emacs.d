;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;;
;;;

;;---------------------------------------------------------------------
;;
;; global
;;
;;---------------------------------------------------------------------

;; org-mode open link
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "RET") 'org-open-at-point)))

;;-------------------------------------------------------------------------------
;;
;; denote
;;
;;-------------------------------------------------------------------------------
;; https://protesilaos.com/emacs/denote#h:58c4746b-b0d8-4896-9d88-a99b1d487231
;; https://github.com/protesilaos/denote
;; https://www.youtube.com/watch?v=mLzFJcLpDFI
(use-package denote
  :ensure t
  :defer t
  :hook
  (;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/vandee/Areas/pkm/denotes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("Thinking" "Philosophy" "Hacking" "Coding"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (setq denote-org-front-matter
        "#+TITLE:      %s
#+DATE:       %s
#+FILETAGS:   %s
#+IDENTIFIER: %s
\n")

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

;;;###autoload
(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'prot-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines))

;;; denote ends

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

;; 代码块格式设置
;; (setq org-src-tab-acts-natively t)               ;; 在代码块中使用原生的tab行为
;; (setq org-adapt-indentation nil)                 ;; 禁止自动缩进
;; (setq org-src-ask-before-returning-to-edit-buffer nil)  ;; 编辑代码块时不询问
;; (setq org-html-htmlize-output-type 'nil) ;禁用 Emacs 的语法高亮渲染
;; 代码块缩进设置
(setq org-src-preserve-indentation t)            ;; 保持原始缩进
(setq org-edit-src-content-indentation 0)        ;; 设置代码块的基础缩进为0
(setq org-export-babel-evaluate nil)

;; 折叠标题层级
;; https://emacs-china.org/t/org-startup-show2levels/16499
;; 可单独配置 #+STARTUP: show2levels
;;(setq org-startup-folded 'show2levels)

;; https://github.com/yibie/org-include-inline
;; (require 'org-include-inline)
;; ;; Auto-enable in all Org buffers
;; (setq org-include-inline-auto-enable-in-org-mode t)

;; ;; Customize maximum lines to display
;; (setq org-include-inline-max-lines-to-display 100)

;; Customize the display face
;; (set-face-attribute 'org-include-inline-face nil
;;                     :background "black"
;;                     :foreground "white")

;;-------------------------------------------------------------------------------
;;
;; org-roam
;;
;;-------------------------------------------------------------------------------
;; 转为使用 denote
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
  (org-roam-directory "~/Vandee/Areas/pkm/roam/")
  (org-id-locations-file "~/Vandee/Areas/pkm/roam/.orgids")
  (org-roam-capture-templates
   `(("n" "note" plain "%?"
      :if-new (file+head "${title}.org"
                         "#+TITLE: ${title}\n#+UID: %<%Y%m%d%H%M%S>\n#+FILETAGS: \n#+TYPE: Article \n#+SOURCE:  %^{source}\n#+DATE: %<%Y-%m-%d>\n")
      :unnarrowed t))
   )
  (org-roam-completion-everywhere t)
  :bind (
         ;;("C-c n l" . org-roam-buffer-toggle)
         ;;("C-c n f" . org-roam-node-find)
         ;;("C-c n i" . org-roam-node-insert)
         ;;("C-c n I" . org-roam-node-insert-immediate)
         ;;("C-c n c" . org-roam-capture)
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
                               "......" "-----------------------------------------------------"))

  ;; 设置TODO状态
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
  :defer 0.1
  ;; (server-start)
  ;; (require 'org-protocol)
  :init
  (setq org-modules-loaded t)
  :config
  (setq org-return-follows-link t) ;;开启 return 打开链接
  (global-set-key (kbd "s-<return>") 'org-return)
  (setq org-tags-column 0) ; 如果你不希望标签固定在某一列，可以将 org-tags-column 设置为 0，这样标签会紧随标题，而不会自动对齐到特定的列。默认情况下，org-tags-column 值为 -77，即在右侧边距对齐。如果这个值较大，标签会向右偏移
  ;; 默认开启缩进
  (setq org-startup-indented t)
  ;; (add-hook 'org-mode-hook #'org-indent-mode)
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
  (setq org-agenda-files '("~/Vandee/Areas/pkm/org/Clips.org" "~/Vandee/Areas/pkm/org/Projects.org" "~/vandee/Areas/pkm/org/Tasks.org"))
  ;; (setq org-agenda-include-diary t)
  ;; (setq org-agenda-diary-file "~/Vandee/pkm/org/Journal.org")
  (setq org-directory "~/Vandee/Areas/pkm/org/")
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
               '("j" "Journal" entry (file+datetree "~/Vandee/Areas/pkm/org/Journal.org")
                 "* Inbox\n- %?"))

  (add-hook 'org-capture-after-finalize-hook
            (lambda ()
              (when (string= (org-capture-get :key) "j")
                (find-file "~/Vandee/Areas/pkm/org/Journal.org"))))
  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry (file+datetree "~/Vandee/Areas/pkm/org/Inbox.org")
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
               '("tt" "Task" entry (file+headline "~/Vandee/Areas/pkm/org/Tasks.org" "Tasks")
                 "* TODO %^{任务名}\n%T\n%a\n"
                 :prepend t))
  (add-to-list 'org-capture-templates
               '("tp" "Project" entry
                 (file+headline "~/Vandee/Areas/pkm/org/Projects.org" "Projects")
                 "* TODO %^{任务名}\n%T"
                 :prepend t))
  ;; colections
  (add-to-list 'org-capture-templates '("c" "Collections"))
  (add-to-list 'org-capture-templates
               '("cw" "Web Collections" item
                 (file+headline "~/Vandee/Areas/pkm/org/Websites.org" "实用")
                 "Intro: %^{Intro}\n\nSource: %^{Source}\n%?"
                 :prepend t))
  (add-to-list 'org-capture-templates
               '("ct" "Tool Collections" item
                 (file+headline "~/Vandee/Areas/pkm/org/Tools.org" "实用")
                 "Intro: %^{Intro}\n\nSource: %^{Source}\n%?"
                 :prepend t))
  (add-to-list 'org-capture-templates
               '("cc" "Clip Collections" entry
                 (file+headline "~/Vandee/Areas/pkm/org/Clips.org" "Clips")
                 "* %^{heading} %^g\n%T\n\nSource: %^{source}\n\n%?"
                 :prepend t))
  (add-to-list 'org-capture-templates
               '("cC" "Code Collections" entry
                 (file+headline "~/Vandee/Areas/pkm/org/Codes.org" "Codes")
                 "* %U - %^{Intro} %^G\n\nSource: %^{source}\n%?"
                 :prepend t))




  (setq org-tag-alist '((:startgroup . nil)
                        ("Coding" . c)
                        (:grouptags . nil)
                        ("Python" . p)
                        ("JavaScript" . j)
                        ("Rust" . ru)
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



;;---------------------------------------------------------------------
;;
;; org 美化
;;
;;---------------------------------------------------------------------

;; (defun my/block-highlighting ()
;; "Setup block region highlighting for Org mode."
;; ;; Define the face for block backgrounds
;; ;; Define the face for block backgrounds
;; (defface org-block-region-background
;; '((t (:extend t :background "#252525")))
;; "Face for the entire block region including begin/end markers.")

;; (defvar block-region-overlay-pool nil
;; "List of overlays for block region highlighting.")

;; (defun clear-block-region-overlays ()
;; (while block-region-overlay-pool
;; (delete-overlay (pop block-region-overlay-pool))))

;; (defun highlight-block-regions ()
;; (interactive)
;; (save-excursion
;; (clear-block-region-overlays)
;; (goto-char (point-min))
;; (while (re-search-forward "^[ \t]*#\\+begin_\\(src\\|quote\\|example\\)" nil t)
;; (let* ((begin-line-start (line-beginning-position))
;; (block-type (match-string 1))
;; (end-regexp (concat "^[ \t]*#\\+end_" block-type)))
;; ;; 使用 org-fold-folded-p 检查是否折叠
;; (unless (org-fold-folded-p begin-line-start)
;; (when (re-search-forward end-regexp nil t)
;; (let* ((end-line-end (line-end-position))
;; (ov (make-overlay begin-line-start (1+ end-line-end))))
;; (overlay-put ov 'face 'org-block-region-background)
;; (overlay-put ov 'evaporate t)
;; (overlay-put ov 'priority -1)
;; (push ov block-region-overlay-pool))))))))
;; ;; 创建次要模式
;; (define-minor-mode block-region-highlight-mode
;; "Toggle background highlighting for entire block regions."
;; :lighter " BRH"
;; (if block-region-highlight-mode
;; (progn
;; (highlight-block-regions)
;; (add-hook 'post-command-hook #'highlight-block-regions nil t)
;; ;; 添加对折叠状态变化的监听
;; (add-hook 'org-fold-core-style-changed-functions #'highlight-block-regions nil t))
;; (clear-block-region-overlays)
;; (remove-hook 'post-command-hook #'highlight-block-regions t)
;; (remove-hook 'org-fold-core-style-changed-functions #'highlight-block-regions t)))
;; ;; 为 org-mode 自动启用
;; (add-hook 'org-mode-hook #'block-region-highlight-mode))

;; 开启 block-highlighting
;; (my/block-highlighting)
;; (block-region-highlight-mode)


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


;; (defun my-paste-with-space-after-url ()
;; "Paste and add a space after a URL if present, and between Chinese and English characters."
;; (interactive)
;; (let ((orig-point (point)))
;; (evil-paste-from-register ?*)  ; Use * register for system clipboard on Mac
;; (let ((pasted-text (buffer-substring-no-properties orig-point (point))))
;; (when (string-match "\\(https?://\\)" pasted-text)
;; (insert " "))
;; ;; Apply Chinese-English spacing to the pasted text
;; (save-excursion
;; (goto-char orig-point)
;; (while (< (point) (point-max))
;; (add-space-between-chinese-and-english)
;; (forward-char))))))

;; (global-set-key (kbd "C-v") 'my-paste-with-space-after-url)






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



;;----------------------------------------------
;; org-publish
;; ----------------------------------------------

(defun my/org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (or (org-element-property :language src-block) "nil")) ; 使用 "nil" 作为默认语言
           (code (org-html-format-code src-block info))
           (label (let ((lbl (org-element-property :name src-block)))
                    (if lbl (org-html--anchor lbl nil info) ""))))
      (format "<div class=\"org-src-container\">\n%s%s\n</div>"
              (if (not (string= label ""))
                  (format "<label class=\"org-src-name\">%s</label>\n" label)
                "")
              (format "<pre class=\"src src-%s\">%s</pre>"
                      lang
                      (replace-regexp-in-string "[ \t\n]*$" "" code))))))

(defun my/org-html-fontify-code (code lang)
  "Fontify CODE block using LANG mode.
This is a modified version that prevents sh-mode indentation."
  (with-temp-buffer
    (insert code)
    (let ((inhibit-message t))  ; 抑制所有消息
      (delay-mode-hooks        ; 延迟模式钩子
        (let ((major-mode nil) ; 清除主模式
              (sh-basic-offset 0)
              (sh-indentation 0)
              (indent-line-function 'ignore)
              (before-change-functions nil)
              (after-change-functions nil)
              (org-src-preserve-indentation t))
          (cond
           ;; 对于 shell 脚本特殊处理
           ((member lang '("sh" "bash" "shell"))
            (progn
              (fundamental-mode)
              (font-lock-mode 1)))
           ;; 对于没有指定语言的代码块
           ((or (null lang) (string= lang "nil") (string= lang ""))
            (progn
              (fundamental-mode)
              (font-lock-mode 1)))
           ;; 其他语言正常处理
           (t
            (let ((mode-name (intern (concat lang "-mode"))))
              (if (fboundp mode-name)
                  (funcall mode-name)
                ;; 如果找不到对应的模式，使用 fundamental-mode
                (fundamental-mode)
                (font-lock-mode 1))))))))
    (font-lock-ensure)
    (buffer-string)))

;; 替换原有的代码高亮函数和代码块处理函数
(with-eval-after-load 'org-static-blog
  (advice-add 'org-html-fontify-code :override #'my/org-html-fontify-code)
  (advice-add 'org-html-src-block :override #'my/org-html-src-block))

;; 确保 sh-mode 不会自动设置缩进
(with-eval-after-load 'sh-script
  (setq sh-basic-offset 0)
  (setq sh-indentation 0)
  (advice-add 'sh-set-indent :override #'ignore))

;; 修改 org-static-blog 的发布过程
(defun my/org-static-blog-publish-file-advice (orig-fun &rest args)
  "Advice to control indentation during file publishing."
  (let ((before-save-hook nil)         ; 清空保存钩子
        (after-save-hook nil)          ; 清空保存后钩子
        (write-file-functions nil)     ; 清空写文件函数
        (indent-line-function #'ignore) ; 禁用行缩进
        (org-src-preserve-indentation t)
        (org-edit-src-content-indentation 0)
        ;; shell 相关设置
        (sh-basic-offset 0)
        (sh-indentation 0)
        ;; org 导出设置
        (org-html-indent nil)
        ;; 其他缩进控制
        (indent-tabs-mode nil)
        (tab-width 0))
    (apply orig-fun args)))

(with-eval-after-load 'org-static-blog
  (advice-add 'org-static-blog-publish-file :around #'my/org-static-blog-publish-file-advice))


;; https://taxodium.ink/org-publish-blog.html ,开启内容折叠
;; (setq org-html-html5-fancy t)
;; (setq org-html-doctype "html5")

;; ;;----------------------------------------------
;; org-blog
;; ----------------------------------------------
;; https://github.com/bastibe/org-static-blog/blob/master/org-static-blog.el
;; org-static-blog config

(setq org-static-blog-publish-title "Vandee's Blog")
(setq org-static-blog-publish-url "https://www.vandee.art/blog/")
(setq org-static-blog-publish-directory "~/vandee/Areas/org-blog/blog")
(setq org-static-blog-posts-directory "~/vandee/Areas/org-blog/posts/")
(setq org-static-blog-drafts-directory "~/vandee/Areas/org-blog/drafts/")
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
       <meta name=\"referrer\" content=\"origin-when-cross-origin\">
       <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">

       <link rel=\"stylesheet\" href=\"assets/css/blog-style.css\" type=\"text/css\"/>
       <link rel=\"stylesheet\"
             href=\"https://lf26-cdn-tos.bytecdntp.com/cdn/expire-1-M/font-awesome/6.0.0/css/all.min.css\"/>
       <link rel=\"stylesheet\"
             href=\"https://testingcf.jsdelivr.net/npm/@fancyapps/ui@4.0.12/dist/fancybox.css\"/>
       <link rel=\"icon\" type=\"image/x-icon\" href=\"/favicon.ico\"/>

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
       <script src=\"assets/js/shiba.js\" defer></script>
       <script src=\"assets/js/search.js\" defer></script>")


;; Preamble for every page (e.g., navigation)
(setq org-static-blog-page-preamble
      (format "
      <header>
      <h1><a href=\"%s\">Vandee's Blog</a></h1>
      <img class=\"shiba\" width=\"100px\" src=\"assets/shiba_gif/shiba_idle_8fps.gif\">
      <nav>
      <a href=\"%s\">Home</a>
      <a href=\"https://x404.xyz/wiki\">Wiki</a>
      <a href=\"https://x404.xyz/photo\">Photo</a>
      <a href=\"archive.html\">Archive</a>
      <a href=\"tags.html\">Tags</a>
      <div id=\"search-container\">
        <input type=\"text\" id=\"search-input\" placeholder=\"e.g. Emacs PKM...\">
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
      (with-temp-file (concat org-static-blog-publish-directory "/assets/post-list.json")
        (insert json-data))
      (message "Updated post-list.json")))

  (advice-add 'org-static-blog-publish :after #'update-post-list))

(provide 'init-org)
;;; init-org.el ends here
