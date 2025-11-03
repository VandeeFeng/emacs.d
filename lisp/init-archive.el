;;; init-archive.el --- archive packages and configs -*- lexical-binding: t -*-
;; Author: Vandee
;; Created: 2025-09-19
;; Keywords:
;;; Commentary:
;;; Code:

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


;;-------------------------------------------------------------------------------
;;
;; org-roam
;;
;;-------------------------------------------------------------------------------
;; 转为使用 denote
;;(setq org-roam-dailies-directory "~/Vandee/pkm/Journals/")
;;(setq org-export-with-toc nil) ;;禁止生成toc
;; (use-package org-roam
;;   :defer t
;;   ;;:demand t  ;; Ensure org-roam is loaded by default 如果没有这个后面的zotero链接函数会不起作用，还在解决.这样会导致Emacs在第一次开机启动的时候很慢
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   ;; (org-roam-dailies-capture-templates
;;   ;;  '(("d" "daily" plain "* %<%Y-%m-%d>\n** TODO\n- \n** Inbox\n- %?"
;;   ;;     :if-new (file+head "%<%Y>/%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n"))))
;;   (org-roam-directory "~/Vandee/Areas/pkm/roam/")
;;   (org-id-locations-file "~/Vandee/Areas/pkm/roam/.orgids")
;;   (org-roam-capture-templates
;;    `(("n" "note" plain "%?"
;;       :if-new (file+head "${title}.org"
;;                          "#+TITLE: ${title}\n#+UID: %<%Y%m%d%H%M%S>\n#+FILETAGS: \n#+TYPE: Article \n#+SOURCE:  %^{source}\n#+DATE: %<%Y-%m-%d>\n")
;;       :unnarrowed t))
;;    )
;;   (org-roam-completion-everywhere t)
;;   :bind (
;;          ;;("C-c n l" . org-roam-buffer-toggle)
;;          ;;("C-c n f" . org-roam-node-find)
;;          ;;("C-c n i" . org-roam-node-insert)
;;          ;;("C-c n I" . org-roam-node-insert-immediate)
;;          ;;("C-c n c" . org-roam-capture)
;;          ;;("C-c n j" . org-roam-dailies-capture-today)
;;          ;;("C-c n n" . my/org-roam-find-notes)
;;          ;;("C-c n t" . my/org-roam-capture-task)
;;          ;;("C-c n b" . my/org-roam-capture-inbox)
;;          :map org-mode-map
;;          ("C-M-i" . completion-at-point)
;;          :map org-roam-dailies-map
;;          ("Y" . org-roam-dailies-capture-yesterday)
;;          ("T" . org-roam-dailies-capture-tomorrow))
;;   :bind-keymap
;;   ("C-c n d" . org-roam-dailies-map)
;;   :config
;;   (require 'org-roam-dailies) ;; Ensure the keymap is available
;;   (org-roam-db-autosync-mode)
;;   (require 'org-roam-protocol)

;;   )

;; (defun my/org-roam-node-has-tag (node tag)
;;   "Filter function to check if the given NODE has the specified TAG."
;;   (member tag (org-roam-node-tags node)))

;; (defun my/org-roam-node-find-by-tag ()
;;   "Find and open an Org-roam node based on a specified tag."
;;   (interactive)
;;   (let ((tag (read-string "Enter tag: ")))
;;     (org-roam-node-find nil nil (lambda (node) (my/org-roam-node-has-tag node tag)))))
;; (defun org-roam-node-insert-immediate (arg &rest args)
;;   (interactive "P")
;;   (let ((args (push arg args))
;;         (org-roam-capture-templates (list (append (car org-roam-capture-templates)
;;                                                   '(:immediate-finish t)))))
;;     (apply #'org-roam-node-insert args)))

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
;; (add-to-list 'display-buffer-alist
;;              '("\\*org-roam\\*"
;;                (display-buffer-in-side-window)
;;                (side . right)
;;                (slot . 0)
;;                (window-width . 0.33)
;;                (window-parameters . ((no-other-window . t)
;;                                      (no-delete-other-windows . t)))))

;; 智能注释，太蠢了，赶不上 Emacs 原生的。。
;; (defun my/comment-or-uncomment-region-codes ()
;;   "根据当前的主模式选择合适的注释符号来注释/取消注释选定区域"
;;   (interactive)
;;   (let* ((mode-comment-pairs '((emacs-lisp-mode . ";;")
;;                                (lisp-mode . ";;")
;;                                (scheme-mode . ";;")
;;                                (python-mode . "#")
;;                                (ruby-mode . "#")
;;                                (c-mode . "//")
;;                                (c++-mode . "//")
;;                                (java-mode . "//")
;;                                (js-mode . "//")
;;                                (js2-mode . "//")
;;                                (typescript-mode . "//")
;;                                (sh-mode . "#")
;;                                (shell-mode . "#")
;;                                (perl-mode . "#")
;;                                (php-mode . "//")
;;                                (css-mode . "/*")
;;                                (scss-mode . "//")
;;                                (sass-mode . "//")
;;                                (html-mode . "<!--")))
;;          (comment-str (or (cdr (assoc major-mode mode-comment-pairs)) ";;"))
;;          ;; 获取区域
;;          (start (if (region-active-p)
;;                     (region-beginning)
;;                   (line-beginning-position)))
;;          (end (if (region-active-p)
;;                   (region-end)
;;                 (line-end-position))))
;;     ;; 确保处理完整的行
;;     (save-excursion
;;       (goto-char start)
;;       (setq start (line-beginning-position))
;;       (goto-char end)
;;       (unless (bolp)                    ; 如果不在行首，移到下一行
;;         (forward-line 1))
;;       (setq end (point)))

;;     ;; 检查是否所有非空行都已注释
;;     (save-excursion
;;       (goto-char start)
;;       (let ((all-commented t)
;;             (any-uncommented nil))
;;         (while (and (< (point) end)
;;                     (or all-commented any-uncommented))
;;           (beginning-of-line)
;;           (unless (looking-at "^[ \t]*$") ; 跳过空行
;;             (if (looking-at (concat "^[ \t]*" (regexp-quote comment-str)))
;;                 (setq any-uncommented nil)
;;               (setq all-commented nil
;;                     any-uncommented t)))
;;           (forward-line 1))

;;         ;; 根据检查结果决定注释或取消注释
;;         (goto-char start)
;;         (if all-commented
;;             ;; 取消注释
;;             (while (< (point) end)
;;               (beginning-of-line)
;;               (when (re-search-forward
;;                      (concat "^[ \t]*" (regexp-quote comment-str) "[ \t]?")
;;                      (line-end-position) t)
;;                 (replace-match ""))
;;               (forward-line 1))
;;           ;; 添加注释
;;           (while (< (point) end)
;;             (beginning-of-line)
;;             (unless (looking-at "^[ \t]*$") ; 跳过空行
;;               (unless (looking-at (concat "^[ \t]*" (regexp-quote comment-str)))
;;                 (skip-chars-forward " \t")
;;                 (insert comment-str " ")))
;;             (forward-line 1)))))

;;     ;; 重新缩进区域
;;     (indent-region start end)))


;; (defun my-copy-buffer-file-name (event &optional bufName)
;;   "Copy buffer file name to kill ring.
;; If no file is associated with buffer just get buffer name.
;; "
;;   (interactive "eP")
;;   (save-selected-window
;;     (message "bufName: %S" bufName)
;;     (select-window (posn-window (event-start event)))
;;     (let ((name (or (unless bufName (buffer-file-name)) (buffer-name))))
;;       (message "Saved file name \"%s\" in killring." name)
;;       (kill-new name)
;;       name)))
;; (define-key mode-line-buffer-identification-keymap [mode-line mouse-2] 'copy-buffer-file-name)
;; (define-key mode-line-buffer-identification-keymap [mode-line S-mouse-2] '(lambda (e) (interactive "e") (copy-buffer-file-name e 't)))
;;


;; 自定义搜索
;; (defun my-build-or-regexp-by-keywords (keywords)
;;   "构建or语法的正则"
;;   (let (wordlist tmp regexp)
;;     (setq wordlist (split-string keywords " "))
;;     (dolist (word wordlist)
;;       (setq tmp (format "(%s)" word))
;;       (if regexp (setq regexp (concat regexp "|")))
;;       (setq regexp (concat regexp tmp)))
;;     regexp
;;     ))

;; (defun my-build-and-regexp-by-keywords (keywords)
;;   "构建and语法的正则"
;;   (let (reg wlist fullreg reglist)
;;     (setq wlist (split-string keywords " "))
;;     (dolist (w1 wlist)
;;       (setq reg w1)
;;       (dolist (w2 wlist)
;;         (unless (string-equal w1 w2)
;;           (setq reg (format "%s.*%s" reg w2))))
;;       (setq reg (format "(%s)" reg))
;;       (add-to-list 'reglist reg)
;;       )
;;     ;; 还要反过来一次
;;     (dolist (w1 wlist)
;;       (setq reg w1)
;;       (dolist (w2 (reverse wlist))
;;         (unless (string-equal w1 w2)
;;           (setq reg (format "%s.*%s" reg w2))))
;;       (setq reg (format "(%s)" reg))
;;       (add-to-list 'reglist reg)
;;       )

;;     (dolist (r reglist)
;;       (if fullreg (setq fullreg (concat fullreg "|")))
;;       (setq fullreg (concat fullreg r)))

;;     fullreg
;;     ))

;; (defun my-search-or-by-rg ()
;;   "以空格分割关键词，以or条件搜索多个关键词的内容
;;   如果要搜索tag，可以输入`:tag1 :tag2 :tag3'
;;   "
;;   (interactive)
;;   (let* ((keywords (read-string "Or Search(rg): "))
;;          (regexp (eye--build-or-regexp-by-keywords keywords)))
;;     (message "search regexp:%s" regexp)
;;     (color-rg-search-input regexp)
;;     ))


;; (defun my-search-and-by-rg ()
;;   "以空格分割关键词，以and条件搜索同时包含多个关键词的内容
;;   如果要搜索tag，可以输入`:tag1 :tag2 :tag3'
;;   "
;;   (interactive)
;;   (let* ((keywords (read-string "And Search(rg): "))
;;          (regexp (eye--build-and-regexp-by-keywords keywords)))
;;     (message "search regexp:%s" regexp)
;;     (color-rg-search-input regexp)
;;     ))

;; 隐藏 macos 里 Emacs 的 menu bar https://lmno.lol/alvaro/toggle-macos-menu-bar-from-you-know-where

(defun dwim-shell-commands-macos-toggle-menu-bar-autohide ()
  "Toggle macOS menu bar auto-hide."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Toggle menu bar auto-hide."
   "current_status=$(osascript -e 'tell application \"System Events\" to get autohide menu bar of dock preferences')

if [ \"$current_status\" = \"true\" ]; then
    osascript -e 'tell application \"System Events\" to set autohide menu bar of dock preferences to false'
    echo \"Auto-hide disabled.\"
else
    osascript -e 'tell application \"System Events\" to set autohide menu bar of dock preferences to true'
    echo \"Auto-hide enabled.\"
fi"
   :utils "osascript"
   :silent-success t))

;;; init-archive.el ends here
