;;; init-archive.el --- archive packages and configs -*- lexical-binding: t -*-
;; Author: Vandee
;; Created: 2025-09-19
;; Keywords:
;;; Commentary:
;;; Code:

;; https://github.com/emacs-evil/evil-surround
;; (use-package evil-surround
;;   :ensure t
;;   :after evil
;;   :config
;;   (global-evil-surround-mode 1))

;; https://github.com/hlissner/evil-multiedit
;; https://github.com/gabesoft/evil-mc
;; (use-package evil-multiedit
;;   :ensure t
;;   :defer t
;;   :after evil
;;   ;; :init
;;   ;; (setq evil-multiedit-dwim-motion-keys nil)
;;   :config
;;   (evil-define-key 'normal 'global
;;     (kbd "M-d")   #'evil-multiedit-match-symbol-and-next
;;     (kbd "M-D")   #'evil-multiedit-match-symbol-and-prev)
;;   (evil-define-key 'visual 'global
;;     "R"           #'evil-multiedit-match-all
;;     (kbd "M-d")   #'evil-multiedit-match-and-next
;;     (kbd "M-D")   #'evil-multiedit-match-and-prev)
;;   (evil-define-key '(visual normal) 'global
;;     (kbd "C-M-d") #'evil-multiedit-restore)
;;   (with-eval-after-load 'evil-mutliedit
;;     (evil-define-key 'multiedit 'global
;;       (kbd "M-d")   #'evil-multiedit-match-and-next
;;       (kbd "M-S-d") #'evil-multiedit-match-and-prev
;;       (kbd "M-RET")   #'evil-multiedit-toggle-or-restrict-region)
;;     (evil-define-key '(multiedit multiedit-insert) 'global
;;       (kbd "C-n")   #'evil-multiedit-next
;;       (kbd "C-p")   #'evil-multiedit-prev))
;;   )

;; org 标题链接
;; (defun my/org-get-current-headline-link ()
;;   "Get the org-mode link for the current headline, removing tags and preceding spaces."
;;   (interactive)
;;   (let* ((headline (org-get-heading))
;;          (headline-without-tags (replace-regexp-in-string " +:[a-zA-Z0-9_:]*$" "" headline)))
;;     (when headline-without-tags
;;       (let ((link (concat "[[file:" (buffer-file-name) "::*" headline-without-tags "][" headline-without-tags "]]")))
;;         (kill-new link)
;;         (message "Org-mode link for current headline (without tags) copied to clipboard.")))))

;; (defun my/compile-grep-rn (pattern)
;;   "Run `grep -irn` with the given PATTERN in the current directory."
;;   (interactive "sGrep pattern: ")
;;   (let ((command (format "grep -irn --color=always %s ."
;;                          (shell-quote-argument pattern))))
;;     (grep command)))

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

;; 没作用
;; (defun my-modify-syntax-for-chinese ()
;;   "Modify syntax table to treat each Chinese character as a word."
;;   (modify-syntax-entry ?\\ "w" (standard-syntax-table)) ; 避免反斜杠干扰
;;   (dolist (char (number-sequence #x4e00 #x9fff)) ; 汉字的 Unicode 范围
;;     (modify-syntax-entry char "w" (standard-syntax-table))))

;; (add-hook 'evil-local-mode-hook 'my-modify-syntax-for-chinese)

;; (defun my/org-html-src-block (src-block _contents info)
;;   "Transcode a SRC-BLOCK element from Org to HTML.
;; CONTENTS holds the contents of the item.  INFO is a plist holding
;; contextual information."
;;   (if (org-export-read-attribute :attr_html src-block :textarea)
;;       (org-html--textarea-block src-block)
;;     (let* ((lang (or (org-element-property :language src-block) "nil")) ; 使用 "nil" 作为默认语言
;;            (code (org-html-format-code src-block info))
;;            (label (let ((lbl (org-element-property :name src-block)))
;;                     (if lbl (org-html--anchor lbl nil info) ""))))
;;       (format "<div class=\"org-src-container\">\n%s%s\n</div>"
;;               (if (not (string= label ""))
;;                   (format "<label class=\"org-src-name\">%s</label>\n" label)
;;                 "")
;;               (format "<pre class=\"src src-%s\">%s</pre>"
;;                       lang
;;                       (replace-regexp-in-string "[ \t\n]*$" "" code))))))

;; (defun my/org-html-fontify-code (code lang)
;;   "Fontify CODE block using LANG mode.
;; This is a modified version that prevents sh-mode indentation."
;;   (with-temp-buffer
;;     (insert code)
;;     (let ((inhibit-message t))  ; 抑制所有消息
;;       (delay-mode-hooks        ; 延迟模式钩子
;;         (let ((major-mode nil) ; 清除主模式
;;               (sh-basic-offset 0)
;;               (sh-indentation 0)
;;               (indent-line-function 'ignore)
;;               (before-change-functions nil)
;;               (after-change-functions nil)
;;               (org-src-preserve-indentation t))
;;           (cond
;;            ;; 对于 shell 脚本特殊处理
;;            ((member lang '("sh" "bash" "shell"))
;;             (progn
;;               (fundamental-mode)
;;               (font-lock-mode 1)))
;;            ;; 对于没有指定语言的代码块
;;            ((or (null lang) (string= lang "nil") (string= lang ""))
;;             (progn
;;               (fundamental-mode)
;;               (font-lock-mode 1)))
;;            ;; 其他语言正常处理
;;            (t
;;             (let ((mode-name (intern (concat lang "-mode"))))
;;               (if (fboundp mode-name)
;;                   (funcall mode-name)
;;                 ;; 如果找不到对应的模式，使用 fundamental-mode
;;                 (fundamental-mode)
;;                 (font-lock-mode 1))))))))
;;     (font-lock-ensure)
;;     (buffer-string)))

;; ;; 确保 sh-mode 不会自动设置缩进
;; ;; 试了很多方法，还是得手动切换，今天突然又不行了
;; (with-eval-after-load 'sh-script
;;   ;; (message ">>> SUCCESS: org-static-blog has been loaded. Applying advice now. <<<")
;;   (setq sh-basic-offset 0)
;;   (setq sh-indentation 0)
;;   (advice-add 'sh-set-indent :override #'ignore))


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

;; (with-eval-after-load 'faces
;;   (set-face-attribute 'default nil :background "#181818")
;;   (set-cursor-color "orange") ; orange,gold1
;;   ;; 设置默认字体和大小
;;   (set-face-attribute 'default nil
;;                       :family "Source Code Pro" ;Source Code Pro ,FiraCode Nerd Font
;;                       :height 140               ; 基础字体大小(pt)
;;                       :weight 'light
;;                       :foreground "#d1d5db")
;;   https://www.jyshare.com/front-end/6214/#d1d5db
;;   #F5F5f5 ,#b2b5ba 15% ,#bcc0c5 10% ，#c7cad0 5% 加灰黑《 #d1d5db 原始灰色 》加白 #f6f7f8 80% #f1f2f4 70%，#edeef1 60% ,#e8eaed 50% ,#e3e6e9 40% ,#dfe2e6 30% #dadde2 20% ,#d6d9df 10% ,#d3d7dd 5%
;;   ;; 设置 mode-line
;;   (set-face-attribute 'mode-line nil :box nil)
;;   (set-face-attribute 'mode-line-inactive nil :box nil)

;;   ;; 设置注释的样式
;;   (set-face-attribute 'font-lock-comment-face nil :foreground "#787878")

;;   ;; 设置字符串的样式
;;   ;; (set-face-attribute 'font-lock-string-face nil :weight 'normal :foreground "#96D0FF")

;;   ;; 设置常量的样式
;;   (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
;;   ;; (set-face-attribute 'font-lock-constant-face nil :weight 'normal :foreground "#79C0FF")

;;   ;; 设置内置函数的样式
;;   (set-face-attribute 'font-lock-builtin-face nil :weight 'normal :foreground "#79C0FF")

;;   ;; 设置关键字的样式
;;   (set-face-attribute 'font-lock-keyword-face nil :weight 'normal :foreground "#FF7B72")

;;   ;; 设置函数名的样式
;;   (set-face-attribute 'font-lock-function-name-face nil :weight 'normal :foreground "#D2A8FF")

;;   ;; 设置变量名的样式
;;   (set-face-attribute 'font-lock-variable-name-face nil :weight 'normal :foreground "#FFA657")

;;   ;; 设置类型的样式
;;   (set-face-attribute 'font-lock-type-face nil :weight 'normal :foreground "#FF7B72") ;深红色

;;   ;; 设置文档字符串的样式
;;   (set-face-attribute 'font-lock-doc-face nil :weight 'normal :foreground "#787878")

;;   ;; 设置当前行号颜色
;;   (set-face-foreground 'line-number-current-line "gold1")

;;   )

;; (defun my-org-face-settings ()
;;   (interactive)
;;   (custom-set-faces
;;    ;; 设置默认字体和大小
;;    '(default ((t (:family "Source Code Pro" :height 140 :weight light :foreground "#d1d5db"))))
;;    ;; 代码块背景和边框
;;    ;; '(org-block-begin-line
;;    ;; ((t (:background "#343942" :foreground "#7F8490" :extend t))))
;;    '(org-level-1 ((t (:height 1.4 :weight normal))))
;;    '(org-level-2 ((t (:height 1.3 :weight normal))))
;;    '(org-level-3 ((t (:height 1.2 :weight normal))))
;;    '(org-level-4 ((t (:height 1.1 :weight normal))))
;;    '(org-level-5 ((t (:height 1.05 :weight normal))))
;;    '(org-level-6 ((t (:inherit outline-6 :height 1.05 :weight normal))))
;;    '(org-level-7 ((t (:inherit outline-7 :height 1.0 :weight normal))))
;;    '(org-level-8 ((t (:inherit outline-8 :height 1.0 :weight normal))))

;;    ;; 设置文档标题 (#+TITLE:)
;;    '(org-document-title ((t (:inherit default :weight bold
;;                                       :height 1.5 ; 文档标题字体大小
;;                                       :underline nil ; 添加下划线
;;                                       ))))           ; 标题颜色

;;    ;; 设置特殊关键字 (#+STARTUP: 等)
;;    '(org-meta-line ((t (:inherit font-lock-comment-face
;;                                  :height 1.1 ; 关键字字体大小
;;                                  ;; :slant italic     ; 斜体
;;                                  )))))

;;   )

;; ;; 在初始化时应用设置
;; (add-hook 'after-init-hook #'my-org-face-settings)
;; ;; 为新 frame 开启默认 org 美化设置
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (with-selected-frame frame
;;               (my-org-face-settings))))

;; (use-package conda
;;   :defer t
;;   :config
;;   (setq conda-anaconda-home "~/miniconda3/")
;;   (defun conda-setup ()
;;     (conda-env-initialize-interactive-shells)
;;     (conda-env-initialize-eshell)
;;     (conda-env-autoactivate-mode t)
;;     (setq conda-env-home-directory "~/miniconda3/"))

;;   (use-package eshell
;;     :hook (eshell-mode . conda-setup))

;;   (use-package python
;;     :hook (python-mode . conda-setup)))

;; ;;(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)) ;; 默认使用 python-ts-mode，需要安装 python 的 treesitter
;; ;;python black
;; (after! python
;;   :preface
;;   (defun +python-make-fstring ()
;;     "Change string to fstring"
;;     (interactive)
;;     (when (nth 3 (syntax-ppss))
;;       (let ((p (point)))
;;         (goto-char (nth 8 (syntax-ppss)))
;;         (insert "f")
;;         (goto-char p)
;;         (forward-char))))
;;   (defun +python-format-buffer ()
;;     "Format python buffer with black"
;;     (interactive)
;;     (python-black-buffer))
;;   :bind
;;   (map! :map python-mode-map
;;         "C-c x s" #'+python-make-fstring
;;         "C-c x f" #'+python-format-buffer))
;;
;;

;;jupter
;;https://github.com/emacs-jupyter/jupyter
;; (use-package jupyter
;;   ;; :elpaca t
;;   :ensure t
;;   :defer t
;;   :custom
;;   (org-babel-jupyter-override-src-block "python")
;;   :config
;;   (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))
;;   (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
;;                                                        (:session . "py")
;;                                                        (:kernel . "base"))))

;;=========================
;; general
;;=========================

;; (use-package general
;;   :ensure t
;;   :init
;;   :config
;;   (general-evil-setup)

;;   ;;set up 'SPC' as the global leader key
;;   (general-create-definer vf/leader-keys
;;     :states '(normal  visual  emacs)
;;     :keymaps 'override-global-map
;;     :prefix "SPC") ;; set leader
;;   ;;:global-prefix "M-SPC") ;; access leader in insert mode

;;   (vf/leader-keys
;;     "SPC" '(execute-extended-command :wk "M-x") ;; counsel-M-x
;;     ;; "/" '(find-file :wk "Find file")
;;     "." '(compile :wk "Compile")
;;     ;; "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
;;     "TAB TAB" '(comment-line :wk "Comment lines")
;;     ;; "u" '(universal-argument :wk "Universal argument")
;;     )

;;   ;; (vf/leader-keys
;;   ;;   "m" '(:ignore t :wk "Mark")
;;   ;;   "m p" '(my/remember-init :wk "Remember current position")
;;   ;;   "m j" '(my/remember-jump :wk "Jump back to latest position")
;;   ;;   ;; "m s" '(mc--mark-symbol-at-point :wk "mark symbol at point")
;;   ;;   "m s" '(mark-sexp :wk "mark symbol at point")
;;   ;;   )

;;   (vf/leader-keys
;;     "c" '(:ignore t :wk "Coding")
;;     "c h" '(eldoc-box-help-at-point :wk "open eldoc-help hover at point")
;;     ;; "c c" '(:ignore t :wk "thing copy")
;;     ;; "c s" '(thing-copy-symbol :wk "thing copy symbol")
;;     ;; "c S" '(thing-copy-sentence :wk "thing copy sentence")
;;     ;; "c w" '(thing-copy-word :wk "thing copy word")
;;     ;; "c e" '(thing-copy-to-line-end :wk "thing copy to line end")
;;     ;; "c b" '(thing-copy-to-line-beginning :wk "thing copy to line beginning")
;;     )

;;   (vf/leader-keys
;;     "l" '(:ignore t :wk "LLM like")
;;     "l s" '(gptel-send :wk "gpt发送")
;;     "l n" '(gptel :wk "gpt新buffer")
;;     "l m" '(gptel-menu :wk "gpt-send-menu")
;;     "l a" '(aidermacs-transient-menu :wk "aidermacs-transient-menu")
;;     )

;;   (vf/leader-keys
;;     "v" '(:ignore t :wk "Vandee")
;;     ;; "v C" '(my/comment-or-uncomment-region-codes :wk "comment or uncomment codes")
;;     ;; "v c" '(org-capture :wk "org-capture")
;;     "v e" '(my-execute-src-block :wk "execute-src-block")
;;     "v g" '(magit :wk "magit")
;;     ;; "v r" '(org-roam-capture :wk "org-roam-capture")
;;     "v t" '(vt :wk "open vterm")
;;     ;; "v t s" '(org-set-tags-command :wk "插入TAGS")
;;     "v a" '(:ignore t :wk "agenda and TODO")
;;     "v a t" '(org-todo :wk "编辑TODO状态")
;;     "v a i" '(org-insert-todo-heading :wk "插入任务项")

;;     ;; "v T" '(my-tags-view :wk "my-tags-view")
;;     "v T" '(my-insert-timestamp :wk "insert-timestamp")
;;     ;;"v r" '(my-remove-extra-spaces :wk "my-remove-extra-spaces")
;;     "v h" '(my-org-show-current-heading-tidily :wk "折叠其他标题")
;;     ;; "v p" '(my-buffer-path :wk "pwd")
;;     "v s" '(my-shell-command :wk "my-minibuffer-shell")
;;     "v v" '((lambda () (interactive)
;;               (find-file "~/Vandee/Areas/pkm/org/Vandee.org"))
;;             :wk "go to Vandee")
;;     "v j" '((lambda () (interactive)
;;               (find-file "~/Vandee/Areas/pkm/org/Journal.org"))
;;             :wk "go to Journals")
;;     )

;;   (vf/leader-keys
;;     "n" '(:ignore t :wk "notes")
;;     "n l" '(my/org-backlink :wk "rgrep find org backlink")
;;     "n i" '(my/insert-org-file-link :wk "insert common org file link")
;;     "n I" '(denote-link :wk "denote-link-insert")
;;     "n a" '(org-agenda :wk "org-agenda")
;;     "n f" '(denote-open-or-create :wk "denote-open")
;;     "n d" '(denote :wk "denote-create-new-note")
;;     "n e" '(org-export-dispatch :wk "org-export-dispatch")
;;     ;; "n u" '(org-roam-ui-open :wk "org-roam-ui-open")
;;     "n c" '(org-capture :wk "org-capture")
;;     "n ." '(org-emphasize :wk "org-emphasize")
;;     )

;;   (vf/leader-keys
;;     "b" '(:ignore t :wk "Bookmarks/Buffers")
;;     "b i" '(ibuffer :wk "Ibuffer")
;;     "b v" '(view-buffer :wk "View all the buffers")
;;     "b b" '(switch-to-buffer :wk "Switch to buffer")
;;     "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
;;     "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
;;     "b k" '(kill-current-buffer :wk "Kill current buffer")
;;     "b K" '(kill-all-buffers-except-scratch :wk "Kill all buffers")
;;     "b n" '(next-buffer :wk "Next buffer")
;;     "b p" '(previous-buffer :wk "Previous buffer")
;;     "b r" '(revert-buffer :wk "Reload buffer")
;;     "b R" '(rename-buffer :wk "Rename buffer")
;;     "b s" '(basic-save-buffer :wk "Save buffer")
;;     "b S" '(save-some-buffers :wk "Save multiple buffers")
;;     ;; "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file")
;;     "b d" '(bookmark-delete :wk "Delete bookmark")
;;     "b D" '(bookmark-delete-all :wk "Kill multiple buffers")
;;     "b l" '(list-bookmarks :wk "List bookmarks")
;;     "b m" '(bookmark-set :wk "Set bookmark")
;;     "b j" '(bookmark-jump :wk "Bookmark jump")
;;     )


;;   (vf/leader-keys
;;     "d" '(:ignore t :wk "Dired")
;;     "d d" '(dired :wk "Open dired")
;;     "d f" '(dired-x-find-file :wk "Dired find file")
;;     "d u" '(dired-up-directory :wk "Dired go to up dictionary")
;;     "d c" '(dired-create-empty-file :wk "Dired create file")
;;     "d C" '(dired-create-directory :wk "Dired create file")
;;     "d j" '(dired-jump :wk "Dired jump to current")
;;     "d n" '(dired-copy-filename-as-kill :wk "Dired copy the file name")
;;     "d p" '(my/dired-copy-absolute-path :wk "Dired copy the file full path")
;;     "d N" '(neotree-dir :wk "Open directory in neotree")
;;     "d r" '(dired-toggle-read-only :wk "dired-toggle-read-only")
;;     )

;;   (vf/leader-keys
;;     "e" '(:ignore t :wk "Eshell/Evaluate")
;;     "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
;;     "e d" '(eval-defun :wk "Evaluate defun containing or after point")
;;     "e e" '(eval-expression :wk "Evaluate and elisp expression")
;;     "e h" '(counsel-esh-history :which-key "Eshell history")
;;     "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
;;     "e r" '(eval-region :wk "Evaluate elisp in region")
;;     "e R" '(eww-reload :which-key "Reload current page in EWW")
;;     "e s" '(eshell :which-key "Eshell")
;;     "e w" '(eww :which-key "EWW emacs web wowser"))

;;   (vf/leader-keys
;;     "f" '(:ignore t :wk "Files")
;;     "f n" '(my/put-buffer-name-on-clipboard :wk "Put file name on clipboard")
;;     "f p" '(my/put-file-name-on-clipboard :wk "Put full file path on clipboard")
;;     "f P" '(my-buffer-path :wk "Put file parent path on clipboard")
;;     "f d" '(find-grep-dired :wk "Search for string in files in DIR")
;;     "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
;;     "f j" '(counsel-file-jump :wk "Jump to a file below current directory")
;;     ;; "f l" '(counsel-locate :wk "Locate a file")
;;     "f l" '(my/org-get-current-headline-link :wk "org-get-current-headline-link")
;;     "f r" '(recentf :wk "Find recent files")
;;     "f u" '(sudo-edit-find-file :wk "Sudo find file")
;;     "f U" '(sudo-edit :wk "Sudo edit file"))

;;   (vf/leader-keys
;;     "h" '(:ignore t :wk "Help")
;;     "h a" '(counsel-apropos :wk "Apropos")
;;     "h b" '(describe-bindings :wk "Describe bindings")
;;     "h c" '(describe-char :wk "Describe character under cursor")
;;     "h d" '(:ignore t :wk "Emacs documentation")
;;     "h d a" '(about-emacs :wk "About Emacs")
;;     "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
;;     "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
;;     "h d m" '(info-emacs-manual :wk "The Emacs manual")
;;     "h d n" '(view-emacs-news :wk "View Emacs news")
;;     "h d o" '(describe-distribution :wk "How to obtain Emacs")
;;     "h d p" '(view-emacs-problems :wk "View Emacs problems")
;;     "h d t" '(view-emacs-todo :wk "View Emacs todo")
;;     "h d w" '(describe-no-warranty :wk "Describe no warranty")
;;     "h e" '(view-echo-area-messages :wk "View echo area messages")
;;     "h f" '(describe-function :wk "Describe function")
;;     "h F" '(describe-face :wk "Describe face")
;;     "h g" '(describe-gnu-project :wk "Describe GNU Project")
;;     "h i" '(info :wk "Info")
;;     "h I" '(describe-input-method :wk "Describe input method")
;;     "h k" '(describe-key :wk "Describe key")
;;     "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
;;     "h L" '(describe-language-environment :wk "Describe language environment")
;;     "h m" '(describe-mode :wk "Describe mode")
;;     "h r" '(:ignore t :wk "Reload")
;;     "h r r" '((lambda () (interactive)
;;                 (load-file "~/.config/emacs/init.el")
;;                 (ignore (elpaca-process-queues)))
;;               :wk "Reload emacs config")
;;     "h t" '(load-theme :wk "Load theme")
;;     "h v" '(describe-variable :wk "Describe variable")
;;     "h w" '(where-is :wk "Prints keybinding for command if set")
;;     "h x" '(describe-command :wk "Display full documentation for command"))


;;   (vf/leader-keys
;;     "o" '(:ignore t :wk "Open")
;;     "o f" '(make-frame :wk "Open buffer in new frame")
;;     "o F" '(select-frame-by-name :wk "Select frame by name"))

;;   ;; projectile-command-map already has a ton of bindings
;;   ;; set for us, so no need to specify each individually.
;;   (vf/leader-keys
;;     "p" '(projectile-command-map :wk "Projectile"))

;;   (vf/leader-keys
;;     "s" '(:ignore t :wk "Search")
;;     "s d" '(my/search-cwd :wk "Search cwd")
;;     "s D" '(my/search-other-cwd :wk "Search another dictionary")
;;     "s b" '(my/search-buffer :wk "Search buffer")
;;     "s g" '(my/compile-grep-rn :wk "Compile grep")
;;     "s p" '(sanityinc/consult-ripgrep-at-point :wk "consult-ripgrep-at-point")
;;     )

;;   (vf/leader-keys
;;     "t" '(:ignore t :wk "Toggle")
;;     "t e" '(eshell-toggle :wk "Toggle eshell")
;;     "t f" '(flycheck-mode :wk "Toggle flycheck")
;;     "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
;;     "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
;;     "t o" '(org-mode :wk "Toggle org mode")
;;     "t r" '(rainbow-mode :wk "Toggle rainbow mode")
;;     "t t" '(visual-line-mode :wk "Toggle truncated lines")
;;     ;; "t v" '(vterm-toggle :wk "Toggle vterm")
;;     )

;;   (vf/leader-keys
;;     "w" '(:ignore t :wk "Windows")
;;     ;; Window splits
;;     "w c" '(evil-window-delete :wk "Close window")
;;     "w n" '(evil-window-new :wk "New window")
;;     "w s" '(evil-window-split :wk "Horizontal split window")
;;     "w v" '(evil-window-vsplit :wk "Vertical split window")
;;     "w d" '(delete-other-windows :wk "Delete other windows")
;;     ;; Window motions
;;     "w h" '(evil-window-left :wk "Window left")
;;     "w j" '(evil-window-down :wk "Window down")
;;     "w k" '(evil-window-up :wk "Window up")
;;     "w l" '(evil-window-right :wk "Window right")
;;     "w w" '(evil-window-next :wk "Goto next window")
;;     ;; Move Windows
;;     "w H" '(buf-move-left :wk "Buffer move left")
;;     "w J" '(buf-move-down :wk "Buffer move down")
;;     "w K" '(buf-move-up :wk "Buffer move up")
;;     "w L" '(buf-move-right :wk "Buffer move right"))
;;   )

;; ;; 作为 multiple cursor 的后端
;; (use-package evil-mc
;;   :ensure t
;;   :after evil
;;   :config
;;   (global-evil-mc-mode 1)
;;   (evil-define-key '(normal visual) 'global
;;     "gzm" #'evil-mc-make-all-cursors
;;     "gzu" #'evil-mc-undo-all-cursors
;;     "gzz" #'+evil/mc-toggle-cursors
;;     "gzc" #'+evil/mc-make-cursor-here
;;     "gzn" #'evil-mc-make-and-goto-next-cursor
;;     "gzp" #'evil-mc-make-and-goto-prev-cursor
;;     "gzN" #'evil-mc-make-and-goto-last-cursor
;;     "gzP" #'evil-mc-make-and-goto-first-cursor)
;;   (with-eval-after-load 'evil-mc
;;     (evil-define-key '(normal visual) evil-mc-key-map
;;       (kbd "C-n") #'evil-mc-make-and-goto-next-cursor
;;       (kbd "C-N") #'evil-mc-make-and-goto-last-cursor
;;       (kbd "C-p") #'evil-mc-make-and-goto-prev-cursor
;;       (kbd "C-P") #'evil-mc-make-and-goto-first-cursor)
;;     )
;;   )

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


;; codeium
;; (use-package codeium
;;   ;; if you use straight
;;   ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
;;   ;; otherwise, make sure that the codeium.el file is on load-path
;;   :init
;;   ;; use globally
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   ;; or on a hook
;;   ;; (add-hook 'python-mode-hook
;;   ;;           (lambda ()
;;   ;;             (setq-local completion-at-point-functions '(codeium-completion-at-point))))

;;   ;; ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
;;   ;; (add-hook 'python-mode-hook
;;   ;;           (lambda ()
;;   ;;             (setq-local completion-at-point-functions
;;   ;;                         (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
;;   ;; an async company-backend is coming soon!

;;   ;; codeium-completion-at-point is autoloaded, but you can
;;   ;; optionally set a timer, which might speed up things as the
;;   ;; codeium local language server takes ~0.2s to start up
;;   ;; (add-hook 'emacs-startup-hook
;;   ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;   ;; :defer t ;; lazy loading, if you want
;;   :config
;;   (setq use-dialog-box nil) ;; do not use popup boxes

;;   ;; if you don't want to use customize to save the api-key
;;   ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;   ;; get codeium status in the modeline
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;   ;; alternatively for a more extensive mode-line
;;   ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;   (setq codeium-api-enabled
;;         (lambda (api)
;;           (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;   ;; you can also set a config for a single buffer like this:
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local codeium/editor_options/tab_size 4)))

;;   ;; You can overwrite all the codeium configs!
;;   ;; for example, we recommend limiting the string sent to codeium for better performance
;;   (defun my-codeium/document/text ()
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;   ;; if you change the text, you should also change the cursor_offset
;;   ;; warning: this is measured by UTF-8 encoded bytes
;;   (defun my-codeium/document/cursor_offset ()
;;     (codeium-utf8-byte-length
;;      (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;   (setq codeium/document/text 'my-codeium/document/text)
;;   (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; ;; ends ----------------------------------------------------------

;; uvicorn app.main:app --reload
;; (maybe-require-package 'websocket)
;; (require 'json)
;; (require 'corfu)

;; (defun run-uvicorn-server-uv ()
;;   "Run uv python -m uvicorn app.main:app --reload in a new async shell using absolute path."
;;   (interactive)
;;   (let ((default-directory "~/Vandee/Projects/NoNotes/"))
;;     (async-shell-command
;;      "uv run uvicorn app.main:app --reload"
;;      "*Uvicorn Server*")))

;; ;; Common websocket connection function
;; (defvar corfu-ws-endpoint-rag "ws://localhost:8000/api/v1/ws/complete/rag/emacs-corfu"
;;   "WebSocket endpoint for RAG-based completion.")

;; (defvar corfu-ws-endpoint-vector "ws://localhost:8000/api/v1/ws/complete/vector/emacs-corfu"
;;   "WebSocket endpoint for vector-based completion.")

;; (defvar corfu-ws--client nil
;;   "WebSocket client connection.")

;; (defvar corfu-ws--current-endpoint nil
;;   "Current WebSocket endpoint in use.")

;; (defvar corfu-ws--pending nil
;;   "Callback function to be called when receiving completion response.")

;; (defvar corfu-ws--debounce-timer nil
;;   "Timer for debouncing completion requests.")

;; (defvar corfu-ws--debounce-delay 0.3
;;   "Delay in seconds for debouncing completion requests.")

;; (defun corfu-ws-connect (endpoint)
;;   "Establish websocket connection to ENDPOINT if not already connected."
;;   (when (and corfu-ws--client
;;              (websocket-openp corfu-ws--client)
;;              (not (string= endpoint corfu-ws--current-endpoint)))
;;     (websocket-close corfu-ws--client)
;;     (setq corfu-ws--client nil))

;;   (unless (and corfu-ws--client (websocket-openp corfu-ws--client))
;;     (setq corfu-ws--client
;;           (websocket-open
;;            endpoint
;;            :on-message (lambda (_ws frame)
;;                          (let* ((data (websocket-frame-payload frame))
;;                                 (json (json-parse-string data :object-type 'alist))
;;                                 (suggestions (mapcar (lambda (item)
;;                                                        (alist-get 'text item))
;;                                                      (alist-get 'suggestions json))))
;;                            (when corfu-ws--pending
;;                              (funcall corfu-ws--pending suggestions)
;;                              (setq corfu-ws--pending nil))))))
;;     (setq corfu-ws--current-endpoint endpoint)))

;; (defun corfu-ws-request-debounced (prefix callback endpoint max-results)
;;   "Debounced version of corfu-ws-request."
;;   (when corfu-ws--debounce-timer
;;     (cancel-timer corfu-ws--debounce-timer))
;;   (setq corfu-ws--debounce-timer
;;         (run-with-timer corfu-ws--debounce-delay nil
;;                         #'corfu-ws-request prefix callback endpoint max-results)))

;; (defun corfu-ws-request (prefix callback endpoint max-results)
;;   "Send completion request for PREFIX to ENDPOINT, call CALLBACK with results.
;; Optional MAX-RESULTS limits the number of suggestions (defaults to 5)."
;;   (corfu-ws-connect endpoint)
;;   (let* ((request (json-encode `(("text" . ,prefix)
;;                                  ("cursor_position" . ,(length prefix))
;;                                  ("max_results" . ,(or max-results 5))))))
;;     (setq corfu-ws--pending callback)
;;     (websocket-send-text corfu-ws--client request)))

;; (defun corfu-ws-capf (endpoint max-results)
;;   "Create a completion-at-point-function using ENDPOINT and MAX-RESULTS suggestions."
;;   (let* ((start (save-excursion (skip-syntax-backward "w_") (point)))
;;          (end (point)))
;;     (list start end
;;           (lambda (str pred action)
;;             (if (eq action 'metadata)
;;                 '(metadata (category . corfu-ws))
;;               (let ((cont (lambda (cands)
;;                             (funcall completion-in-region-function start end cands))))
;;                 (if (string= endpoint corfu-ws-endpoint-rag)
;;                     ;; Use debounced version for RAG completion
;;                     (corfu-ws-request-debounced (buffer-substring-no-properties start end) cont endpoint max-results)
;;                   ;; Use normal version for vector completion
;;                   (corfu-ws-request (buffer-substring-no-properties start end) cont endpoint max-results))
;;                 nil))))))

;; ;; Specific completion functions for different modes
;; (defun corfu-ws-complete-rag ()
;;   "Manually trigger RAG-based completion with single result."
;;   (interactive)
;;   (let ((completion-at-point-functions (list (lambda () (corfu-ws-capf corfu-ws-endpoint-rag 1)))))
;;     (completion-at-point)))

;; (defun corfu-ws-complete-vector ()
;;   "Manually trigger vector-based completion with multiple results."
;;   (interactive)
;;   (let ((completion-at-point-functions (list (lambda () (corfu-ws-capf corfu-ws-endpoint-vector 5)))))
;;     (completion-at-point)))

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


;;; agent-shell OpenCode Subagent Support
;;; Shows available subagents and enables @@subagent invocation syntax
;; (defvar agent-shell-opencode--subagents nil
;;   "Cached list of available subagents.")

;; (defun agent-shell-opencode--get-subagent-description (agent-name)
;;   "Get description for subagent AGENT-NAME from its config file."
;;   (let* ((config-file (expand-file-name (format "%s.md" agent-name)
;;                                         "~/.config/opencode/agents/"))
;;          (description
;;           (when (file-exists-p config-file)
;;             (with-temp-buffer
;;               (insert-file-contents config-file)
;;               (goto-char (point-min))
;;               (when (re-search-forward "^description: \\(.*\\)$" nil t)
;;                 (match-string 1))))))
;;     (or description "Subagent")))

;; (defun agent-shell-opencode--list-subagents ()
;;   "List all subagents using opencode agent list command.
;; Returns list of agent alists with :id, :name, :description."
;;   (let* ((output (shell-command-to-string "opencode agent list"))
;;          (lines (split-string output "\n"))
;;          agents)
;;     (dolist (line lines)
;;       (when (string-match "^\\([^\s]+\\) (subagent)$" line)
;;         (let* ((name (match-string 1 line))
;;                (description (agent-shell-opencode--get-subagent-description name)))
;;           (push `((:id . ,name)
;;                   (:name . ,name)
;;                   (:description . ,description))
;;                 agents))))
;;     (nreverse agents)))

;; (defun agent-shell-opencode--format-subagents (subagents)
;;   "Format SUBAGENTS list for display."
;;   (mapconcat
;;    (lambda (agent)
;;      (let ((name (map-elt agent :name))
;;            (desc (map-elt agent :description)))
;;        (format "@@%s - %s" (propertize name 'font-lock-face 'font-lock-keyword-face) desc)))
;;    subagents
;;    "\n"))

;; (defun agent-shell-opencode--show-subagents-fragment ()
;;   "Add subagents UI fragment after available modes fragment."
;;   (when-let ((subagents agent-shell-opencode--subagents)
;;              ((> (length subagents) 0))
;;              ((fboundp 'agent-shell--update-fragment))
;;              (state agent-shell--state)
;;              (request-count (map-elt state :request-count))
;;              (buffer (map-elt state :buffer)))
;;     (with-current-buffer buffer
;;       (save-excursion
;;         (goto-char (point-max))
;;         (when (text-property-search-backward
;;                'agent-shell-ui-state nil
;;                (lambda (_ prop-state)
;;                  (equal (map-elt prop-state :qualified-id)
;;                         (format "%s-available_modes" request-count)))
;;                t)
;;           (let* ((block-range (agent-shell-ui--block-range :position (point)))
;;                  (insert-pos (map-elt block-range :end)))
;;             (goto-char insert-pos)
;;             (let ((inhibit-read-only t))
;;               (insert "\n\n")
;;               (agent-shell-ui--insert-fragment
;;                (agent-shell-ui-make-fragment-model
;;                 :namespace-id (number-to-string request-count)
;;                 :block-id "available_subagents"
;;                 :label-left (propertize "Available subagents" 'font-lock-face 'font-lock-doc-markup-face)
;;                 :body (agent-shell-opencode--format-subagents subagents))
;;                (format "%s-available_subagents" request-count)))))))))

;; (defun agent-shell-opencode--load-subagents-delayed ()
;;   "Load subagents and show them in UI after delay."
;;   (run-with-timer
;;    0.5 nil
;;    (lambda ()
;;      (when-let ((subagents (agent-shell-opencode--list-subagents))
;;                 ((> (length subagents) 0)))
;;        (setq agent-shell-opencode--subagents subagents)
;;        (agent-shell-opencode--show-subagents-fragment)))))

;; (defun agent-shell-opencode--around-initiate-session (orig-fun &rest args)
;;   "Around advice for agent-shell--initiate-session."
;;   (let* ((plist args)
;;          (on-session-init (plist-get plist :on-session-init))
;;          (new-on-session-init
;;           (lambda ()
;;             (funcall on-session-init)
;;             (agent-shell-opencode--load-subagents-delayed))))
;;     (plist-put plist :on-session-init new-on-session-init)
;;     (apply orig-fun plist)))

;; (advice-add 'agent-shell--initiate-session :around
;;             #'agent-shell-opencode--around-initiate-session)

;; ;;; Transform @@subagent syntax to @subagent before sending to OpenCode
;; (defun agent-shell-opencode--filter-send-command-args (args)
;;   "Transform @@subagent to @subagent in prompt before sending."
;;   (let ((prompt (plist-get args :prompt)))
;;     (when (and prompt (stringp prompt))
;;       (plist-put args :prompt (replace-regexp-in-string "@@" "@" prompt))))
;;   args)

;; (advice-add 'agent-shell--send-command :filter-args
;;             #'agent-shell-opencode--filter-send-command-args)

;; ;;; @@subagent completion support
;; (defun agent-shell-opencode--subagent-completion-at-point ()
;;   "Complete subagent names after @@ prefix."
;;   (when-let* ((bounds (agent-shell--completion-bounds "[:alnum:]_-" ?@))
;;               ((save-excursion
;;                  (goto-char (map-elt bounds :start))
;;                  (eq (char-before) ?@)))
;;               (subagents agent-shell-opencode--subagents)
;;               (descriptions (mapcar (lambda (a)
;;                                       (cons (map-elt a :name)
;;                                             (map-elt a :description)))
;;                                     subagents)))
;;     (list (map-elt bounds :start) (map-elt bounds :end)
;;           (mapcar #'car descriptions)
;;           :exclusive t
;;           :annotation-function
;;           (lambda (name)
;;             (when-let* ((desc (map-elt descriptions name)))
;;               (concat "  " desc)))
;;           :company-kind (lambda (_) 'function))))

;; (defun agent-shell-opencode--setup-completion ()
;;   "Set up @@subagent completion in agent-shell."
;;   (add-hook 'completion-at-point-functions
;;             #'agent-shell-opencode--subagent-completion-at-point
;;             nil t))

;; (add-hook 'agent-shell-mode-hook #'agent-shell-opencode--setup-completion)

;; ----------------------------------------------------------
;; gptel
;; ----------------------------------------------------------

;; ;; gptel 设置默认模型
;; (use-package gptel
;;   :defer t
;;   :config
;;   (let* ((auth-info (car (auth-source-search :host "generativelanguage.googleapis.com")))
;;          (api-key (and auth-info (plist-get auth-info :secret)))) ; 使用 :secret 获取 API key
;;     (if api-key
;;         (setq
;;          ;; gptel-model 'gemini-2.5-flash
;;          gptel-backend (gptel-make-gemini "Gemini"
;;                          :stream t
;;                          :key api-key
;;                          :models '("gemini-2.5-flash"))
;;          )
;;       (error "未在 auth-source 中找到 Gemini API 密钥！请检查您的 auth-source 配置。")))

;;   ;; gptel-model "qwen2.5"
;;   ;; gptel-backend (gptel-make-ollama "Ollama"
;;   ;;                 :host "localhost:11434"
;;   ;;                 :stream t
;;   ;;                 :models '("qwen2.5:14b")))

;;   (gptel-make-ollama "Ollama"           ;Any name of your choosing
;;     :host "localhost:11434"             ;Where it's running
;;     :stream t                           ;Stream responses
;;     :models '("qwen3:1.7b"))            ;List of models

;;   (gptel-make-ollama "Deepseek"         ;Any name of your choosing
;;     :host "localhost:11434"             ;Where it's running
;;     :stream t                           ;Stream responses
;;     :models '("deepseek-r1:14b"))       ;List of models

;;   ;; preset
;;   ;; https://github.com/karthink/gptel/?tab=readme-ov-file#option-presets
;;   (gptel-make-preset 'explain
;;     :system "Explain what this code does to a novice programmer.")

;;   ;; https://github.com/karthink/gptel/issues/514
;;   (gptel-make-tool
;;    :function (lambda (url)
;;                (let* ((proxy-url (concat "https://r.jina.ai/" url))
;;                       (buffer (url-retrieve-synchronously proxy-url)))
;;                  (with-current-buffer buffer
;;                    (goto-char (point-min)) (forward-paragraph)
;;                    (let ((dom (libxml-parse-html-region (point) (point-max))))
;;                      (run-at-time 0 nil #'kill-buffer (current-buffer))
;;                      (with-temp-buffer
;;                        (shr-insert-document dom)
;;                        (buffer-substring-no-properties (point-min) (point-max)))))))
;;    :name "read_url"
;;    :description "Fetch and read the contents of a URL using Jina.ai reader"
;;    :args (list '(:name "url"
;;                        :type "string"
;;                        :description "The URL to read"))
;;    :category "web")
;;   ;; (gptel-make-tool
;;   ;;  :function (lambda (url)
;;   ;;              (with-current-buffer (url-retrieve-synchronously url)
;;   ;;                (goto-char (point-min)) (forward-paragraph)
;;   ;;                (let ((dom (libxml-parse-html-region (point) (point-max))))
;;   ;;                  (run-at-time 0 nil #'kill-buffer (current-buffer))
;;   ;;                  (with-temp-buffer
;;   ;;                    (shr-insert-document dom)
;;   ;;                    (buffer-substring-no-properties (point-min) (point-max))))))
;;   ;;  :name "read_url"
;;   ;;  :description "Fetch and read the contents of a URL"
;;   ;;  :args (list '(:name "url"
;;   ;;                      :type "string"
;;   ;;                      :description "The URL to read"))
;;   ;;  :category "web")

;;   (gptel-make-tool
;;    :function (lambda (buffer text)
;;                (with-current-buffer (get-buffer-create buffer)
;;                  (save-excursion
;;                    (goto-char (point-max))
;;                    (insert text)))
;;                (format "Appended text to buffer %s" buffer))
;;    :name "append_to_buffer"
;;    :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
;;    :args (list '(:name "buffer"
;;                        :type "string"
;;                        :description "The name of the buffer to append text to.")
;;                '(:name "text"
;;                        :type "string"
;;                        :description "The text to append to the buffer."))
;;    :category "emacs")

;;   ;; Message buffer logging tool
;;   (gptel-make-tool
;;    :function (lambda (text)
;;                (message "%s" text)
;;                (format "Message sent: %s" text))
;;    :name "echo_message"
;;    :description "Send a message to the *Messages* buffer"
;;    :args (list '(:name "text"
;;                        :type "string"
;;                        :description "The text to send to the messages buffer"))
;;    :category "emacs")

;;   ;; buffer retrieval tool
;;   (gptel-make-tool
;;    :function (lambda (buffer)
;;                (unless (buffer-live-p (get-buffer buffer))
;;                  (error "Error: buffer %s is not live." buffer))
;;                (with-current-buffer  buffer
;;                  (buffer-substring-no-properties (point-min) (point-max))))
;;    :name "read_buffer"
;;    :description "Return the contents of an Emacs buffer"
;;    :args (list '(:name "buffer"
;;                        :type "string"
;;                        :description "The name of the buffer whose contents are to be retrieved"))
;;    :category "emacs")


;;   (gptel-make-tool
;;    :function (lambda (directory)
;;                (mapconcat #'identity
;;                           (directory-files directory)
;;                           "\n"))
;;    :name "list_directory"
;;    :description "List the contents of a given directory"
;;    :args (list '(:name "directory"
;;                        :type "string"
;;                        :description "The path to the directory to list"))
;;    :category "filesystem")

;;   (gptel-make-tool
;;    :function (lambda (parent name)
;;                (condition-case nil
;;                    (progn
;;                      (make-directory (expand-file-name name parent) t)
;;                      (format "Directory %s created/verified in %s" name parent))
;;                  (error (format "Error creating directory %s in %s" name parent))))
;;    :name "make_directory"
;;    :description "Create a new directory with the given name in the specified parent directory"
;;    :args (list '(:name "parent"
;;                        :type "string"
;;                        :description "The parent directory where the new directory should be created, e.g. /tmp")
;;                '(:name "name"
;;                        :type "string"
;;                        :description "The name of the new directory to create, e.g. testdir"))
;;    :category "filesystem")

;;   (gptel-make-tool
;;    :function (lambda (path filename content)
;;                (let ((full-path (expand-file-name filename path)))
;;                  (with-temp-buffer
;;                    (insert content)
;;                    (write-file full-path))
;;                  (format "Created file %s in %s" filename path)))
;;    :name "create_file"
;;    :description "Create a new file with the specified content"
;;    :args (list '(:name "path"
;;                        :type "string"
;;                        :description "The directory where to create the file")
;;                '(:name "filename"
;;                        :type "string"
;;                        :description "The name of the file to create")
;;                '(:name "content"
;;                        :type "string"
;;                        :description "The content to write to the file"))
;;    :category "filesystem")

;;   (gptel-make-tool
;;    :function (lambda (filepath)
;;                (with-temp-buffer
;;                  (insert-file-contents (expand-file-name filepath))
;;                  (buffer-string)))
;;    :name "read_file"
;;    :description "Read and display the contents of a file"
;;    :args (list '(:name "filepath"
;;                        :type "string"
;;                        :description "Path to the file to read.  Supports relative paths and ~."))
;;    :category "filesystem")

;;   )

;; ;; gtpel + org-protocol
;; (require 'org-protocol)
;; (require 'gptel)

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; ;; Handler for gptel queries from browser
;; (defun my/gptel-org-protocol-handler (info)
;;   "Handle gptel query from org-protocol.
;; INFO is the data passed by org-protocol."
;;   (let ((text (plist-get info :text)))
;;     (when (and text (not (string-empty-p text)))
;;       (let ((query (decode-coding-string (url-unhex-string text) 'utf-8)))
;;         (message "Received gptel query: %s" query)
;;         ;; Create or switch to gptel buffer
;;         (let ((buffer (get-buffer-create "*gptel-browser*")))
;;           (with-current-buffer buffer
;;             (unless (eq major-mode 'gptel-default-mode)
;;               (funcall gptel-default-mode))
;;             (gptel-mode 1)
;;             (goto-char (point-max))
;;             (insert "\n\n--- From Browser ---\n")
;;             (insert query)
;;             (insert "\n")
;;             (goto-char (point-max))
;;             ;; Send the query to gptel
;;             (gptel-send)
;;             (display-buffer buffer)))))))

;; ;; Register the protocol handler
;; (setq org-protocol-protocol-alist
;;       (append org-protocol-protocol-alist
;;               '(("gptel-browser"
;;                  :protocol "gptel"
;;                  :function my/gptel-org-protocol-handler))))

;; JavaScript bookmarklet for browser (copy this as bookmark URL):
;; Basic version:
;; javascript:(function(){const selectedText=window.getSelection().toString().trim();if(!selectedText){alert('Please select some text first');return;}const pageTitle=document.title;const pageUrl=window.location.href;const fullText=`From: ${pageTitle}\nURL: ${pageUrl}\n\nSelected text:\n${selectedText}`;const encodedText=encodeURIComponent(fullText);const protocolUrl=`org-protocol://gptel?text=${encodedText}`;window.location.href=protocolUrl;})();

;; Enhanced version with prompt for question:
;; javascript:(function(){const selectedText=window.getSelection().toString().trim();if(!selectedText){alert('Please select some text first');return;}const pageTitle=document.title;const pageUrl=window.location.href;const userQuestion=prompt('Optional: Add a question or context:');let fullText=`From: ${pageTitle}\nURL: ${pageUrl}\n\n`;if(userQuestion){fullText+=`Question: ${userQuestion}\n\n`;}fullText+=`Selected text:\n${selectedText}`;const encodedText=encodeURIComponent(fullText);const protocolUrl=`org-protocol://gptel?text=${encodedText}`;window.location.href=protocolUrl;})();

;; (custom-set-faces
;;  ;; global 部分-------------------------------------------
;;  '(match ((t (:background unspecified :foreground "#79C0FF"))))
;;  '(popup-tip-face ((t (:background "#1D1F21" :foreground "#d1d5db"))))
;;  '(shadow ((t (:foreground "gray70"))))
;;  '(show-paren-match ((t (:background "SteelBlue3" :foreground "gray90"))))
;;  '(show-paren-mismatch ((t (:background "HotPink3" :foreground "white"))))

;;  ;; 设置默认字体和大小
;;  ;; '(default ((t (:family "GeistMono Nerd Font" :height 140 :weight light :background "#1D1F21" :foreground "#d1d5db"))))
;;  '(default ((t (:font "fontset-myfontset" :height 140 :background "#1D1F21" :foreground "#d1d5db"))))
;;  '(cursor ((t (:background "orange"))))

;;  '(link ((t (:foreground "#96a6c8" :underline t))))
;;  '(highlight ((t (:background "#5A5F66" :foreground unspecified)))) ;; #60656C

;;  ;; 设置注释的样式
;;  '(font-lock-comment-face ((t (:foreground "#787878"))))
;;  '(font-lock-comment-delimiter-face ((t (:foreground "#787878"))))
;;  ;; 设置字符串的样式
;;  '(font-lock-string-face ((t (:foreground "#A1D08E")))) ;#A1D08E,#A7D08A #73c936 #96D0FF

;;  ;; 设置常量的样式
;;  ;; '(font-lock-constant-face ((t (:weight normal :foreground "95a99f"))))

;;  ;; 设置内置函数的样式
;;  '(font-lock-builtin-face ((t (:weight normal :foreground "#79C0FF"))))

;;  ;; 设置关键字的样式
;;  '(font-lock-keyword-face ((t (:weight normal :foreground "#FFA657")))) ;#FFA657-橙色 ,#FF7B72

;;  ;; 设置函数名的样式
;;  ;; '(font-lock-function-name-face ((t (:weight normal :foreground "#96a6c8"))))

;;  ;; 设置变量名的样式
;;  ;; '(font-lock-variable-name-face ((t (:weight normal :foreground "#c5b49f")))) ;#c5b49f-浅咖，#bc9575-焦糖橙 备用

;;  ;; 设置类型的样式
;;  ;; '(font-lock-type-face ((t (:foreground "#9DA3A2" :slant normal :weight normal)))) ; #a6adac ,#B6B9AE 很浅的灰绿备用

;;  ;; 设置文档字符串的样式
;;  '(font-lock-doc-face ((t (:weight normal :foreground "#787878"))))

;;  ;; 设置当前行号颜色
;;  '(line-number-current-line ((t (:foreground "#ffdd33"))))

;;  ;; 设置行号颜色
;;  '(line-number ((t (:foreground "gray30"))))

;;  ;; org 部分 --------------------------------------------------------
;;  ;; 代码块背景和边框
;;  ;; '(org-block-begin-line
;;  ;; ((t (:background "#343942" :foreground "#7F8490" :extend t))))
;;  '(org-level-1 ((t (:height 1.4 :weight normal))))
;;  '(org-level-2 ((t (:height 1.3 :weight normal))))
;;  '(org-level-3 ((t (:height 1.2 :weight normal))))
;;  '(org-level-4 ((t (:height 1.1 :weight normal))))
;;  '(org-level-5 ((t (:height 1.05 :weight normal))))
;;  '(org-level-6 ((t (:inherit outline-6 :height 1.05 :weight normal))))
;;  '(org-level-7 ((t (:inherit outline-7 :height 1.0 :weight normal))))
;;  '(org-level-8 ((t (:inherit outline-8 :height 1.0 :weight normal))))

;;  ;; 设置文档标题 (#+TITLE:)
;;  '(org-document-title ((t (:inherit default :weight bold
;;                                     :height 1.5 ; 文档标题字体大小
;;                                     :underline nil ; 添加下划线
;;                                     ))))           ; 标题颜色

;;  ;; 设置特殊关键字 (#+STARTUP: 等)
;;  '(org-meta-line ((t (:inherit font-lock-comment-face
;;                                :height 1.1 ; 关键字字体大小
;;                                ;; :slant italic     ; 斜体
;;                                ))))

;;  ;; 设置时间戳颜色
;;  '(org-date ((t (:foreground "#61AFEF" :background unspecified :weight normal))))

;;  ;; 设置 org-tags 的颜色
;;  '(org-tag ((t (:foreground "#8B949E" :weight normal :height 0.9 :inherit nil :slant normal))))

;;  ;; 设置 org-block-begin-line 的样式
;;  '(org-block-begin-line ((t (:background "#252525" :foreground "#757575" :extend t :italic t))))

;;  ;; 设置 org-block 的样式
;;  '(org-block ((t (:background "#252525" :extend t))))

;;  ;; 设置 org-block-end-line 的样式
;;  '(org-block-end-line ((t (:background "#252525" :foreground "#757575" :extend t :italic t))))

;;  ;; 设置 org-code 的样式
;;  '(org-code ((t (:foreground "#da8548" :weight normal))))
;;  ;; 设置 mode-line
;;  '(mode-line ((t (:box nil))))
;;  '(mode-line-inactive ((t (:box nil))))
;;  ;; org 部分 ends----------------------------------------------------

;;  ;; 插件部分 --------------------------------------------------------
;;  '(diff-hl-change ((t (:background unspecified :foreground "#ffc125"))))
;;  '(diff-hl-delete ((t (:background unspecified :foreground "#ff3030"))))
;;  '(diff-hl-insert ((t (:background unspecified :foreground "green1"))))
;;  '(diredfl-date-time ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-deletion ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-dir-heading ((t (:foreground "#95a99f"))))
;;  '(diredfl-dir-name ((t (:foreground "#79C0FF"))))
;;  '(diredfl-dir-priv ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-exec-priv ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-file-name ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-file-suffix ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-no-priv ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-number ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-other-priv ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-rare-priv ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-read-priv ((t (:foreground unspecified :background unspecified))))
;;  '(diredfl-write-priv ((t (:foreground unspecified :background unspecified))))
;;  '(which-key-command-description-face ((t (:foreground "#79C0FF"))))
;;  '(which-key-group-description-face ((t (:foreground "#79C0FF" :weight medium))))
;;  '(orderless-match-face-1 ((t (:foreground "#79C0FF" :weight medium))))
;;  '(orderless-match-face-2 ((t (:foreground "#79C0FF" :weight medium))))
;;  '(orderless-match-face-3 ((t (:foreground "#79C0FF" :weight medium))))
;;  '(vertico-current ((t (:extend nil :background "gray35" :foreground "gray100"))))
;;  '(marginalia-key ((t (:foreground "#79C0FF"))))
;;  '(corfu-default ((t (:background "gray15" :foreground "gray80"))))
;;  '(anzu-mode-line ((t (:foreground "#79C0FF" :weight bold))))
;;  '(anzu-mode-line-no-match ((t (:foreground "#79C0FF"))))
;;  '(company-preview ((t (:background "gray35"))))
;;  )


;;; init-archive.el ends here
