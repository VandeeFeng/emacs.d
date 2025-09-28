;;; init-archive.el --- archive packages and configs -*- lexical-binding: t -*-
;; Author: Vandee
;; Created: 2025-09-19
;; Keywords:
;;; Commentary:
;;; Code:


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
