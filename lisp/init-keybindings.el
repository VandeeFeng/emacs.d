;;; init-keybindings.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Customized key bindings
;; 还有一部分的自定义函数在 init-editing-utils.el
;;----------------------------------------------------------------------------
;;自定义函数
;;----------------------------------------------------------------------------

(global-set-key (kbd "M-w") 'evil-window-delete)

(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-j") 'next-line)
  (define-key evil-insert-state-map (kbd "C-k") 'previous-line))

(defun my/org-datetree-progress-bar ()
  "Scan the datetree in the current Org file and insert a custom progress bar."
  (interactive)
  (let* ((file (buffer-file-name))
         (today (current-time))
         (current-year (nth 5 (decode-time today)))
         (total-days (date-days-in-year current-year))
         (year-start (encode-time 0 0 0 1 1 current-year))
         (day-of-year (1+ (- (time-to-days today) (time-to-days year-start))))
         (days-so-far 0)
         (progress-bar-width 20)
         progress-bar)

    (unless (and file (string-match-p "\\.org$" file))
      (error "Not in an Org file"))

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (format "^\\*\\*\\* %d-\\([0-1][0-9]\\)-\\([0-3][0-9]\\)" current-year) nil t)
        (setq days-so-far (1+ days-so-far))))

    (let* ((progress (if (and (numberp day-of-year) (numberp total-days) (> total-days 0))
                         (/ (float day-of-year) total-days)
                       0.0))
           (filled-bars (floor (* progress progress-bar-width)))
           (empty-bars (- progress-bar-width filled-bars)))

      (setq progress-bar (concat "["
                                 (make-string filled-bars ?█)
                                 (make-string empty-bars ?-)
                                 "] "
                                 (format "%.1f%%" (* progress 100))))

      (message "Year: %d, Day of Year: %d, Total Days: %d, Progress: %.2f"
               current-year day-of-year total-days progress))

    (insert (format "\n* Progress for %d\n%s\n" current-year progress-bar))))

(defun date-days-in-year (year)
  "Return the number of days in YEAR."
  (if (and (numberp year)
           (= (% year 4) 0)
           (or (not (= (% year 100) 0))
               (= (% year 400) 0)))
      366
    365))



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

;;---------------------------------------------
;; Search
;; ---------------------------------------------
;; inspired by doom emacs
(defun my/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
         (if arg
             (read-directory-name "Search directory: ")
           default-directory)))
    (if (featurep 'vertico)
        (cond
         ((and (require 'consult nil t)
               (executable-find "rg"))
          (call-interactively #'consult-ripgrep))
         ((require 'consult nil t)
          (call-interactively #'consult-grep))
         (t
          (call-interactively #'grep-find)))
      (call-interactively #'grep-find))))


(defun my/search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (my/search-cwd 'other))


(defun my/search-buffer ()
  "Conduct a text search on the current buffer.

If a selection is active and multi-line, perform a search restricted to that
region.

If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p
          (narrow-to-region start end)))
      (if (featurep 'vertico)  ;; 检查是否启用了 Vertico
          (if (and start end (not multiline-p))
              (consult-line (buffer-substring-no-properties start end))
            (call-interactively #'consult-line))
        (message "Vertico is not installed or enabled.")))))



;; 去除多余空格

;; (defun my-remove-extra-spaces ()
;;   "Remove extra spaces in the current buffer."
;;   (interactive)
;;   (replace-regexp "\\(\\s-\\)\\s-" "\\1" nil (point-min) (point-max)))

;; ;; 绑定到一个快捷键，例如 C-c s
;; (global-set-key (kbd "C-c s") 'my-remove-extra-spaces)

;;-------------------------------------------------------------------------------------------
;;
;; markdown to org
;;
;;-------------------------------------------------------------------------------------------

(defun my-markdown-to-org ()
  (interactive)
  (save-excursion
    ;; 转换Markdown标题为Org-mode标题
    (goto-char (point-min))
    (while (re-search-forward "^\s*\\(#+\\) \\(.*\\)" nil t)
      (let ((level (length (match-string 1)))
            (title1 (match-string 2)))
        (replace-match (concat (make-string level ?*) " " title1)))))
  ;; 转换Markdown链接为Org-mode链接,但是跳过图片链接
  (goto-char (point-min))
  (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
    (let ((title (match-string 1))
          (url (match-string 2)))
      (unless (and (string-match "\\(jpeg\\|png\\|svg\\)" url)
                   (string-match "https" url))
        (replace-match (format "[[%s][%s]]" url title)))))
  ;; 转换Markdown代码块为Org-mode代码块
  (goto-char (point-min))
  (while (re-search-forward "^```" nil t)
    (if (looking-back "^```")
        (progn
          (replace-match "#+begin_src")
          (re-search-forward "^```" nil t)
          (if (looking-back "^```")
              (replace-match "#+end_src")))))
  ;; 转换Markdown行内代码为Org-mode行内代码，添加空格
  (goto-char (point-min))
  (while (re-search-forward "`\\([^`]+?\\)`" nil t)
    (replace-match "~\\1~" t))

  ;; 转换Markdown强调为Org-mode强调，添加空格
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\|[^*]\\)\\*\\*\\([^*]+?\\)\\*\\*\\($\\|[^*]\\)" nil t)
    ;; 检查当前行是否以 * 开头（org 标题）
    (unless (save-excursion
              (beginning-of-line)
              (looking-at "^\\*+\\s-"))
      ;; 保留匹配的前后字符（如果有的话）
      (replace-match "\\1 *\\2* \\3"))))



;; 关闭所有 buffers
(defun kill-all-buffers-except-scratch ()
  "Close all buffers except *scratch* and *Messages*, and show the number of closed buffers."
  (interactive)
  (let ((count 0))  ; 初始化计数器
    (dolist (buffer (buffer-list))
      (unless (member (buffer-name buffer) '("*scratch*" "*Messages*"))
        (kill-buffer buffer)
        (setq count (1+ count))))  ; 每关闭一个buffer就增加计数
    (message "All %d buffers closed except *scratch* and *Messages*." count)))



;; org-mode realtime editor
(defvar my-org-preview-file (expand-file-name "org-preview.html" "~/.emacs.d/cache/")
  "用于存放 Org 文件实时预览的固定 HTML 文件路径。")

(defvar my-org-preview-active nil
  "是否正在进行 Org 文件的实时预览。")

(defun my-org-generate-html ()
  "生成当前 Org 文件的 HTML 内容。"
  (org-export-string-as (buffer-string) 'html t))

(defun my-org-preview-in-browser ()
  "更新浏览器中的 Org 文件预览。"
  (let ((html (my-org-generate-html)))
    (with-temp-file my-org-preview-file
      (insert html))))

(defun my-org-preview ()
  "手动控制 Org 文件的 HTML 预览开关。"
  (interactive)
  (if my-org-preview-active
      (progn
        (setq my-org-preview-active nil)
        (remove-hook 'after-save-hook 'my-org-preview-in-browser)
        (message "Org 预览已停止。"))
    (setq my-org-preview-active t)
    (my-org-preview-in-browser)
    (browse-url (concat "file://" my-org-preview-file))
    (add-hook 'after-save-hook 'my-org-preview-in-browser)
    (message "Org 预览已启动。")))

;;----------------------------------------------------------------------------
;; general
;;----------------------------------------------------------------------------

(use-package general
  :ensure t
  :init
  :config
  (general-evil-setup)

  ;;set up 'SPC' as the global leader key
  (general-create-definer vf/leader-keys
    :states '(normal  visual  emacs)
    :keymaps 'override-global-map
    :prefix "SPC") ;; set leader
  ;;:global-prefix "M-SPC") ;; access leader in insert mode

  (vf/leader-keys
    "SPC" '(execute-extended-command :wk "M-x") ;; counsel-M-x
    "/" '(find-file :wk "Find file")
    "." '(compile :wk "Compile")
    "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "TAB TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument"))

  (vf/leader-keys
    "c" '(:ignore t :wk "Coding")
    "c p" '(my/remember-init :wk "Remember current position")
    "c j" '(my/remember-jump :wk "Jump back to latest position")
    ;; "c c" '(:ignore t :wk "thing copy")
    "c s" '(thing-copy-sentence :wk "thing copy sentence")
    "c w" '(thing-copy-word :wk "thing copy word")
    "c e" '(thing-copy-to-line-end :wk "thing copy to line end")
    "c b" '(thing-copy-to-line-beginning :wk "thing copy to line beginning")
    )

  (vf/leader-keys
    "l" '(:ignore t :wk "LLM like")
    "l s" '(gptel-send :wk "gpt发送")
    "l n" '(gptel :wk "gpt新buffer")
    "l m" '(gptel-menu :wk "gpt-send-menu")
    "l a" '(aidermacs-transient-menu :wk "aidermacs-transient-menu")
    )

  (vf/leader-keys
    "v" '(:ignore t :wk "Vandee")
    ;; "v C" '(my/comment-or-uncomment-region-codes :wk "comment or uncomment codes")
    ;; "v c" '(org-capture :wk "org-capture")
    "v e" '(my-execute-src-block :wk "execute-src-block")
    ;; "v r" '(org-roam-capture :wk "org-roam-capture")
    "v t" '(vt :wk "open vterm")
    ;; "v t s" '(org-set-tags-command :wk "插入TAGS")
    "v a" '(:ignore t :wk "agenda and TODO")
    "v a t" '(org-todo :wk "编辑TODO状态")
    "v a i" '(org-insert-todo-heading :wk "插入任务项")

    ;; "v T" '(my-tags-view :wk "my-tags-view")
    "v T" '(my-insert-timestamp :wk "insert-timestamp")
    ;;"v r" '(my-remove-extra-spaces :wk "my-remove-extra-spaces")
    "v h" '(my-org-show-current-heading-tidily :wk "折叠其他标题")
    ;; "v p" '(my-buffer-path :wk "pwd")
    "v s" '(my-shell-command :wk "my-minibuffer-shell")
    "v v" '((lambda () (interactive)
              (find-file "~/Vandee/Areas/pkm/org/Vandee.org"))
            :wk "go to Vandee")
    "v j" '((lambda () (interactive)
              (find-file "~/Vandee/Areas/pkm/org/Journal.org"))
            :wk "go to Journals")
    )

  (vf/leader-keys
    "n" '(:ignore t :wk "notes")
    "n i" '(denote-link :wk "denote-link-insert")
    "n a" '(org-agenda :wk "org-agenda")
    "n f" '(denote :wk "denote-find")
    "n e" '(org-export-dispatch :wk "org-export-dispatch")
    "n u" '(org-roam-ui-open :wk "org-roam-ui-open")
    "n c" '(org-capture :wk "org-capture")
    "n ." '(org-emphasize :wk "org-emphasize")
    )

  (vf/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(switch-to-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b v" '(view-buffer :wk "View all the buffers")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-all-buffers-except-scratch :wk "Kill all buffers")
    "b D" '(kill-some-buffers :wk "Kill multiple buffers")
    ;; "b l" '(list-bookmarks :wk "List bookmarks")
    ;; "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    ;; "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file")
    )


  (vf/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d c" '(dired-create-empty-file :wk "Dired create file")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(dired-copy-filename-as-kill :wk "Dired copy the file name")
    "d p" '(my/dired-copy-absolute-path :wk "Dired copy the file full path")
    "d N" '(neotree-dir :wk "Open directory in neotree")
    "d r" '(dired-toggle-read-only :wk "dired-toggle-read-only")
    )

  (vf/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e R" '(eww-reload :which-key "Reload current page in EWW")
    "e s" '(eshell :which-key "Eshell")
    "e w" '(eww :which-key "EWW emacs web wowser"))

  (vf/leader-keys
    "f" '(:ignore t :wk "Files")
    "f n" '(my/put-buffer-name-on-clipboard :wk "Put file name on clipboard")
    "f p" '(my/put-file-name-on-clipboard :wk "Put full file path on clipboard")
    "f P" '(my-buffer-path :wk "Put file parent path on clipboard")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
    "f j" '(counsel-file-jump :wk "Jump to a file below current directory")
    ;; "f l" '(counsel-locate :wk "Locate a file")
    "f l" '(my/org-get-current-headline-link :wk "org-get-current-headline-link")
    "f r" '(recentf :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (vf/leader-keys
    "h" '(:ignore t :wk "Help")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
                (load-file "~/.config/emacs/init.el")
                (ignore (elpaca-process-queues)))
              :wk "Reload emacs config")
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))


  (vf/leader-keys
    "o" '(:ignore t :wk "Open")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

  ;; projectile-command-map already has a ton of bindings
  ;; set for us, so no need to specify each individually.
  (vf/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))

  (vf/leader-keys
    "s" '(:ignore t :wk "Search")
    "s d" '(my/search-cwd :wk "Search cwd")
    "s D" '(my/search-other-cwd :wk "Search another dictionary")
    "s b" '(my/search-buffer :wk "Search buffer")
    "s c" '(my/compile-grep-rn :wk "Compile grep")
    )

  (vf/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t o" '(org-mode :wk "Toggle org mode")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  (vf/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    "w d" '(delete-other-windows :wk "Delete other windows")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right"))
  )


(provide 'init-keybindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here
