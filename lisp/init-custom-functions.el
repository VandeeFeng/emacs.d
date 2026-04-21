;;; init-custom-functions.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; My customized functions

;; 还有一部分的自定义函数在 init-editing-utils.el
;; 原来 general 的按键设置转到 init-hyper.el

;;=========================
;; 自定义函数
;;=========================

(defun my/org-backlink ()
  "Find all org files in the current directory that link to the current file.
Searches for org links containing the current filename."
  (interactive)
  (unless buffer-file-name
    (error "Current buffer is not visiting a file"))

  (let* ((current-file (file-name-nondirectory buffer-file-name))
         (current-dir (file-name-directory buffer-file-name))
         (file-quoted (regexp-quote current-file))
         ;; Match links containing the filename in various formats:
         ;; - file:path/to/filename.org
         ;; - file:path/to/filename.org::*headline
         ;; - path/to/filename.org][description
         (search-pattern (concat "\\[\\[.*" file-quoted)))
    (rgrep search-pattern "*.org" current-dir)))

;; 窗口移动,buffer 切换
(global-set-key (kbd "M-,") 'previous-buffer) ;; emacs 默认是 ctrl+x 左右箭头切换
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "M-.") 'next-buffer)
  (define-key evil-normal-state-map (kbd "M-.") 'next-buffer)
  (define-key evil-insert-state-map (kbd "M-.") 'next-buffer)
  (define-key evil-visual-state-map (kbd "M-.") 'next-buffer))
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)
(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "M-d") nil))
(global-set-key (kbd "M-d") 'delete-window)
(global-set-key (kbd "M-c") 'kill-current-buffer)
(global-set-key (kbd "M-C") 'kill-all-buffers-except-scratch)
(setq windmove-wrap-around t)
;; 解决和 org-mode 的冲突
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "M-h") 'windmove-left)
            (define-key org-mode-map (kbd "M-j") 'windmove-down)))
;; end

(global-unset-key (kbd "C-SPC")) ;; 和我习惯的切换输入法快捷键冲突了,这个快捷键系统默认绑定的是 set-mark-command

;; progress bar
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

    (insert (format "%s\n" progress-bar))))

(defun date-days-in-year (year)
  "Return the number of days in YEAR."
  (if (and (numberp year)
           (= (% year 4) 0)
           (or (not (= (% year 100) 0))
               (= (% year 400) 0)))
      366
    365))
;; end

;;=========================
;; Search
;;=========================

;; search bilibli in eww
(defun my/eww-bilibili-search (keyword)
  "Search Bilibili for KEYWORD using `eww'.The keyword is read from the minibuffer and URL-encoded automatically."
  (interactive "sBilibili search keyword: ")
  (let* ((base "https://search.bilibili.com/all?keyword=")
         (encoded (url-hexify-string keyword))
         (url (concat base encoded)))
    (eww url)))

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


;;=========================
;; markdown to org
;;=========================

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
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (unless (member (buffer-name buffer) '("*scratch*" "*Messages*"))
        (kill-buffer buffer)
        (setq count (1+ count))))
    (unless (and (boundp 'eglot-managed-buffers) eglot-managed-buffers)
      (message "All %d buffers closed except *scratch* and *Messages*." count))))


;; org-mode realtime editor
(defvar my-org-preview-file (expand-file-name "org-preview.html" "~/.emacs.d/cache/")
  "用于存放 Org 文件实时预览的固定 HTML 文件路径。")

(defvar-local my-org-preview-active nil
  "是否正在进行 Org 文件的实时预览。")

(defun my-org-generate-html ()
  "生成当前 Org 文件的 HTML 内容。"
  (unless (and buffer-file-name
               (derived-mode-p 'org-mode)
               (string-match-p "\\.org\\'" buffer-file-name))
    (error "my-org-generate-html: current buffer is not an org file"))
  (org-export-string-as (buffer-string) 'html t))

(defun my-org-preview-in-browser ()
  "更新浏览器中的 Org 文件预览。"
  ;; 从文件路径中获取目录名
  (let ((cache-dir (file-name-directory my-org-preview-file)))
    ;; 如果目录不存在，则创建它
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t)))
  (let ((html (my-org-generate-html)))
    (with-temp-file my-org-preview-file
      (insert html))))

(defun my-org-preview-stop ()
  "停止当前 buffer 的 Org 预览。"
  (interactive)
  (setq my-org-preview-active nil)
  (remove-hook 'after-save-hook #'my-org-preview-in-browser t)
  (remove-hook 'kill-buffer-hook #'my-org-preview-stop t)
  (message "Org 预览已停止。"))

(defun my-org-preview ()
  "手动控制 Org 文件的 HTML 预览开关。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "my-org-preview: current buffer is not an org buffer"))
  (if my-org-preview-active
      (my-org-preview-stop)
    (setq my-org-preview-active t)
    (my-org-preview-in-browser)
    (browse-url (concat "file://" my-org-preview-file))
    (add-hook 'after-save-hook #'my-org-preview-in-browser nil t)
    (add-hook 'kill-buffer-hook #'my-org-preview-stop nil t)
    (message "Org 预览已启动。")))

;; 执行代码块
(defun my-execute-src-block ()
  "Execute the selected org code block and display a message."
  (interactive)
  (message "Executing selected org code block...")
  (org-babel-execute-src-block))

;;在minibuffer里使用shell指令
;;https://stackoverflow.com/questions/10121944/passing-emacs-variables-to-minibuffer-shell-commands
(defun my-shell-command (command &optional output-buffer error-buffer)
  "Run a shell command with the current file (or marked dired files).
In the shell command, the file(s) will be substituted wherever a '%' is."
  (interactive (list (read-from-minibuffer "Shell command: "
                                           nil nil nil 'shell-command-history)
                     current-prefix-arg
                     shell-command-default-error-buffer))
  (cond ((buffer-file-name)
         (setq command (replace-regexp-in-string "%" (buffer-file-name) command nil t)))
        ((and (equal major-mode 'dired-mode) (save-excursion (dired-move-to-filename)))
         (setq command (replace-regexp-in-string "%" (mapconcat 'identity (dired-get-marked-files) " ") command nil t))))
  (shell-command command output-buffer error-buffer))

;; simple ai code shell command
(defun my/ai-shell-command (prompt)
  "Execute ai command asynchronously and display the output."
  (interactive "sAI prompt: ")
  (let* ((command-name "pi")
         (output-buffer-name "*AI Output*")
         (output-buffer (get-buffer-create output-buffer-name))
         (shell-program (or (getenv "SHELL") shell-file-name))
         ;; Use shell-quote-argument
         (command-str (format "%s --no-session -p %s" command-name (shell-quote-argument prompt))))
    (with-current-buffer output-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq-local header-line-format (format "AI Output for prompt: %s" prompt)))
    ;; (display-buffer output-buffer) ; Show the buffer immediately
    (message "AI command running asynchronously...")
    ;; Start the async process
    (let ((process (start-process "ai-process"
                                  output-buffer-name
                                  shell-program
                                  "-lc"
                                  command-str)))
      ;; Store command-name for sentinel
      (process-put process :command-name command-name)
      ;; Set a function to be called when the process finishes
      (set-process-sentinel process #'my/ai-process-sentinel))))

(defun my/ai-process-sentinel (process _event)
  "Sentinel for the ai async process. Handles success and error cases."
  (when (memq (process-status process) '(exit signal))
    (let* ((buffer (process-buffer process))
           (exit-code (process-exit-status process))
           (command-name (process-get process :command-name)))
      (cond
       ;; Case 1: Process failed (non-zero exit code)
       ((/= exit-code 0)
        (kill-buffer buffer)
        (if (= exit-code 127)
            (message "Error: '%s' command not found. Please ensure it's in your shell's PATH." command-name)
          (message "Error: AI command failed with exit code %d." exit-code)))

       ;; Case 2: Process succeeded but produced no output
       ((zerop (with-current-buffer buffer (buffer-size)))
        (kill-buffer buffer)
        (message "AI command finished with no output."))

       ;; Case 3: Success
       (t
        (with-current-buffer buffer
          (setq buffer-read-only t)
          (goto-char (point-min)))
        (display-buffer buffer)
        (message "AI command finished."))))))


;; 显示当前 heading 内容并折叠其他
;; https://emacs-china.org/t/org-mode/23205
(defun my-org-show-current-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (save-excursion
      (outline-back-to-heading)
      (unless (and (bolp) (org-on-heading-p))
        (org-up-heading-safe)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children))
    ))


(defun my-insert-timestamp ()
  "Insert a custom formatted timestamp."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a %H:%M>")))

(defun my/org-tags-view (tags-match)
  "Search for headings with TAGS-MATCH in all .org files in the current directory of the buffer.
TAGS-MATCH is a tags search string, like '+project-work'.
This version disables tag inheritance to avoid listing all headings if a filetag matches."
  (interactive "sTags match (e.g., +project-work): ")
  (unless (derived-mode-p 'org-mode)
    (user-error "This function must be called from an Org-mode buffer"))
  (let* ((current-file (buffer-file-name))
         (current-dir (file-name-directory current-file))
         ;; (parent-dir (expand-file-name ".." current-dir))
         ;; (org-files (directory-files-recursively parent-dir "\\.org$"))
         (org-files (directory-files-recursively current-dir "\\.org$"))
         )
    (if (null org-files)
        (message "No .org files found in parent directory: %s" current-dir)
      (let ((org-agenda-files org-files)
            (org-use-tag-inheritance nil))  ; Disable inheritance to ignore filetags
        (org-tags-view nil tags-match)))))

;; org 标题链接
(defun my/org-get-current-headline-link ()
  "Get the org-mode link for the current headline, removing TODO keywords, tags, and preceding spaces."
  (interactive)
  (let* ((headline (org-get-heading))
         (todo-keywords-list (car org-todo-keywords-1)) ; 获取第一个关键词集合
         (todo-keywords (if (listp todo-keywords-list) ; 检查是否是列表
                            todo-keywords-list      ; 如果是列表，直接使用
                          '("TODO" "DOING" "DONE" "WAITING" "HOLD" "CANCELLED"))) ; 否则提供一个默认列表
         (headline-without-todo headline)
         headline-without-tags
         trimmed-headline)

    ;; 移除 TODO 关键词
    (dolist (keyword todo-keywords)
      (when (string-prefix-p (concat keyword " ") headline)
        (setq headline-without-todo (string-remove-prefix (concat keyword " ") headline))))

    ;; 移除末尾的标签
    (setq headline-without-tags (replace-regexp-in-string " +:[a-zA-Z0-9_:]*$" "" headline-without-todo))

    ;; 移除标题前后的空格
    (setq trimmed-headline (string-trim-left headline-without-tags))

    (when trimmed-headline
      (let ((link (concat "[[file:" (buffer-file-name) "::*" trimmed-headline "][" trimmed-headline "]]")))
        (kill-new link)
        (message "Org-mode link for current headline (without TODOs and tags) copied to clipboard.")))))

;;=========================
;; 文件路径和文件名相关
;;=========================
;; https://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer#3669681
(defun my-buffer-path ()
  "copy buffer's full path to kill ring"
  (interactive)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (kill-new (file-name-directory file-path))
      (message "Copied parent directory path: %s" (file-name-directory file-path)))))

(defun my/dired-copy-absolute-path ()
  "Copy the absolute file name of the file at point in Dired."
  (interactive)
  (dired-copy-filename-as-kill 0)) ; 使用前缀参数 0 表示绝对路径

;; https://github.com/rexim/dotfiles/blob/master/.emacs.rc/misc-rc.el
(defun rc/buffer-file-name ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun rc/parent-directory (path)
  (file-name-directory (directory-file-name path)))

(defun rc/root-anchor (path anchor)
  (cond
   ((string= anchor "") nil)
   ((file-exists-p (concat (file-name-as-directory path) anchor)) path)
   ((string-equal path "/") nil)
   (t (rc/root-anchor (rc/parent-directory path) anchor))))

(defun rc/clipboard-org-mode-file-link (anchor)
  (interactive "sRoot anchor: ")
  (let* ((root-dir (rc/root-anchor default-directory anchor))
         (org-mode-file-link (format "file:%s::%d"
                                     (if root-dir
                                         (file-relative-name (rc/buffer-file-name) root-dir)
                                       (rc/buffer-file-name))
                                     (line-number-at-pos))))
    (kill-new org-mode-file-link)
    (message org-mode-file-link)))

;; Taken from here:
;; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun my/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (rc/buffer-file-name)))
    (when filename
      (kill-new filename)
      (message filename))))

(defun my/put-buffer-name-on-clipboard ()
  "Put the current buffer name on the clipboard"
  (interactive)
  (kill-new (buffer-name))
  (message (buffer-name)))

(defun rc/kill-autoloads-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (string-match-p "-autoloads.el" name)
        (kill-buffer buffer)
        (message "Killed autoloads buffer %s" name)))))

(defun my/insert-org-file-link ()
  "Insert an Org-mode file link with automatic filename as description."
  (interactive)
  (let* ((file (read-file-name "Select file: "))
         (filename (file-name-base file))
         (link (format "[[file:%s][%s]]" file filename)))
    (insert link)))

(provide 'init-custom-functions)
;;; init-custom-functions.el ends here
