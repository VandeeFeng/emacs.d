;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;=========================
;; languages setting
;;=========================

;; indentation
(setq-default indent-tabs-mode nil)

(setq c-default-style "k&r"
      c-basic-offset 4)

;; (add-hook 'sh-mode-hook
;;           (lambda ()
;;             (setq sh-basic-offset 2)))

(setq sh-basic-offset 2
      sh-indentation 2)

;;=========================
;; editing functions
;;=========================
;; 其他常用操作在 file:/home/vandee/Vandee/Areas/pkm/org/Code_Notes.org::*编辑
;; 还没有想清楚怎么优化 vim 键位和 Emacs 的结合。不太想频繁的在 insert 模式和其他模式之间切换
;; 但是 vim 里导航的逻辑很好，最省事的逻辑还是多切换。组合键导航的劣势很大

;; 启用系统复制粘贴
(setq select-enable-clipboard t)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)

;; jk 退出 insert
(with-eval-after-load 'evil
  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay 0.3) ;; 版本更新之后，默认j k 的判断时间变少了
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)))

;; Mark & edit
(global-set-key (kbd "M-m") 'set-mark-command)

(with-eval-after-load 'evil
  (define-prefix-command 'my/mark-map)
  (define-key evil-normal-state-map (kbd "m") 'my/mark-map)

  ;; ;; 下面这段可以实现连续按 m 执行 mark-sexp，但是就不会显示按键绑定提示了
  ;; (defun my/smart-mark-sexp ()
  ;;   "If mark is active, call `mark-sexp`, otherwise enter `my/mark-map`."
  ;;   (interactive)
  ;;   (if mark-active
  ;;       (call-interactively 'mark-sexp)
  ;;     (set-transient-map my/mark-map)))
  ;; (define-key evil-normal-state-map (kbd "m") 'my/smart-mark-sexp)

  ;; this keybindings only use not in insert mode
  (dolist (binding '(("m" . mark-sexp)
                     ("p" . set-pin-mark) ; 这是自己实现的 init-mark.el
                     ("g" . goto-pin-mark) ; 简化版的 bookmark
                     ("d" . mark-defun)
                     (":" . comment-indent)
                     (";" . comment-dwim)
                     ("k" . move-dup-move-lines-up)
                     ("j" . move-dup-move-lines-down)
                     ("u" . upcase-dwim) ; equal to g U in vim
                     ("s" . thing-copy-symbol)
                     ("S" . thing-cut-symbol)
                     ("w" . thing-copy-word)
                     ("W" . thing-cut-word)
                     ("-" . thing-copy-to-line-end)
                     ("_" . thing-cut-to-line-end)
                     ("0" . thing-copy-to-line-beginning)
                     (")" . thing-cut-to-line-beginning)))
    (define-key my/mark-map (kbd (car binding)) (cdr binding)))
  )

;; (define-prefix-command 'my/mark-map)
;; (global-set-key (kbd "M-c") 'my/mark-map)
;; (define-key my/mark-map (kbd "s") 'thing-copy-symbol)
;; (define-key my/mark-map (kbd "M-s") 'thing-cut-symbol)
;; (define-key my/mark-map (kbd "S") 'thing-copy-sentence)
;; (define-key my/mark-map (kbd "M-S") 'thing-cut-sentence)
;; (define-key my/mark-map (kbd "w") 'thing-copy-word)
;; (define-key my/mark-map (kbd "M-w") 'thing-cut-word)

(with-eval-after-load 'evil
  ;; normal, visual, insert
  (dolist (binding '(("C-d" . backward-kill-sexp)
                     ("C-f" . kill-sexp)
                     ("C-k" . kill-visual-line)
                     ("C-y" . clipboard-yank)
                     ;; ("C-a" . beginning-of-line)
                     ;; ("C-e" . end-of-line)
                     ;; ("C-n" . forward-sexp)
                     ;; ("C-p" . backward-sexp)
                     ;; ("C-w" . thing-copy-word)
                     ;; ("C-W" . thing-cut-word)
                     ;; ("C-s" . thing-copy-sexp)
                     ;; ("C-S" . thing-cut-sexp)
                     ))
    (dolist (state '(normal visual insert))
      (evil-global-set-key state (kbd (car binding)) (cdr binding))))

  ;; ;; visual, insert
  ;; (dolist (binding '(("C-h" . backward-char)
  ;;                    ("C-l" . forward-char)
  ;;                    ("C-j" . next-line)
  ;;                    ("C-k" . previous-line)))
  ;;   (dolist (state '(visual insert))
  ;;     (evil-global-set-key state (kbd (car binding)) (cdr binding))))

  ;; normal visual
  (dolist (binding '(("-" . end-of-line)
                     ))
    (dolist (state '(normal visual))
      (evil-global-set-key state (kbd (car binding)) (cdr binding))))

  (define-key evil-motion-state-map (kbd "C-v") nil)
  (global-unset-key (kbd "C-v"))
  (global-set-key (kbd "S-<backspace>") 'delete-char))

;; magit
(with-eval-after-load 'magit
  (dolist (binding '(("J" . magit-status-jump)
                     ("K" . magit-discard)
                     ("j" . magit-next-line)
                     ("k" . magit-previous-line)
                     ("Z" . magit-stash-drop)))
    (define-key magit-status-mode-map (kbd (car binding)) (cdr binding)))

  (dolist (map '(magit-mode-map magit-status-mode-map magit-log-mode-map magit-diff-mode-map magit-revision-mode-map))
    (when (boundp map)
      ;; (define-key (symbol-value map) (kbd "j") #'magit-section-forward)
      ;; (define-key (symbol-value map) (kbd "k") #'magit-section-backward)
      (define-key (symbol-value map) (kbd "J") #'magit-revision-jump)
      (define-key (symbol-value map) (kbd "j") #'magit-next-line)
      (define-key (symbol-value map) (kbd "k") #'magit-previous-line))
    )
  )


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

;; compile grep
(defun my/compile-grep-rn (pattern)
  "Run `grep -irn` with the given PATTERN in the current directory."
  (interactive "sGrep pattern: ")
  (compile (format "grep -irn '%s' ." pattern)))

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

;;=========================
;; packages
;;=========================

;; 快速移动当前行内容，感觉和原生的差不多
;; 现在用的是https://github.com/wyuenho/move-dup
;;(use-package move-text)
;; (global-set-key (kbd "M-S-<up>") 'move-text-up)
;; (global-set-key (kbd "M-S-<down>") 'move-text-down)

;; thing-edit
;; https://github.com/manateelazycat/thing-edit
(require 'thing-edit)

;; ==============================================
;; mutiple cursor
(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; mutiple edit ends here
;; ========================================

;; outline-indent
;; https://github.com/jamescherti/outline-indent.el

;; (use-package outline-indent
;;   :ensure t
;;   :defer t
;;   :commands outline-indent-minor-mode
;;   :custom
;;   (outline-indent-ellipsis " ▼"))


(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

(maybe-require-package 'list-unicode-display)


;;; Some basic preferences

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)



;; Huge files

(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

(require-package 'vlf)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))


;;; A simple visible bell which works in all terminal types
(require-package 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)



;;; Newline behaviour (see also electric-indent-mode, enabled above)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)))



(with-eval-after-load 'subword
  (diminish 'subword-mode))



(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-ts-mode-hook 'display-line-numbers-mode))


;; 设置代码辅助列宽限制竖线
;; (when (boundp 'display-fill-column-indicator)
;; (setq-default indicate-buffer-boundaries 'left)
;; (setq-default display-fill-column-indicator-character ?┊)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))
(setq-default display-fill-column-indicator nil)
(global-display-fill-column-indicator-mode -1)


(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (with-eval-after-load 'symbol-overlay
    (diminish 'symbol-overlay-mode)
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))


;;; Zap *up* to char is a handy pair for zap-to-char
(global-set-key (kbd "M-Z") 'zap-up-to-char)



(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode))


;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)

(when (fboundp 'repeat-mode)
  (add-hook 'after-init-hook 'repeat-mode))


;;; Handy key bindings

(with-eval-after-load 'help
  (define-key help-map "A" 'describe-face))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))


;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)



;;; Page break lines

(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (with-eval-after-load 'page-break-lines
    (diminish 'page-break-lines-mode)))



;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.

(require-package 'move-dup)
(global-set-key [M-S-up] 'move-dup-move-lines-up)
(global-set-key [M-S-down] 'move-dup-move-lines-down)

(global-set-key (kbd "C-c d") 'move-dup-duplicate-down)
(global-set-key (kbd "C-c u") 'move-dup-duplicate-up)


;;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL

(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp) ; C-M-u, C-M-up



;;; Cut/copy the current line if no region is active
(require-package 'whole-line-or-region)
(add-hook 'after-init-hook 'whole-line-or-region-global-mode)
(with-eval-after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-local-mode))



;; M-^ is inconvenient, so also bind M-j
(global-set-key (kbd "M-J") 'join-line)


;; Random line sorting
(defun sanityinc/sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))



(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)


(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(setq-default which-key-idle-delay 1.5)
(with-eval-after-load 'which-key
  (diminish 'which-key-mode))


(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
