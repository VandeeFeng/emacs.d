;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; ==============================================
;; editing functions
;; ==============================================

;; Harper
;; https://writewithharper.com/docs/integrations/emacs
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(text-mode . ("harper-ls" "--stdio"))))

;; jk 退出 insert
(with-eval-after-load 'evil
  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)))

(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-j") 'next-line)
  (define-key evil-insert-state-map (kbd "C-k") 'previous-line))

(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)

;; 启用系统复制粘贴
(setq select-enable-clipboard t)

(defun my/remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun my/remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

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

;; 在 normal 模式下将 - 键导航到行尾
(with-eval-after-load 'evil
  (defun move-to-end-of-line ()
    "Move the cursor to the end of the current line."
    (interactive)
    (end-of-line))

  ;; 在 normal 模式下将 - 键绑定到这个函数
  (define-key evil-normal-state-map (kbd "-") #'move-to-end-of-line)
  (define-key evil-visual-state-map (kbd "-") #'move-to-end-of-line)
  (define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-visual-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-a" )'beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-visual-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-e" )'end-of-line)

  )

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

(defun my-tags-view ()
  "Show all headlines for org files matching a TAGS criterion."
  (interactive)
  (let* ((org-agenda-files '("~/Vandee/Areas/pkm"))
         (org-tags-match-list-sublevels nil))
    (call-interactively 'org-tags-view)))

;;---------------------------------------
;; 文件路径和文件名相关
;;---------------------------------------
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


;; ==========================================
;; packages
;; ==========================================

;; thing-edit
;; https://github.com/manateelazycat/thing-edit
(require 'thing-edit)

;; ==============================================
;; mutiple cursor

;; https://github.com/emacs-evil/evil-surround
;; (use-package evil-surround
;;   :ensure t
;;   :after evil
;;   :config
;;   (global-evil-surround-mode 1))

;; https://github.com/hlissner/evil-multiedit
;; https://github.com/gabesoft/evil-mc
(use-package evil-multiedit
  :ensure t
  :after evil
  ;; :init
  ;; (setq evil-multiedit-dwim-motion-keys nil)
  :config
  (evil-define-key 'normal 'global
    (kbd "M-d")   #'evil-multiedit-match-symbol-and-next
    (kbd "M-D")   #'evil-multiedit-match-symbol-and-prev)
  (evil-define-key 'visual 'global
    "R"           #'evil-multiedit-match-all
    (kbd "M-d")   #'evil-multiedit-match-and-next
    (kbd "M-D")   #'evil-multiedit-match-and-prev)
  (evil-define-key '(visual normal) 'global
    (kbd "C-M-d") #'evil-multiedit-restore)
  (with-eval-after-load 'evil-mutliedit
    (evil-define-key 'multiedit 'global
      (kbd "M-d")   #'evil-multiedit-match-and-next
      (kbd "M-S-d") #'evil-multiedit-match-and-prev
      (kbd "M-RET")   #'evil-multiedit-toggle-or-restrict-region)
    (evil-define-key '(multiedit multiedit-insert) 'global
      (kbd "C-n")   #'evil-multiedit-next
      (kbd "C-p")   #'evil-multiedit-prev))
  )

;; (use-package evil-mc
;;   :ensure t
;;   :after evil
;;   :config
;;   ;; evil-mc
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
;;       (kbd "C-P") #'evil-mc-make-and-goto-first-cursor))
;;   )


(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; mutiple edit ends here
;; ========================================

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
(global-set-key (kbd "M-j") 'join-line)


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
