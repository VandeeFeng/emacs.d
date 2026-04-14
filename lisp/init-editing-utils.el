;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;=========================
;; languages setting
;;=========================

;; Disable Bidirectional Text Scanning
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; indentation
(setq-default indent-tabs-mode nil)

(setq c-default-style "k&r"
      c-basic-offset 4)

;; (add-hook 'sh-mode-hook
;;           (lambda ()
;;             (setq sh-basic-offset 2)))

(setq sh-basic-offset 2
      sh-indentation 2)

(setq css-indent-offset 2)

(use-package snap-indent
  :ensure t
  :config
  ;; Exclude TypeScript/JavaScript since biome handles formatting
  ;; (setq snap-indent-excluded-modes
  ;;       (append snap-indent-excluded-modes
  ;;               '(typescript-mode tsx-mode)))
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save t))
  :hook (prog-mode . snap-indent-mode))


;;=========================
;; editing functions
;;=========================
;; 其他常用操作在 file:/home/vandee/Vandee/Areas/pkm/org/Code_Notes.org::*编辑
;; 还没有想清楚怎么优化 vim 键位和 Emacs 的结合。不太想频繁的在 insert 模式和其他模式之间切换
;; 但是 vim 里导航的逻辑很好，最省事的逻辑还是多切换。组合键导航的劣势很大
;; stay simple !
;; normal 模式下，多用我现在自定义的 m(mark) 键位。可以弥补 vim 传统模式里 ciw,diw 这类编辑。
;; 还有 r ； ’ 这几个键位候选自定义前缀, ctrl+, ctrl+. 也是候选
;; 对 sexp，symbol 编辑的不足。最常用的还是 mm，可以连续使用 Emacs 的 mark,按 v 就可以推出 mark
;; Emacs 的 mark 在 evil 下就对应 visual 模式
;; normal 模式下，f，t 快速导航到字符串，再加上 w,e,b 就很高效了


;;; Kill Ring (Emacs’s Clipboard History) and Clipboard

;; Save the Clipboard Before Killing
;; Here’s a scenario: you copy a URL from your browser, switch to Emacs, kill a line with C-k, and then try to yank the URL you copied earlier with C-y. Gone. The kill replaced it on the clipboard.
(setq save-interprogram-paste-before-kill t)
;; Kill the same line three times and you get three identical entries in the kill ring, wasting slots. This deduplicates them:
(setq kill-do-not-save-duplicates t)
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
  (define-key evil-motion-state-map (kbd ";") nil)
  (evil-define-key '(normal visual) 'global
    "gc" #'comment-dwim)
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
                     (";" . comment-indent)
                     ;; (";" . comment-dwim) ; use gc instead
                     ("k" . move-dup-move-lines-up)
                     ("j" . move-dup-move-lines-down)
                     ("u" . upcase-dwim) ; equal to g U in vim, also ~ to capitalize
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
  (dolist (binding '(("C-s" . backward-kill-sexp)
                     ("C-S-s" . kill-back-to-indentation)
                     ("C-d" . kill-sexp)
                     ("C-k" . kill-visual-line)
                     ("C-y" . clipboard-yank)
                     ("C-a" . beginning-of-line)
                     ("C-e" . end-of-line)
                     ("C-b" . backward-char) ;; 覆盖 evil 的键位，回归 Emacs 默认键位
                     ("C-f" . forward-char) ;; 覆盖 evil 的键位，回归 Emacs 默认键位
                     ("C-n" . next-line) ;; 覆盖 evil 的键位，回归 Emacs 默认键位
                     ("C-p" . previous-line) ;; 覆盖 evil 的键位，回归 Emacs 默认键位
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
                     ;; use vim default ]] instead
                     ;; ("<" . beginning-of-defun)
                     ;; (">" . end-of-defun)
                     ("," . backward-sexp)
                     ("." . forward-sexp)
                     ))
    (dolist (state '(normal visual))
      (evil-global-set-key state (kbd (car binding)) (cdr binding))))

  (define-key evil-motion-state-map (kbd "C-v") nil)
  (global-unset-key (kbd "C-v"))
  (global-set-key (kbd "S-<backspace>") 'delete-char))

;; magit
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "x") 'magit-file-checkout)
  (dolist (binding '(("J" . magit-status-jump)
                     ("K" . magit-discard)
                     ("j" . magit-next-line)
                     ("k" . magit-previous-line)
                     ("Z" . magit-stash-drop)
                     ))
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

;; compile grep
(defun my/compile-grep-rn (pattern)
  "Run `grep -irn` with the given PATTERN in the current directory."
  (interactive "sGrep pattern: ")
  (compile (format "grep -irn '%s' ." pattern)))


;;=========================
;; packages
;;=========================

;; yasnippet
(use-package yasnippet
  :ensure t
  )

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

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
  (when (boundp 'electric-pair-pairs)
    (setq electric-pair-pairs
          (append electric-pair-pairs '((?\{ . ?\}) (?\' . ?\')))))
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


;; (when (maybe-require-package 'symbol-overlay)
;;   (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
;;     (add-hook hook 'symbol-overlay-mode))
;;   (with-eval-after-load 'symbol-overlay
;;     (diminish 'symbol-overlay-mode)
;;     (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
;;     (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
;;     (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
;;     (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))


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

;; (global-set-key (kbd "C-x C-.") 'pop-global-mark)

;; (when (maybe-require-package 'avy)
;;   (global-set-key (kbd "C-;") 'avy-goto-char-timer))


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

;; (global-set-key (kbd "C-c d") 'move-dup-duplicate-down)
;; (global-set-key (kbd "C-c u") 'move-dup-duplicate-up)


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
                   (lambda (_ _) (eq (random 2) 0)))))))



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
