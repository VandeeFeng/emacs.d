;;; init-hydra.el --- Hydra keybindings configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'hydra)

;; Setup general.el for global leader key that works across all major modes
(maybe-require-package 'general)
(general-evil-setup t)

;; Define global leader key using general.el's override keymap
(general-define-key
 :states '(normal motion visual)
 :keymaps 'override
 :prefix "SPC"
 "" '(hydra-leader/body :wk "hydra leader"))

;; Global C-c C-<key> bindings for hydra access
(global-set-key (kbd "C-c C-w") 'hydra-windows/body)
(global-set-key (kbd "C-c C-f") 'hydra-files/body)
(global-set-key (kbd "C-c C-b") 'hydra-buffers/body)
(global-set-key (kbd "C-c C-n") 'hydra-notes/body)
(global-set-key (kbd "C-c C-l") 'hydra-llm/body)
(global-set-key (kbd "C-c C-v") 'hydra-vandee/body)
(global-set-key (kbd "C-c C-d") 'hydra-dired/body)
(global-set-key (kbd "C-c C-e") 'hydra-eval/body)
;; (global-set-key (kbd "C-c C-h") 'hydra-help/body)
(global-set-key (kbd "C-c C-s") 'hydra-search/body)
(global-set-key (kbd "C-c C-t") 'hydra-toggle/body)
(global-set-key (kbd "C-c C-o") 'hydra-open/body)

;; Mode-specific hydra bindings
(define-key evil-normal-state-map (kbd "C-.") nil)
(defun hydra-mode-setup ()
  "Setup hydra bindings for specific modes."
  (cond
   ((eq major-mode 'dired-mode)
    (local-set-key (kbd "C-.") 'hydra-dired/body))

   ((eq major-mode 'org-mode)
    (local-set-key (kbd "C-.") 'hydra-org/body))

   ((eq major-mode 'magit-status-mode)
    (local-set-key (kbd "C-.") 'hydra-magit/body))

   ((derived-mode-p 'prog-mode)
    (local-set-key (kbd "C-.") 'hydra-code/body))))

;; Add mode-specific setup to hooks
(add-hook 'dired-mode-hook 'hydra-mode-setup)
(add-hook 'org-mode-hook 'hydra-mode-setup)
(add-hook 'magit-status-mode-hook 'hydra-mode-setup)
(add-hook 'prog-mode-hook 'hydra-mode-setup)

(defhydra hydra-leader (:color blue :hint nil)
  "
^hydra^                     (C-c C-<key> for direct access)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
^General^             ^Misc^           ^Buffers^           ^Windows^
^^^^^^^^---------------------------------------------------------------------
[_SPC_] M-x           [_f_] files      [_b_] buffers       [_w_] windows
[_._] compile         [_n_] notes      [_d_] dired         [_e_] eval/eshell
[_v_] vandee          [_l_] LLM        [_s_] search        [_t_] toggle
[_c_] code            [_h_] help       [_p_] projects      [_o_] open
"
  ;; M-x alternatives
  ("SPC" execute-extended-command)
  ("." compile)

  ;; Files (f)
  ("f" hydra-files/body)

  ;; Code (c)
  ("c" hydra-code/body)

  ;; Notes (n)
  ("n" hydra-notes/body)

  ;; LLM (l)
  ("l" hydra-llm/body)

  ;; Vandee (v)
  ("v" hydra-vandee/body)

  ;; Buffers (b)
  ("b" hydra-buffers/body)

  ;; Dired (d)
  ("d" hydra-dired/body)

  ;; Eval/Eshell (e)
  ("e" hydra-eval/body)

  ;; Help (h)
  ("h" hydra-help/body)

  ;; Open (o)
  ("o" hydra-open/body)

  ;; Projects (p)
  ("p" hydra-projects/body)

  ;; Search (s)
  ("s" hydra-search/body)

  ;; Toggle (t)
  ("t" hydra-toggle/body)

  ;; Windows (w)
  ("w" hydra-windows/body)

  ;; Quit
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Files hydra
(defhydra hydra-files (:color blue :hint nil)
  "
^Files^                    (C-c C-f to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_n_] copy buffer name    [_d_] find-grep-dired      [_r_] recent files
[_p_] copy full path      [_g_] grep current         [_u_] sudo find file
[_P_] copy parent path    [_j_] jump to org heading  [_U_] sudo edit file
[_l_] get org headline    [_b_] browse current file  [_i_] insert org file
"
  ("n" my/put-buffer-name-on-clipboard)
  ("p" my/put-file-name-on-clipboard)
  ("P" my-buffer-path)
  ("d" find-grep-dired)
  ("g" consult-ripgrep)
  ("j" consult-org-heading)
  ("l" my/org-get-current-headline-link)
  ("r" recentf)
  ("u" sudo-edit-find-file)
  ("U" sudo-edit)
  ("b" browse-current-file)
  ("i" my/insert-org-file-link)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Notes hydra
(defhydra hydra-notes (:color blue :hint nil)
  "
^Notes^                   (C-c C-n to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
^org^                       ^denote^
^^^^^^^^^^--------------------------------------------------------
[_a_] org agenda            [_I_] denote link
[_c_] org capture           [_d_] denote create
[_l_] find org backlinks    [_f_] denote open
[_i_] insert org link       [_g_] denote grep
[_e_] org export
[_._] org emphasize
"
  ("l" my/org-backlink)
  ("i" my/insert-org-file-link)
  ("I" denote-link)
  ("a" org-agenda)
  ("f" denote-open-or-create)
  ("d" denote)
  ("g" denote-grep)
  ("e" org-export-dispatch)
  ("c" org-capture)
  ("." org-emphasize)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; LLM hydra
(defhydra hydra-llm (:color blue :hint nil)
  "
^LLM^                     (C-c C-l to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
^gptel^                 ^agent-shell^                ^custom^
^^^^^^^^^^--------------------------------------------------------
[_s_] gptel send        [_o_] agent-shell opencode   [_i_] simple ai shell
[_n_] gptel new buffer  [_d_] agent-shell send dwim  [_a_] aidermacs transient
[_m_] gptel menu
"
  ("s" gptel-send) ;; C-c RET is more convenient
  ("n" gptel)
  ("m" gptel-menu)
  ("a" aidermacs-transient-menu)
  ("i" my/ai-shell-command)
  ("o" agent-shell-opencode-start-agent)
  ("d" agent-shell-send-dwim)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Vandee hydra
(defhydra hydra-vandee (:color blue :hint nil)
  "
^Vandee^                  (C-c C-v to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_t_] vterm           [_h_] fold headings        [_v_] go to Vandee.org
[_g_] magit           [_T_] insert timestamp     [_j_] go to Journals.org
[_a_] agenda/TODO     [_o_] org
"
  ("g" magit)
  ("t" vt)
  ("a" hydra-vandee-agenda/body)
  ("o" hydra-org/body)
  ("T" my-insert-timestamp)
  ("h" my-org-show-current-heading-tidily)
  ("v" (find-file "~/Vandee/Areas/pkm/org/Vandee.org"))
  ("j" (find-file "~/Vandee/Areas/pkm/org/Journal.org"))
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Vandee Agenda/TODO sub-hydra
(defhydra hydra-vandee-agenda (:color blue :hint nil)
  "
^Agenda & TODO^
^^^^^^^^^^^^^^
[_t_] edit TODO state
[_i_] insert TODO heading
"
  ("t" org-todo)
  ("i" org-insert-todo-heading)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Buffers hydra
(defhydra hydra-buffers (:color blue :hint nil)
  "
^Buffers^                 (C-c C-b to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_i_] ibuffer           [_n_] next buffer       [_s_] save buffer
[_v_] view buffer       [_p_] previous buffer   [_S_] save some buffers
[_b_] switch to buffer  [_r_] revert buffer     [_d_] delete bookmark
[_c_] clone indirect    [_R_] rename buffer     [_D_] delete all bookmarks
[_C_] clone other win   [_k_] kill buffer       [_l_] list bookmarks
[_K_] kill all scratch  [_m_] set bookmark      [_j_] bookmark jump
"
  ("i" ibuffer)
  ("v" view-buffer)
  ("b" switch-to-buffer)
  ("c" clone-indirect-buffer)
  ("C" clone-indirect-buffer-other-window)
  ("k" kill-current-buffer)
  ("K" kill-all-buffers-except-scratch)
  ("n" next-buffer)
  ("p" previous-buffer)
  ("r" revert-buffer)
  ("R" rename-buffer)
  ("s" basic-save-buffer)
  ("S" save-some-buffers)
  ("d" bookmark-delete)
  ("D" bookmark-delete-all)
  ("l" list-bookmarks)
  ("m" bookmark-set)
  ("j" bookmark-jump)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Dired hydra
(defhydra hydra-dired (:color blue :hint nil)
  "
^Dired^                   (C-c C-d to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_d_] open dired          [_+_] create empty file   [_n_] copy filename
[_f_] dired find file     [_C_] dired do copy       [_p_] copy abs path
[_u_] dired up directory  [_j_] dired jump current  [_N_] neotree dir
[_w_] into wdired-mode    [_R_] dired move/rename
"
  ("d" dired)
  ("f" dired-x-find-file)
  ("u" dired-up-directory)
  ("+" dired-create-empty-file)
  ("C" dired-do-copy)
  ("j" dired-jump)
  ("n" dired-copy-filename-as-kill)
  ("p" my/dired-copy-absolute-path)
  ("N" neotree-dir)
  ("w" wdired-change-to-wdired-mode)
  ("R" dired-do-rename)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Eval/Eshell hydra
(defhydra hydra-eval (:color blue :hint nil)
  "
^Eval/Eshell^             (C-c C-e to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_b_] eval buffer         [_l_] eval last sexp    [_s_] eshell
[_d_] eval defun          [_r_] eval region       [_w_] eww
[_e_] eval expression     [_R_] eww reload        [_h_] eshell history
"
  ("b" eval-buffer)
  ("d" eval-defun)
  ("e" eval-expression)
  ("h" counsel-esh-history)
  ("l" eval-last-sexp)
  ("r" eval-region)
  ("R" eww-reload)
  ("s" eshell)
  ("w" eww)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Help hydra
(defhydra hydra-help (:color blue :hint nil)
  "
^Help^                    (C-c C-h to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_f_] describe function       [_v_] describe variable     [_c_] describe command
[_b_] describe bindings       [_k_] describe key          [_F_] describe face
[_I_] describe input method   [_L_] describe language env [_m_] describe mode
[_x_] describe command        [_a_] apropos               [_i_] info
[_l_] view lossage            [_w_] where is              [_r_] reload emacs
[_t_] load theme
"
  ("a" apropos)
  ("b" describe-bindings)
  ("c" describe-char)
  ("f" describe-function)
  ("F" describe-face)
  ("i" info)
  ("I" describe-input-method)
  ("k" describe-key)
  ("l" view-lossage)
  ("L" describe-language-environment)
  ("m" describe-mode)
  ("t" load-theme)
  ("v" describe-variable)
  ("w" where-is)
  ("x" describe-command)
  ("r" (lambda () (interactive)
         (load-file "~/.emacs.d/init.el")
         (message "Emacs configuration reloaded")))
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Open hydra
(defhydra hydra-open (:color blue :hint nil)
  "
^Open^                   (C-c C-o to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_f_] make frame         [_F_] select frame by name
"
  ("f" make-frame)
  ("F" select-frame-by-name)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Search hydra
(defhydra hydra-search (:color blue :hint nil)
  "
^Search^                  (C-c C-s to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_d_] search cwd         [_b_] search buffer      [_p_] consult ripgrep
[_D_] search other dir   [_g_] compile grep
"
  ("d" my/search-cwd)
  ("D" my/search-other-cwd)
  ("b" my/search-buffer)
  ("g" my/compile-grep-rn)
  ("p" sanityinc/consult-ripgrep-at-point)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Toggle hydra
(defhydra hydra-toggle (:color blue :hint nil)
  "
^Toggle^                  (C-c C-t to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_e_] eshell toggle      [_l_] line numbers      [_r_] rainbow mode
[_f_] flycheck mode      [_n_] neotree toggle    [_t_] visual line mode
[_o_] org mode
"
  ("e" eshell-toggle)
  ("f" flycheck-mode)
  ("l" display-line-numbers-mode)
  ("n" neotree-toggle)
  ("o" org-mode)
  ("r" rainbow-mode)
  ("t" visual-line-mode)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))


;; Projects hydra
(defhydra hydra-projects (:color blue :hint nil)
  "
^Projects^                 (C-c C-p to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
^Navigation^           ^Search^              ^Actions^
^^^^^^^^^^--------------------------------------------------------
[_s_] switch project    [_r_] riggrep         [_c_] cleanup
[_o_] open project      [_g_] grep            [_t_] test
[_f_] find file         [_p_] projectile ag   [_v_] version control
[_d_] find dir          [_i_] ibuffer         [_k_] kill buffers
[_a_] add project
"
  ;; Navigation
  ("s" my/projectile-switch-project-dired)
  ("o" my/projectile-switch-open-project-dired)
  ("f" projectile-find-file)
  ("d" projectile-find-dir)

  ;; Search
  ("r" projectile-ripgrep)
  ("g" projectile-grep)
  ("p" projectile-ag)
  ("i" projectile-ibuffer)

  ;; Actions
  ("a" projectile-add-known-project)
  ("c" projectile-cleanup-known-projects)
  ("C" projectile-compile-project)
  ("t" projectile-test-project)
  ("v" projectile-vc)
  ("k" projectile-kill-buffers)

  ;; Quit
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Windows hydra
(defhydra hydra-windows (:color blue :hint nil)
  "
^Windows^                  (C-c C-w to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
^Splits^              ^Motion^
^^^^^^^^^-----------------------------------
[_c_] close            [_h_] left
[_n_] new              [_j_] down
[_s_] horizontal split [_k_] up
[_v_] vertical split   [_l_] right
[_d_] delete others    [_w_] next window
"
  ;; Splits
  ("c" evil-window-delete)
  ("n" evil-window-new)
  ("s" evil-window-split)
  ("v" evil-window-vsplit)
  ("d" delete-other-windows)
  ;; Motion
  ("h" evil-window-left)
  ("j" evil-window-down)
  ("k" evil-window-up)
  ("l" evil-window-right)
  ("w" evil-window-next)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Mode-specific hydras

;; Org mode hydra
(defhydra hydra-org (:color blue :hint nil)
  "
^Org Mode^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_t_] toggle todo       [_a_] agenda            [_s_] subtree
[_T_] timestamp         [_l_] insert link       [_p_] priority
[_c_] capture           [_e_] export
"
  ("t" org-todo)
  ("T" org-time-stamp)
  ("c" org-capture)
  ("a" org-agenda)
  ("l" org-insert-link)
  ("s" org-cycle)
  ("e" org-export-dispatch)
  ("p" org-priority)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Code hydra for programming modes
(defun my/format-buffer-with-reformatter ()
  "Format buffer using appropriate reformatter based on major mode."
  (interactive)
  (let ((formatter
         (cond
          ((derived-mode-p 'python-mode 'python-ts-mode) 'ruff-format-buffer)
          ((derived-mode-p 'rust-mode 'rust-ts-mode) 'rustfmt-buffer)
          ((derived-mode-p 'typescript-mode 'js2-mode) 'biome-format-buffer)
          ((derived-mode-p 'terraform-mode) 'terraform-format-buffer)
          ((derived-mode-p 'purescript-mode) 'purty-buffer)
          ((derived-mode-p 'tuareg-mode 'ocaml-ts-mode) 'ocp-indent-buffer)
          ((derived-mode-p 'lua-mode) 'lua-format-buffer)
          ((derived-mode-p 'haskell-mode) 'ormolu-buffer)
          (t nil))))
    (if (and formatter (fboundp formatter))
        (progn
          (call-interactively formatter)
          (message "Formatted with %s" formatter))
      (message "No reformatter found for %s" major-mode))))

(defhydra hydra-code (:color blue :hint nil)
  "
^Code^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_c_] compile          [_h_] eldox-box-help    [_e_] execute src block
[_s_] shell command    [_a_] eglot actions     [_f_] format code
[_d_] debug            [_t_] test
"
  ("s" my-shell-command)
  ("c" compile)
  ("h" eldoc-box-help-at-point)
  ("f" (progn (my/format-buffer-with-reformatter))
   :color blue)
  ("d" gud-gdb)
  ("t" (progn (if (fboundp 'projectile-test-project)
                  (call-interactively 'projectile-test-project)
                (message "project-test not available")))
   :color blue)
  ("a" (progn (if (fboundp 'eglot-code-actions)
                  (call-interactively 'eglot-code-actions)
                (message "eglot not available")))
   :color blue)
  ("e" my-execute-src-block)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Magit hydra
(defhydra hydra-magit (:color blue :hint nil)
  "
^Magit^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_s_] status             [_d_] diff             [_r_] rebase
[_c_] commit             [_f_] fetch            [_m_] merge
[_P_] push               [_F_] pull             [_M_] remote
[_b_] branch             [_a_] commit amend     [_z_] stash
[_l_] log                [_A_] stash pop        [_Z_] stash drop
[_x_] file checkout      [_V_] revert
[_X_] reset
"
  ("s" magit-status)
  ("c" magit-commit)
  ("f" magit-fetch)
  ("F" magit-pull)
  ("P" magit-push)
  ("a" magit-commit-amend)
  ("A" magit-stash-pop)
  ("z" magit-stash)
  ("Z" magit-stash-drop)
  ("b" magit-branch)
  ("d" magit-diff)
  ("l" magit-log)
  ("r" magit-rebase)
  ("V" magit-revert)
  ("x" magit-file-checkout)
  ("X" magit-reset)
  ("m" magit-merge)
  ("M" magit-remote)
  ("e" magit-commit-extend)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

(provide 'init-hydra)
;;; init-hydra.el ends here
