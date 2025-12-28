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
[_n_] copy buffer name    [_d_] find-grep-dired  [_r_] recent files
[_p_] copy full path      [_g_] grep current     [_u_] sudo find file
[_P_] copy parent path    [_j_] jump to file     [_U_] sudo edit file
[_l_] get org headline
"
  ("n" my/put-buffer-name-on-clipboard)
  ("p" my/put-file-name-on-clipboard)
  ("P" my-buffer-path)
  ("d" find-grep-dired)
  ("g" counsel-grep-or-swiper)
  ("j" counsel-file-jump)
  ("l" my/org-get-current-headline-link)
  ("r" recentf)
  ("u" sudo-edit-find-file)
  ("U" sudo-edit)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Notes hydra
(defhydra hydra-notes (:color blue :hint nil)
  "
^Notes^                   (C-c C-n to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_l_] find org backlinks   [_a_] org agenda         [_e_] org export
[_i_] insert org link      [_f_] denote open        [_c_] org capture
[_I_] denote link          [_d_] denote create      [_._] org emphasize
"
  ("l" my/org-backlink)
  ("i" my/insert-org-file-link)
  ("I" denote-link)
  ("a" org-agenda)
  ("f" denote-open-or-create)
  ("d" denote)
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
[_s_] gptel send           [_c_] simple claude
[_n_] gptel new buffer     [_a_] aidermacs transient
[_m_] gptel menu
"
  ("s" gptel-send) ;; C-c RET is more convenient
  ("n" gptel)
  ("m" gptel-menu)
  ("a" aidermacs-transient-menu)
  ("c" my/claude-shell-command)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Vandee hydra
(defhydra hydra-vandee (:color blue :hint nil)
  "
^Vandee^                  (C-c C-v to open)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_e_] execute src block    [_t_] vterm             [_v_] go to Vandee.org
[_g_] magit                [_a_] agenda/TODO       [_j_] go to Journals.org
[_T_] insert timestamp     [_h_] fold headings     [_s_] shell command
"
  ("e" my-execute-src-block)
  ("g" magit)
  ("t" vt)
  ("a" hydra-vandee-agenda/body)
  ("T" my-insert-timestamp)
  ("h" my-org-show-current-heading-tidily)
  ("s" my-shell-command)
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
[_d_] open dired          [_c_] create empty file   [_n_] copy filename
[_f_] dired find file     [_C_] create directory    [_p_] copy abs path
[_u_] dired up directory  [_j_] dired jump current  [_N_] neotree dir
[_r_] toggle read only    [_R_] dired move/rename
"
  ("d" dired)
  ("f" dired-x-find-file)
  ("u" dired-up-directory)
  ("c" dired-create-empty-file)
  ("C" dired-create-directory)
  ("j" dired-jump)
  ("n" dired-copy-filename-as-kill)
  ("p" my/dired-copy-absolute-path)
  ("N" neotree-dir)
  ("r" dired-toggle-read-only)
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
^Navigation^           ^Search^            ^Actions^
^^^^^^^^^^--------------------------------------------------------
[_f_] find file         [_r_] riggrep        [_c_] compile
[_s_] switch project    [_g_] grep           [_t_] test
[_d_] find dir          [_p_] projectile ag  [_v_] version control
[_o_] open project      [_i_] ibuffer        [_k_] kill buffers
[_a_] add project
"
  ;; Navigation
  ("f" projectile-find-file)
  ("s" projectile-switch-project)
  ("d" projectile-find-dir)
  ("o" projectile-switch-open-project)
  ("a" projectile-add-known-project)

  ;; Search
  ("r" projectile-ripgrep)
  ("g" projectile-grep)
  ("p" projectile-ag)
  ("i" projectile-ibuffer)

  ;; Actions
  ("c" projectile-compile-project)
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
(defhydra hydra-code (:color blue :hint nil)
  "
^Code^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_c_] compile            [_h_] eldox-box       [_g_] grep
[_e_] eval buffer        [_d_] debug           [_s_] eglot actions
[_r_] eval region        [_t_] test            [_f_] format code
[_l_] comment line
"
  ("c" compile)
  ("h" eldoc-box-help-at-point)
  ("e" eval-buffer)
  ("r" eval-region)
  ("f" (progn (if (fboundp 'format-all-buffer)
                  (format-all-buffer)
                (message "format-all not available"))
              (hydra-code/body))
   :exit nil)
  ("g" rgrep)
  ("d" gud-gdb)
  ("t" (progn (if (fboundp 'projectile-test-project)
                  (call-interactively 'projectile-test-project)
                (message "project-test not available"))
              (hydra-code/body))
   :exit nil)
  ("l" comment-line)
  ("s" (progn (if (fboundp 'eglot-code-actions)
                  (call-interactively 'eglot-code-actions)
                (message "eglot not available"))
              (hydra-code/body))
   :exit nil)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

;; Magit hydra
(defhydra hydra-magit (:color blue :hint nil)
  "
^Magit^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[_s_] status             [_d_] diff             [_r_] rebase
[_c_] commit             [_l_] log              [_m_] merge
[_P_] push               [_p_] pull             [_b_] branch
"
  ("s" magit-status)
  ("c" magit-commit)
  ("P" magit-push)
  ("p" magit-pull)
  ("b" magit-branch)
  ("d" magit-diff)
  ("l" magit-log)
  ("r" magit-rebase)
  ("m" magit-merge)
  ("q" nil "quit")
  ("C-g" nil "quit")
  ("<escape>" nil "quit"))

(provide 'init-hydra)
;;; init-hydra.el ends here
