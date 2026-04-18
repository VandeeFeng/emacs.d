;;; init-vibe.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; set .authinfo file path
(setq auth-sources '("~/.emacs.d/.authinfo"))

(require 'pi-coding-agent)
(defalias 'pi #'pi-coding-agent)

;; (use-package pi-coding-agent
;;   :ensure t
;;   :init (defalias 'pi 'pi-coding-agent))

;; ;; agent-shell
;; (use-package agent-shell
;;   :after evil
;;   :config
;;   (setq agent-shell-opencode-authentication
;;         (agent-shell-opencode-make-authentication :none t))
;;   ;; disable creating transcript file
;;   (setq agent-shell-transcript-file-path-function nil)
;;   ;; disable save prompt when killing buffer
;;   (setq shell-maker-prompt-before-killing-buffer nil)
;;   ;; (setq agent-shell-transcript-file-path-function
;;   ;;       (lambda ()
;;   ;;         (let* ((dir (expand-file-name "~/.agent-shell/transcripts/"))
;;   ;;                (filename (format-time-string "%F-%H-%M-%S.md")))
;;   ;;           (expand-file-name filename dir))))

;;   (setq agent-shell-pi-environment
;;         (agent-shell-make-environment-variables
;;          "MINIMAX_CN_API_KEY" (auth-source-pass-get 'secret "minimax-cn")
;;          "PI_CODING_AGENT_DIR" "~/.pi/agent"
;;          :inherit-env t))

;;   ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
;;   (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
;;   (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)
;;   (evil-define-key 'normal agent-shell-mode-map (kbd "@") #'agent-shell-set-session-mode)
;;   (define-key agent-shell-mode-map (kbd "C-c C-k") 'agent-shell-interrupt)

;;   ;; Configure *agent-shell-diff* buffers to start in Emacs state
;;   (add-hook 'diff-mode-hook
;;             (lambda ()
;;               (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
;;                 (evil-emacs-state)))))

;; aidermacs
(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  ;; (setenv "GEMINI_API_KEY" (my/get-gemini-api-key))
  ;; (setq aidermacs-default-model "gemini/gemini-2.0-flash")
  (setq aidermacs-backend 'vterm)
  ;;(setq aidermacs-use-architect-mode t)
  :custom
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "gemini/gemini-2.5-flash"))

(add-hook 'aidermacs-before-run-backend-hook
          (lambda ()
            (let ((api-key (my/get-gemini-api-key)))
              (if (and api-key (stringp api-key) (not (string-empty-p api-key)))
                  (setenv "GEMINI_API_KEY" api-key)
                (error "Failed to set GEMINI_API_KEY: Invalid or missing API key")))))

(defun my/get-gemini-api-key ()
  "Retrieve Gemini API key from auth-source."
  (let* ((auth-info (car (auth-source-search :host "generativelanguage.googleapis.com")))
         (api-key (and auth-info (plist-get auth-info :secret))))
    (cond
     ((and api-key (functionp api-key)) (funcall api-key)) ; Handle case where secret is a function
     ((and (stringp api-key) (not (string-empty-p api-key))) api-key) ; Return valid non-empty string
     (t (progn
          (message "No valid Gemini API key found in auth-source")
          nil)))))
;;; aidermacs ends

;; ----------------------------------------------------------
;; 自动补全
;; ----------------------------------------------------------

;; https://emacs-china.org/t/deepseek-claude-gemini-ollama-minuet-ai-el/28715
;; https://github.com/milanglacier/minuet-ai.el
(use-package minuet
  :ensure t
  :bind
  (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  ;; (add-to-list 'completion-at-point-functions #'minuet-auto-suggestion-mode)

  :config
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
  ;; I recommend beginning with a small context window size and incrementally
  ;; expanding it, depending on your local computing power. A context window
  ;; of 512, serves as an good starting point to estimate your computing
  ;; power. Once you have a reliable estimate of your local computing power,
  ;; you should adjust the context window to a larger value.
  (setq minuet-context-window 512)

  ;; ollama
  ;; (plist-put minuet-openai-fim-compatible-options :end-point  "http://localhost:11434/v1/completions")
  ;; ;; an arbitrary non-null environment variable as placeholder
  ;; (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  ;; (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  ;; (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:7b")

  ;; llama.cpp
  ;; llama-server -m ~/Models/sweepai_sweep-next-edit-1.5B_sweep-next-edit-1.5b.q8_0.v2.gguf
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:8080/v1/completions")
  ;; an arbitrary non-null environment variable as placeholder
  (plist-put minuet-openai-fim-compatible-options :name "Llama.cpp")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  ;; The model is set by the llama-cpp server and cannot be altered
  ;; post-launch.
  (plist-put minuet-openai-fim-compatible-options :model "PLACEHOLDER")

  ;; Llama.cpp does not support the `suffix` option in FIM completion.
  ;; Therefore, we must disable it and manually populate the special
  ;; tokens required for FIM completion.
  (minuet-set-nested-plist minuet-openai-fim-compatible-options nil :template :suffix)
  (minuet-set-optional-options
   minuet-openai-fim-compatible-options
   :prompt
   (defun minuet-llama-cpp-fim-qwen-prompt-function (ctx)
     (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
             (plist-get ctx :language-and-tab)
             (plist-get ctx :before-cursor)
             (plist-get ctx :after-cursor)))
   :template)

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

(with-eval-after-load 'minuet
  (let* ((auth-info (car (auth-source-search :host "generativelanguage.googleapis.com")))
         (api-key (and auth-info (plist-get auth-info :secret)))) ; 使用 :secret 获取 API key
    (if api-key
        (plist-put minuet-gemini-options :api-key api-key )
      (plist-put minuet-gemini-options :model "gemini-2.5-flash")))
  )


(provide 'init-vibe)
;;; init-vibe.el ends here
