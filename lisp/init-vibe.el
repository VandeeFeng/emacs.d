;;; init-vibe.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; set .authinfo file path
(setq auth-sources '("~/.emacs.d/.authinfo"))

;; agent-shell
;; (use-package agent-shell
;;   :config
;;   (setq agent-shell-opencode-authentication
;;         (agent-shell-opencode-make-authentication :none t))
;;   ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
;;   (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
;;   (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

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
;; gptel
;; ----------------------------------------------------------

;; gptel 设置默认模型
(use-package gptel
  :defer t
  :config
  (let* ((auth-info (car (auth-source-search :host "generativelanguage.googleapis.com")))
         (api-key (and auth-info (plist-get auth-info :secret)))) ; 使用 :secret 获取 API key
    (if api-key
        (setq
         ;; gptel-model 'gemini-2.5-flash
         gptel-backend (gptel-make-gemini "Gemini"
                         :stream t
                         :key api-key
                         :models '("gemini-2.5-flash"))
         )
      (error "未在 auth-source 中找到 Gemini API 密钥！请检查您的 auth-source 配置。")))

  ;; gptel-model "qwen2.5"
  ;; gptel-backend (gptel-make-ollama "Ollama"
  ;;                 :host "localhost:11434"
  ;;                 :stream t
  ;;                 :models '("qwen2.5:14b")))

  (gptel-make-ollama "Ollama"           ;Any name of your choosing
    :host "localhost:11434"             ;Where it's running
    :stream t                           ;Stream responses
    :models '("qwen3:1.7b"))            ;List of models

  (gptel-make-ollama "Deepseek"         ;Any name of your choosing
    :host "localhost:11434"             ;Where it's running
    :stream t                           ;Stream responses
    :models '("deepseek-r1:14b"))       ;List of models

  ;; preset
  ;; https://github.com/karthink/gptel/?tab=readme-ov-file#option-presets
  (gptel-make-preset 'explain
    :system "Explain what this code does to a novice programmer.")

  ;; https://github.com/karthink/gptel/issues/514
  (gptel-make-tool
   :function (lambda (url)
               (let* ((proxy-url (concat "https://r.jina.ai/" url))
                      (buffer (url-retrieve-synchronously proxy-url)))
                 (with-current-buffer buffer
                   (goto-char (point-min)) (forward-paragraph)
                   (let ((dom (libxml-parse-html-region (point) (point-max))))
                     (run-at-time 0 nil #'kill-buffer (current-buffer))
                     (with-temp-buffer
                       (shr-insert-document dom)
                       (buffer-substring-no-properties (point-min) (point-max)))))))
   :name "read_url"
   :description "Fetch and read the contents of a URL using Jina.ai reader"
   :args (list '(:name "url"
                       :type "string"
                       :description "The URL to read"))
   :category "web")
  ;; (gptel-make-tool
  ;;  :function (lambda (url)
  ;;              (with-current-buffer (url-retrieve-synchronously url)
  ;;                (goto-char (point-min)) (forward-paragraph)
  ;;                (let ((dom (libxml-parse-html-region (point) (point-max))))
  ;;                  (run-at-time 0 nil #'kill-buffer (current-buffer))
  ;;                  (with-temp-buffer
  ;;                    (shr-insert-document dom)
  ;;                    (buffer-substring-no-properties (point-min) (point-max))))))
  ;;  :name "read_url"
  ;;  :description "Fetch and read the contents of a URL"
  ;;  :args (list '(:name "url"
  ;;                      :type "string"
  ;;                      :description "The URL to read"))
  ;;  :category "web")

  (gptel-make-tool
   :function (lambda (buffer text)
               (with-current-buffer (get-buffer-create buffer)
                 (save-excursion
                   (goto-char (point-max))
                   (insert text)))
               (format "Appended text to buffer %s" buffer))
   :name "append_to_buffer"
   :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
   :args (list '(:name "buffer"
                       :type "string"
                       :description "The name of the buffer to append text to.")
               '(:name "text"
                       :type "string"
                       :description "The text to append to the buffer."))
   :category "emacs")

  ;; Message buffer logging tool
  (gptel-make-tool
   :function (lambda (text)
               (message "%s" text)
               (format "Message sent: %s" text))
   :name "echo_message"
   :description "Send a message to the *Messages* buffer"
   :args (list '(:name "text"
                       :type "string"
                       :description "The text to send to the messages buffer"))
   :category "emacs")

  ;; buffer retrieval tool
  (gptel-make-tool
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
                 (error "Error: buffer %s is not live." buffer))
               (with-current-buffer  buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer"
   :args (list '(:name "buffer"
                       :type "string"
                       :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")


  (gptel-make-tool
   :function (lambda (directory)
               (mapconcat #'identity
                          (directory-files directory)
                          "\n"))
   :name "list_directory"
   :description "List the contents of a given directory"
   :args (list '(:name "directory"
                       :type "string"
                       :description "The path to the directory to list"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (parent name)
               (condition-case nil
                   (progn
                     (make-directory (expand-file-name name parent) t)
                     (format "Directory %s created/verified in %s" name parent))
                 (error (format "Error creating directory %s in %s" name parent))))
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :args (list '(:name "parent"
                       :type "string"
                       :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                       :type "string"
                       :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
                       :type "string"
                       :description "The directory where to create the file")
               '(:name "filename"
                       :type "string"
                       :description "The name of the file to create")
               '(:name "content"
                       :type "string"
                       :description "The content to write to the file"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (filepath)
               (with-temp-buffer
                 (insert-file-contents (expand-file-name filepath))
                 (buffer-string)))
   :name "read_file"
   :description "Read and display the contents of a file"
   :args (list '(:name "filepath"
                       :type "string"
                       :description "Path to the file to read.  Supports relative paths and ~."))
   :category "filesystem")

  )

;; gtpel + org-protocol
(require 'org-protocol)
(require 'gptel)

(require 'server)
(unless (server-running-p)
  (server-start))

;; Handler for gptel queries from browser
(defun my/gptel-org-protocol-handler (info)
  "Handle gptel query from org-protocol.
INFO is the data passed by org-protocol."
  (let ((text (plist-get info :text)))
    (when (and text (not (string-empty-p text)))
      (let ((query (decode-coding-string (url-unhex-string text) 'utf-8)))
        (message "Received gptel query: %s" query)
        ;; Create or switch to gptel buffer
        (let ((buffer (get-buffer-create "*gptel-browser*")))
          (with-current-buffer buffer
            (unless (eq major-mode 'gptel-default-mode)
              (funcall gptel-default-mode))
            (gptel-mode 1)
            (goto-char (point-max))
            (insert "\n\n--- From Browser ---\n")
            (insert query)
            (insert "\n")
            (goto-char (point-max))
            ;; Send the query to gptel
            (gptel-send)
            (display-buffer buffer)))))))

;; Register the protocol handler
(setq org-protocol-protocol-alist
      (append org-protocol-protocol-alist
              '(("gptel-browser"
                 :protocol "gptel"
                 :function my/gptel-org-protocol-handler))))

;; JavaScript bookmarklet for browser (copy this as bookmark URL):
;; Basic version:
;; javascript:(function(){const selectedText=window.getSelection().toString().trim();if(!selectedText){alert('Please select some text first');return;}const pageTitle=document.title;const pageUrl=window.location.href;const fullText=`From: ${pageTitle}\nURL: ${pageUrl}\n\nSelected text:\n${selectedText}`;const encodedText=encodeURIComponent(fullText);const protocolUrl=`org-protocol://gptel?text=${encodedText}`;window.location.href=protocolUrl;})();

;; Enhanced version with prompt for question:
;; javascript:(function(){const selectedText=window.getSelection().toString().trim();if(!selectedText){alert('Please select some text first');return;}const pageTitle=document.title;const pageUrl=window.location.href;const userQuestion=prompt('Optional: Add a question or context:');let fullText=`From: ${pageTitle}\nURL: ${pageUrl}\n\n`;if(userQuestion){fullText+=`Question: ${userQuestion}\n\n`;}fullText+=`Selected text:\n${selectedText}`;const encodedText=encodeURIComponent(fullText);const protocolUrl=`org-protocol://gptel?text=${encodedText}`;window.location.href=protocolUrl;})();

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
  (plist-put minuet-openai-fim-compatible-options :end-point  "http://localhost:11434/v1/completions")
  ;; an arbitrary non-null environment variable as placeholder
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:7b")

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
