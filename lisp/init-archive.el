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

;;; init-archive.el ends here
