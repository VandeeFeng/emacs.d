;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/rexim/gruber-darker-theme 很简洁暗色
;; https://github.com/credmp/arjen-grey-theme 灰色
;; https://github.com/mswift42/white-sand-theme 护眼
;; https://github.com/sashimacs/os1-theme 护眼
;; https://github.com/Lokaltog/distinguished-theme 黑色
;; https://github.com/gchp/flatland-emacs 还可以
;; https://github.com/jmdeldin/ir-black-theme.el 黑色
;; https://github.com/thblt/eziam-theme-emacs
;; https://github.com/emacsfodder/emacs-theme-creamsody
;; https://github.com/caffo/monotropic-theme 护眼
;; https://github.com/kunalb/poet 护眼
;; https://github.com/erikbackman/mindre-theme 白色
;; https://github.com/cryon/almost-mono-themes 也还不错
;; https://github.com/motform/stimmung-themes 可定制自由度高
;; https://github.com/mclear-tools/bespoke-themes
;; https://github.com/emacsfodder/emacs-theme-darktooth
;; https://github.com/jordonbiondo/ample-theme
;; https://emacs-china.org/t/topic/18556/11
;; https://emacsthemes.com/popular/index.html
;; https://github.com/mrunhap/nano-theme.el
;; https://github.com/rougier/nano-theme
;; (straight-use-package '(nano-theme :type git :host github
;; :repo "rougier/nano-theme"))


;;(load-theme 'ir-black t)
;;(set-face-background 'default "#252525")
;;(set-face-background 'mode-line 'unspecified)  ;; 禁用 mode-line 背景颜色
;;(set-face-background 'vertical-border "#282828")


;; https://github.com/rougier/nano-modeline/blob/master/nano-modeline.el
;;(require 'nano-modeline)
;;(add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;;(add-hook 'text-mode-hook            #'nano-modeline-text-mode)
;;(add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;;(setq nano-modeline-position #'nano-modeline-footer)

;; modeline
(use-package minions
  :ensure t
  :config (minions-mode 1))

;; moody,modeline 边框的设置在 org 美化里
(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  )

;; theme
(require 'gruber-darker-theme)
(load-theme 'gruber-darker t)
(use-package almost-mono-themes
  :ensure t
  :config
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)
  )

;;---------------------------------------------------------------------
;;
;; sustom-set-face
;;
;;---------------------------------------------------------------------
;; M-x describe-face 可以查看颜色
;; https://github.com/xenodium/dotsies/blob/main/emacs/features/fe-ui.el

;; 直接在 custom.el 里设置了，不要每次都重复载入
(custom-set-faces
 ;; globle 部分-------------------------------------------
 '(shadow ((t (:foreground "gray70"))))
 '(show-paren-match ((t (:background "SteelBlue3" :foreground "gray90"))))
 ;; 设置默认字体和大小
 '(default ((t (:family "Source Code Pro" :height 140 :weight light :background "#1D1F21" :foreground "#d1d5db"))))
 '(cursor ((t (:background "orange"))))

 '(link ((t (:foreground "#96a6c8" :underline t))))
 '(highlight ((t (:background "gray35"))))

 ;; 设置注释的样式
 '(font-lock-comment-face ((t (:foreground "#787878"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#787878"))))
 ;; 设置字符串的样式
 ;; '(font-lock-string-face ((t (:weight normal :foreground "#96D0FF"))))

 ;; 设置常量的样式
 ;; '(font-lock-constant-face ((t (:weight normal :foreground "95a99f"))))

 ;; 设置内置函数的样式
 '(font-lock-builtin-face ((t (:weight normal :foreground "#79C0FF"))))

 ;; 设置关键字的样式
 '(font-lock-keyword-face ((t (:weight normal :foreground "#FFA657")))) ;#FFA657-橙色 ,#FF7B72

 ;; 设置函数名的样式
 ;; '(font-lock-function-name-face ((t (:weight normal :foreground "#96a6c8"))))

 ;; 设置变量名的样式
 ;; '(font-lock-variable-name-face ((t (:weight normal :foreground "#c5b49f")))) ;#c5b49f-浅咖，#bc9575-焦糖橙 备用

 ;; 设置类型的样式
 ;; '(font-lock-type-face ((t (:foreground "#a6adac" :slant normal :weight normal)))) ; #a6adac ,#B6B9AE 很浅的灰绿备用

 ;; 设置文档字符串的样式
 '(font-lock-doc-face ((t (:weight normal :foreground "#787878"))))

 ;; 设置当前行号颜色
 '(line-number-current-line ((t (:foreground "#ffdd33"))))

 ;; org 部分 --------------------------------------------------------
 ;; 代码块背景和边框
 ;; '(org-block-begin-line
 ;; ((t (:background "#343942" :foreground "#7F8490" :extend t))))
 '(org-level-1 ((t (:height 1.4 :weight normal))))
 '(org-level-2 ((t (:height 1.3 :weight normal))))
 '(org-level-3 ((t (:height 1.2 :weight normal))))
 '(org-level-4 ((t (:height 1.1 :weight normal))))
 '(org-level-5 ((t (:height 1.05 :weight normal))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.05 :weight normal))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0 :weight normal))))
 '(org-level-8 ((t (:inherit outline-8 :height 1.0 :weight normal))))

 ;; 设置文档标题 (#+TITLE:)
 '(org-document-title ((t (:inherit default :weight bold
                                    :height 1.5 ; 文档标题字体大小
                                    :underline nil ; 添加下划线
                                    ))))           ; 标题颜色

 ;; 设置特殊关键字 (#+STARTUP: 等)
 '(org-meta-line ((t (:inherit font-lock-comment-face
                               :height 1.1 ; 关键字字体大小
                               ;; :slant italic     ; 斜体
                               ))))

 ;; 设置时间戳颜色
 '(org-date ((t (:foreground "#61AFEF" :background nil :weight normal))))

 ;; 设置 org-tags 的颜色
 '(org-tag ((t (:foreground "#8B949E" :weight normal :height 0.9 :inherit nil :slant normal))))

 ;; 设置 org-block-begin-line 的样式
 '(org-block-begin-line ((t (:background "#252525" :foreground "#757575" :extend t :italic t))))

 ;; 设置 org-block 的样式
 '(org-block ((t (:background "#252525" :extend t))))

 ;; 设置 org-block-end-line 的样式
 '(org-block-end-line ((t (:background "#252525" :foreground "#757575" :extend t :italic t))))

 ;; 设置 org-code 的样式
 '(org-code ((t (:foreground "#da8548" :weight normal))))
 ;; 设置 mode-line
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil))))
 ;; org 部分 ends----------------------------------------------------

 ;; 插件部分 --------------------------------------------------------
 '(diff-hl-change ((t (:background nil :foreground "#ffc125"))))
 '(diff-hl-delete ((t (:background nil :foreground "#ff3030"))))
 '(diff-hl-insert ((t (:background nil :foreground "green1"))))
 '(diredfl-date-time ((t nil)))
 '(diredfl-deletion ((t nil)))
 '(diredfl-dir-heading ((t (:foreground "#95a99f"))))
 '(diredfl-dir-name ((t (:foreground "#79C0FF"))))
 '(diredfl-exec-priv ((t nil)))
 '(diredfl-file-name ((t nil)))
 '(diredfl-file-suffix ((t nil)))
 '(diredfl-no-priv ((t nil)))
 '(diredfl-number ((t nil)))
 '(diredfl-other-priv ((t nil)))
 '(diredfl-rare-priv ((t nil)))
 '(diredfl-read-priv ((t nil)))
 '(diredfl-write-priv ((t nil)))
 '(which-key-command-description-face ((t (:foreground "#79C0FF"))))
 '(which-key-group-description-face ((t (:foreground "#79C0FF" :weight medium))))
 '(orderless-match-face-1 ((t (:foreground "#79C0FF"))))
 '(vertico-current ((t (:extend nil :background "gray40" :foreground "gray100"))))
 '(marginalia-key ((t (:foreground "#79C0FF"))))
 '(corfu-default ((t (:background "gray15" :foreground "gray80"))))
 '(anzu-mode-line ((t (:foreground "#79C0FF" :weight bold))))
 '(anzu-mode-line-no-match ((t (:foreground "#79C0FF"))))
 '(company-preview ((t (:background "gray35"))))
 )

(with-eval-after-load 'org
  ;; 启用原生语法高亮
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  ;; 设置行内make up，直接显示*粗体*，/斜体/，=高亮=，~代码~
  (setq org-hide-emphasis-markers t)
  )

;; (with-eval-after-load 'faces
;;   (set-face-attribute 'default nil :background "#181818")
;;   (set-cursor-color "orange") ; orange,gold1
;;   ;; 设置默认字体和大小
;;   (set-face-attribute 'default nil
;;                       :family "Source Code Pro" ;Source Code Pro ,FiraCode Nerd Font
;;                       :height 140               ; 基础字体大小(pt)
;;                       :weight 'light
;;                       :foreground "#d1d5db")
;;   https://www.jyshare.com/front-end/6214/#d1d5db
;;   #F5F5f5 ,#b2b5ba 15% ,#bcc0c5 10% ，#c7cad0 5% 加灰黑《 #d1d5db 原始灰色 》加白 #f6f7f8 80% #f1f2f4 70%，#edeef1 60% ,#e8eaed 50% ,#e3e6e9 40% ,#dfe2e6 30% #dadde2 20% ,#d6d9df 10% ,#d3d7dd 5%
;;   ;; 设置 mode-line
;;   (set-face-attribute 'mode-line nil :box nil)
;;   (set-face-attribute 'mode-line-inactive nil :box nil)

;;   ;; 设置注释的样式
;;   (set-face-attribute 'font-lock-comment-face nil :foreground "#787878")

;;   ;; 设置字符串的样式
;;   ;; (set-face-attribute 'font-lock-string-face nil :weight 'normal :foreground "#96D0FF")

;;   ;; 设置常量的样式
;;   (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
;;   ;; (set-face-attribute 'font-lock-constant-face nil :weight 'normal :foreground "#79C0FF")

;;   ;; 设置内置函数的样式
;;   (set-face-attribute 'font-lock-builtin-face nil :weight 'normal :foreground "#79C0FF")

;;   ;; 设置关键字的样式
;;   (set-face-attribute 'font-lock-keyword-face nil :weight 'normal :foreground "#FF7B72")

;;   ;; 设置函数名的样式
;;   (set-face-attribute 'font-lock-function-name-face nil :weight 'normal :foreground "#D2A8FF")

;;   ;; 设置变量名的样式
;;   (set-face-attribute 'font-lock-variable-name-face nil :weight 'normal :foreground "#FFA657")

;;   ;; 设置类型的样式
;;   (set-face-attribute 'font-lock-type-face nil :weight 'normal :foreground "#FF7B72") ;深红色

;;   ;; 设置文档字符串的样式
;;   (set-face-attribute 'font-lock-doc-face nil :weight 'normal :foreground "#787878")

;;   ;; 设置当前行号颜色
;;   (set-face-foreground 'line-number-current-line "gold1")

;;   )

;; (defun my-org-face-settings ()
;;   (interactive)
;;   (custom-set-faces
;;    ;; 设置默认字体和大小
;;    '(default ((t (:family "Source Code Pro" :height 140 :weight light :foreground "#d1d5db"))))
;;    ;; 代码块背景和边框
;;    ;; '(org-block-begin-line
;;    ;; ((t (:background "#343942" :foreground "#7F8490" :extend t))))
;;    '(org-level-1 ((t (:height 1.4 :weight normal))))
;;    '(org-level-2 ((t (:height 1.3 :weight normal))))
;;    '(org-level-3 ((t (:height 1.2 :weight normal))))
;;    '(org-level-4 ((t (:height 1.1 :weight normal))))
;;    '(org-level-5 ((t (:height 1.05 :weight normal))))
;;    '(org-level-6 ((t (:inherit outline-6 :height 1.05 :weight normal))))
;;    '(org-level-7 ((t (:inherit outline-7 :height 1.0 :weight normal))))
;;    '(org-level-8 ((t (:inherit outline-8 :height 1.0 :weight normal))))

;;    ;; 设置文档标题 (#+TITLE:)
;;    '(org-document-title ((t (:inherit default :weight bold
;;                                       :height 1.5 ; 文档标题字体大小
;;                                       :underline nil ; 添加下划线
;;                                       ))))           ; 标题颜色

;;    ;; 设置特殊关键字 (#+STARTUP: 等)
;;    '(org-meta-line ((t (:inherit font-lock-comment-face
;;                                  :height 1.1 ; 关键字字体大小
;;                                  ;; :slant italic     ; 斜体
;;                                  )))))

;;   )

;; ;; 在初始化时应用设置
;; (add-hook 'after-init-hook #'my-org-face-settings)
;; ;; 为新 frame 开启默认 org 美化设置
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (with-selected-frame frame
;;               (my-org-face-settings))))


;; 设置非窗口内的褪色效果
(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)))

(provide 'init-themes)
;;; init-themes.el ends here
