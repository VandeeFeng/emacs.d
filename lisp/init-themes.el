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


;;==============================
;; theme
;;==============================
(require 'gruber-darker-theme)
(load-theme 'gruber-darker t)

;; (use-package almost-mono-themes
;;   :ensure t
;;   :config
;;   ;; (load-theme 'almost-mono-black t)
;;   (load-theme 'almost-mono-gray t)
;;   ;; (load-theme 'almost-mono-cream t)
;;   ;; (load-theme 'almost-mono-white t)
;;   )

;;(load-theme 'ir-black t)
;;(set-face-background 'default "#252525")
;;(set-face-background 'mode-line 'unspecified)  ;; 禁用 mode-line 背景颜色
;;(set-face-background 'vertical-border "#282828")


;;==============================
;; icon
;;==============================
;; https://github.com/domtronn/all-the-icons.el

;; nerd-icons-completion
;; https://github.com/rainstormstudio/nerd-icons-completion
(require-package 'nerd-icons-corfu)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; kind-icon
;; https://github.com/jdtsmith/kind-icon
;; (use-package kind-icon
;;   :ensure t
;;   :config
;;   (add-hook 'my-completion-ui-mode-hook
;;             (lambda ()
;;               (setq completion-in-region-function
;;                     (kind-icon-enhance-completion
;;                      completion-in-region-function)))))

;;==============================
;; modeline
;;==============================
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

;; modeline 里的彩虹猫！
;; mac 不知道为什么会造成滚动卡顿 linux 里没问题
(use-package nyan-mode
  :ensure t
  :defer 1
  :config
  (setq nyan-bar-length 16)             ;设定彩虹猫的长度
  (setq nyan-minimum-window-width 12)
  ;;(setq nyan-animate-nyancat t) ; 开启动画 nil 关闭
  (nyan-mode 1))

;; https://github.com/rougier/nano-modeline/blob/master/nano-modeline.el
;;(require 'nano-modeline)
;;(add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;;(add-hook 'text-mode-hook            #'nano-modeline-text-mode)
;;(add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;;(setq nano-modeline-position #'nano-modeline-footer)


;;==============================
;; sustom-set-face
;;==============================

;; M-x describe-face 可以查看颜色
;; https://github.com/xenodium/dotsies/blob/main/emacs/features/fe-ui.el

;; 直接在 custom.el 里设置了，不要每次都重复载入
;; (set-fontset-font t 'han (font-spec :family "LXGW WenKai Mono" :height 140)) ;;单独设置Emacsclient的字体不会变

;; 自定义fontset
(create-fontset-from-fontset-spec
 "-*-monospace-normal-r-normal--14-*-*-*-c-*-fontset-myfontset")

(set-fontset-font "fontset-myfontset" 'han (font-spec :family "LXGW WenKai Mono"))

;; Iosevka 字体更窄，可以显示更多代码，作为书面字体和系统字体感觉不太合适，也是因为窄
(set-fontset-font "fontset-myfontset" 'ascii (font-spec :family "Iosevka")) ;; Iosevka , GeistMono Nerd Font

(custom-set-faces
 ;; global 部分-------------------------------------------
 '(match ((t (:background unspecified :foreground "#79C0FF"))))
 '(popup-tip-face ((t (:background "#1D1F21" :foreground "#d1d5db"))))
 '(shadow ((t (:foreground "gray70"))))
 '(show-paren-match ((t (:background "SteelBlue3" :foreground "gray90"))))
 '(show-paren-mismatch ((t (:background "HotPink3" :foreground "white"))))

 ;; 设置默认字体和大小
 ;; '(default ((t (:family "GeistMono Nerd Font" :height 140 :weight light :background "#1D1F21" :foreground "#d1d5db"))))
 '(default ((t (:font "fontset-myfontset" :height 140 :background "#1D1F21" :foreground "#d1d5db"))))
 '(cursor ((t (:background "orange"))))

 '(link ((t (:foreground "#96a6c8" :underline t))))
 '(highlight ((t (:background "#5A5F66" :foreground unspecified)))) ;; #60656C

 ;; 设置注释的样式
 '(font-lock-comment-face ((t (:foreground "#787878"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#787878"))))
 ;; 设置字符串的样式
 '(font-lock-string-face ((t (:foreground "#A1D08E")))) ;#A1D08E,#A7D08A #73c936 #96D0FF

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
 ;; '(font-lock-type-face ((t (:foreground "#9DA3A2" :slant normal :weight normal)))) ; #a6adac ,#B6B9AE 很浅的灰绿备用

 ;; 设置文档字符串的样式
 '(font-lock-doc-face ((t (:weight normal :foreground "#787878"))))

 ;; 设置当前行号颜色
 '(line-number-current-line ((t (:foreground "#ffdd33"))))

 ;; 设置行号颜色
 '(line-number ((t (:foreground "gray30"))))

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
 '(org-date ((t (:foreground "#61AFEF" :background unspecified :weight normal))))

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
 '(diff-hl-change ((t (:background unspecified :foreground "#ffc125"))))
 '(diff-hl-delete ((t (:background unspecified :foreground "#ff3030"))))
 '(diff-hl-insert ((t (:background unspecified :foreground "green1"))))
 '(diredfl-date-time ((t (:foreground unspecified :background unspecified))))
 '(diredfl-deletion ((t (:foreground unspecified :background unspecified))))
 '(diredfl-dir-heading ((t (:foreground "#95a99f"))))
 '(diredfl-dir-name ((t (:foreground "#79C0FF"))))
 '(diredfl-dir-priv ((t (:foreground unspecified :background unspecified))))
 '(diredfl-exec-priv ((t (:foreground unspecified :background unspecified))))
 '(diredfl-file-name ((t (:foreground unspecified :background unspecified))))
 '(diredfl-file-suffix ((t (:foreground unspecified :background unspecified))))
 '(diredfl-no-priv ((t (:foreground unspecified :background unspecified))))
 '(diredfl-number ((t (:foreground unspecified :background unspecified))))
 '(diredfl-other-priv ((t (:foreground unspecified :background unspecified))))
 '(diredfl-rare-priv ((t (:foreground unspecified :background unspecified))))
 '(diredfl-read-priv ((t (:foreground unspecified :background unspecified))))
 '(diredfl-write-priv ((t (:foreground unspecified :background unspecified))))
 '(which-key-command-description-face ((t (:foreground "#79C0FF"))))
 '(which-key-group-description-face ((t (:foreground "#79C0FF" :weight medium))))
 '(orderless-match-face-1 ((t (:foreground "#79C0FF" :weight medium))))
 '(orderless-match-face-2 ((t (:foreground "#79C0FF" :weight medium))))
 '(orderless-match-face-3 ((t (:foreground "#79C0FF" :weight medium))))
 '(vertico-current ((t (:extend nil :background "gray35" :foreground "gray100"))))
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
  (setq org-fontify-quote-and-verse-blocks t) ;;开启之后 quote 的 block 才会有背景色
  )


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
