;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
(use-package almost-mono-themes
  :ensure t
  :config
  ;; (load-theme 'almost-mono-black t)
  (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)
  )


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
