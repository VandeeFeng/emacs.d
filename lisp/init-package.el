;;; init-package.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; MelpaPackages
;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ))
;; -MelpaPackages

;; ConfigurePackageManager
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) ; To prevent initializing twice
  (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; -ConfigurePackageManager

;; ConfigureUsePackage
;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))
;; -ConfigureUsePackage

;; AutoPackageUpdate
(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 90) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (defun my/cleanup-old-package-dirs ()
    "Delete old versions of package directories from `package-user-dir`."
    (interactive)
    (message "Starting cleanup of old package directories...")
    (let ((packages (make-hash-table :test #'equal)))
      ;; Group package directories by name.
      (dolist (dir (directory-files package-user-dir t))
        (when (and (file-directory-p dir)
                   ;; Match paths like ".../packagename-1.2.3"
                   (string-match "/\\([^/]+?\\)-[0-9][.0-9]*\\'" dir))
          (let ((name (match-string 1 dir)))
            (push dir (gethash name packages nil)))))

      ;; For each package, find the newest version and delete the others.
      (maphash
       (lambda (name dirs)
         (when (> (length dirs) 1)
           (message "Checking package: %s" name)
           (let* ((sorted-dirs (sort dirs #'string>))
                  (newest-dir (car sorted-dirs))
                  (dirs-to-delete (cdr sorted-dirs)))
             (message "  Keeping: %s" (file-name-nondirectory newest-dir))
             (dolist (dir dirs-to-delete)
               (message "  Moving to trash: %s" (file-name-nondirectory dir))
               (move-file-to-trash dir)))))
       packages))
    (message "Cleanup of old package directories finished."))
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-success-hook 'my/cleanup-old-package-dirs))
;; -AutoPackageUpdate

;; DimPac
;;(use-package diminish)
;; -DimPac

(provide 'init-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
