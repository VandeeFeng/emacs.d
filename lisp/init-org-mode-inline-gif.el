;;; init-org-mode-inline-gif.el --- Auto animate inline GIFs in org-mode -*- lexical-binding: t; -*-

;; Author: Vandee
;; Version: 0.2
;; Keywords: org, images, gif, animation
;; Package-Requires: ((emacs "27.1"))
;; License: MIT

;;; Commentary:

;; Automatically animates inline GIF images in org-mode buffers.
;; Hover (cursor stays) over a GIF will trigger animation.
;; Lightweight, non-intrusive, resource-friendly.

;;; Code:

(require 'cl-lib)

(defgroup org-mode-inline-gif nil
  "Auto-play inline GIFs in org-mode buffers."
  :prefix "org-mode-inline-gif-"
  :group 'org)

(defcustom org-mode-inline-gif-idle-delay 0.6
  "Delay in seconds before auto-playing a hovered GIF."
  :type 'number
  :group 'org-mode-inline-gif)

(defcustom org-mode-inline-gif-animation-duration 3
  "Duration in seconds after which an animation is considered done.
Used to clear the 'playing' state to allow replays."
  :type 'number
  :group 'org-mode-inline-gif)

(defvar-local org-mode-inline-gif--playing-overlays (make-hash-table :test 'eq)
  "Hash table to track overlays currently being animated.")

(defvar-local org-mode-inline-gif--hover-timer nil
  "Idle timer for triggering GIF playback.")

(defun org-mode-inline-gif--get-overlay-at-point ()
  "Return the inline image overlay under point, if any."
  (car (cl-remove-if-not
        (lambda (ov) (eq (overlay-get ov 'org-image-overlay) t))
        (overlays-at (point)))))

(defun org-mode-inline-gif--animate-once ()
  "Animate the inline GIF at point if not already playing."
  (let ((ov (org-mode-inline-gif--get-overlay-at-point)))
    (when (and ov
               (not (gethash ov org-mode-inline-gif--playing-overlays)))
      (let ((img (overlay-get ov 'display)))
        (when (image-animated-p img)
          (puthash ov t org-mode-inline-gif--playing-overlays)
          (image-animate img)
          (run-at-time org-mode-inline-gif-animation-duration nil
                       (lambda ()
                         (remhash ov org-mode-inline-gif--playing-overlays))))))))

(defun org-mode-inline-gif--hover-trigger ()
  "Debounced trigger to play inline GIF under point."
  (when (and (eq major-mode 'org-mode)
             (display-graphic-p))
    (when org-mode-inline-gif--hover-timer
      (cancel-timer org-mode-inline-gif--hover-timer))
    (setq org-mode-inline-gif--hover-timer
          (run-with-idle-timer org-mode-inline-gif-idle-delay nil
                               #'org-mode-inline-gif--animate-once))))

;;;###autoload
(define-minor-mode org-mode-inline-gif-mode
  "Minor mode to auto-play inline GIFs in org-mode when hovering."
  :lighter " 🌀"
  :group 'org-mode-inline-gif
  (if org-mode-inline-gif-mode
      (add-hook 'post-command-hook #'org-mode-inline-gif--hover-trigger nil t)
    (remove-hook 'post-command-hook #'org-mode-inline-gif--hover-trigger t)
    (when org-mode-inline-gif--hover-timer
      (cancel-timer org-mode-inline-gif--hover-timer))
    (clrhash org-mode-inline-gif--playing-overlays)))

(provide 'init-org-mode-inline-gif)

;;; init-org-mode-inline-gif.el ends here
