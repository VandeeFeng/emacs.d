;;; init-mark.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar pin-marks-alist nil
  "Pin bookmark list: (alias . marker)")

(defvar pin-marks-file (expand-file-name "~/.emacs.d/pin-marks.save")
  "Persistent bookmark save file")

;; Generate alias: buffer-name:line or filename:line
(defun generate-mark-alias ()
  (let* ((buf (or (buffer-name) (buffer-file-name)))
         (line (line-number-at-pos (point)))
         (name (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (replace-regexp-in-string "%" "%%" buf))))
    (format "%s:%d" name line)))

;; Save all bookmarks to file
(defun save-pin-marks ()
  (with-temp-file pin-marks-file
    (insert ";; Pin marks - automatically generated\n")
    (pp pin-marks-alist (current-buffer))))

;; Load bookmarks from file
(defun load-pin-marks ()
  (when (file-exists-p pin-marks-file)
    (with-temp-buffer
      (insert-file-contents pin-marks-file)
      (let ((content (buffer-string)))
        (when (string-match-p ";; Pin marks" content)
          (ignore-errors
            (setq pin-marks-alist (read content))))))))

;; Add or update a pin bookmark (auto-generate alias)
;;;###autoload
(defun set-pin-mark ()
  "Remember current position as pin bookmark, using buffer-name:line as alias."
  (interactive)
  (load-pin-marks) ; Ensure loaded
  (let* ((alias (generate-mark-alias))
         (file-path (buffer-file-name))
         (line-num (line-number-at-pos (point)))
         (column-num (current-column))
         (mark-info (list file-path line-num column-num)))
    (setq pin-marks-alist
          (assq-delete-all (intern alias) pin-marks-alist))
    (push `(,(intern alias) . ,mark-info) pin-marks-alist)
    (save-pin-marks)
    (message "Pin mark set → %s  (total: %d)" alias (length pin-marks-alist))))

;; Jump to pin bookmark (with completing-read fuzzy search)
;;;###autoload
(defun goto-pin-mark ()
  "Fuzzy search and goto saved pin bookmarks."
  (interactive)
  (load-pin-marks)
  (if (null pin-marks-alist)
      (message "No pin marks yet. Use `set-pin-mark' first.")
    (let* ((choices (mapcar (lambda (p)
                              (let* ((name (symbol-name (car p)))
                                     (mark-info (cdr p))
                                     (file-path (nth 0 mark-info))
                                     (line-num (nth 1 mark-info))
                                     (preview (if (file-exists-p file-path)
                                                  (with-temp-buffer
                                                    (insert-file-contents file-path)
                                                    (goto-line line-num)
                                                    (string-trim
                                                     (buffer-substring-no-properties
                                                      (line-beginning-position)
                                                      (line-end-position))))
                                                "(file not found)")))
                                (format "%-30s %s" name preview)))
                            pin-marks-alist))
           (selected (completing-read "Goto pin mark: " choices nil t))
           (alias (intern (replace-regexp-in-string " .*" "" selected)))
           (pair (assoc alias pin-marks-alist)))
      (when pair
        (let* ((mark-info (cdr pair))
               (file-path (nth 0 mark-info))
               (line-num (nth 1 mark-info))
               (column-num (nth 2 mark-info)))
          (if (file-exists-p file-path)
              (progn
                (find-file file-path)
                (goto-line line-num)
                (move-to-column column-num)
                (message "Goto pin mark: %s" (symbol-name alias)))
            (message "File %s no longer exists." file-path)
            ;; Remove dead bookmark
            (setq pin-marks-alist (assq-delete-all alias pin-marks-alist))
            (save-pin-marks)))))))

;; Optional: list all pin bookmarks (for easy viewing)
;;;###autoload
(defun list-pin-marks ()
  "List all pin bookmarks (display in separate buffer)."
  (interactive)
  (load-pin-marks)
  (with-current-buffer (get-buffer-create "*Pin Marks*")
    (erase-buffer)
    (insert (format "Pin marks (%d total):\n\n" (length pin-marks-alist)))
    (dolist (pair (reverse pin-marks-alist))
      (let* ((alias (symbol-name (car pair)))
             (mark-info (cdr pair))
             (file-path (nth 0 mark-info))
             (line-num (nth 1 mark-info))
             (file-exists (file-exists-p file-path)))
        (if file-exists
            (let ((preview (with-temp-buffer
                             (insert-file-contents file-path)
                             (goto-line line-num)
                             (string-trim
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))))
              (insert (format "%s  →  %s:%d  |  %s\n"
                              (propertize alias 'face 'bold)
                              (file-name-nondirectory file-path)
                              line-num
                              preview)))
          (insert (format "%s  →  %s (file not found)\n" alias file-path)))))
    (goto-char (point-min))
    (read-only-mode 1))
  (switch-to-buffer "*Pin Marks*"))

;; Delete a specific pin bookmark
;;;###autoload
(defun delete-pin-mark ()
  "Delete a specific pin bookmark."
  (interactive)
  (load-pin-marks)
  (if (null pin-marks-alist)
      (message "No pin marks to delete.")
    (let* ((choices (mapcar (lambda (p)
                              (let* ((name (symbol-name (car p)))
                                     (mark-info (cdr p))
                                     (file-path (nth 0 mark-info))
                                     (line-num (nth 1 mark-info))
                                     (preview (if (and file-path (file-exists-p file-path))
                                                  (with-temp-buffer
                                                    (insert-file-contents file-path)
                                                    (goto-line line-num)
                                                    (string-trim
                                                     (buffer-substring-no-properties
                                                      (line-beginning-position)
                                                      (line-end-position))))
                                                "(file not found)")))
                                (format "%-30s %s" name preview)))
                            pin-marks-alist))
           (selected (completing-read "Delete pin mark: " choices nil t))
           (alias (intern (replace-regexp-in-string " .*" "" selected))))
      (setq pin-marks-alist (assq-delete-all alias pin-marks-alist))
      (save-pin-marks)
      (message "Deleted pin mark: %s" (symbol-name alias)))))

;; Delete all pin bookmarks
;;;###autoload
(defun delete-all-pin-marks ()
  "Delete all pin bookmarks."
  (interactive)
  (load-pin-marks)
  (if (null pin-marks-alist)
      (message "No pin marks to delete.")
    (when (yes-or-no-p (format "Delete all %d pin marks? " (length pin-marks-alist)))
      (setq pin-marks-alist nil)
      (save-pin-marks)
      (message "All pin marks deleted."))))

;; Auto load on startup, auto save on shutdown
(add-hook 'emacs-startup-hook #'load-pin-marks)
(add-hook 'kill-emacs-hook #'save-pin-marks)

(provide 'init-mark)
;;; init-mark.el ends here
