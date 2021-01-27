(require 'package)

;;; font size
(defun yh/use-large-font (&optional no-recreate)
  (interactive)
  (setf (alist-get 'font default-frame-alist) "Monospace-16")
  (unless no-recreate
    (yh/recreate-frame)))
(defun yh/use-small-font (&optional no-recreate)
  (interactive)
  (setf (alist-get 'font default-frame-alist) "Monospace-12")
  (unless no-recreate
    (yh/recreate-frame)))
(defun yh/recreate-frame ()
  (let ((f (selected-frame)))
    (make-frame)
    (delete-frame f)))

;;; indent buffer
(defun yh/indent-buffer ()
  (indent-region (point-min) (point-max)))

;;; package-refresh-contents
(defun yh/package-refresh-contents ()
  "package-refresh-contents if last update is older than or equal to yesterday."
  (let* ((path (expand-file-name ".date-of-last-package-refresh-contents" user-emacs-directory))
         (today (format-time-string "%Y-%m-%d"))
         (last-date (when (file-exists-p path)
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-substring 1 11))))
         (should-update (or (not last-date)
                            (string< last-date today))))
    (when should-update
      (package-refresh-contents)
      (with-temp-buffer
        (insert today)
        (write-region (point-min) (point-max) path)))))

;;; other-window
(defun yh/other-window-or-split ()
  "Move to another window. If the frame has only 1 window, split first"
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window 1))

(provide 'yh)
