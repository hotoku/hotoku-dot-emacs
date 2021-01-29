;;; yh.el --- personal utility functions

;;; Commentary:

;;; Code:


(require 'package)

;;; font size
(defun yh/use-large-font ()
  "Change font size."
  (interactive)
  (setf (alist-get 'font default-frame-alist) "Monospace-16")
  (yh/recreate-frame))
(defun yh/use-small-font ()
  "Change font size."
  (interactive)
  (setf (alist-get 'font default-frame-alist) "Monospace-12")
  (yh/recreate-frame))
(defun yh/recreate-frame (&optional w h)
  "Recreate frame.  W, H are frame size (width and height)."
  (let ((f (selected-frame)))
    (make-frame)
    (delete-frame f)))

;;; indent buffer
(defun yh/indent-buffer ()
  "Indent whole buffer."
  (indent-region (point-min) (point-max)))

;;; package-refresh-contents
(defun yh/package-refresh-contents ()
  "Package-refresh-contents if last update is older than or equal to yesterday."
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
  "Move to another window.  If the frame has only 1 window, split first."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

;;; mapcar with multiple argument
(defun yh-mapcar (f &rest xs)
  "Apply F on each value of XS."
  (when (not (memq nil xs))
    (cons (apply f (mapcar 'car xs))
          (apply 'yh-mapcar f (mapcar 'cdr xs)))))


(provide 'yh)
;;; yh.el ends here
