;;; yh-font.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh/use-huge-font ()
  "Change font size."
  (interactive)
  (setf (alist-get 'font default-frame-alist) "Monospace-20")
  (yh/recreate-frame))
(defun yh/use-large-font ()
  "Change font size."
  (interactive)
  (setf (alist-get 'font default-frame-alist) "Monospace-16")
  (yh/recreate-frame))
(defun yh/use-medium-font ()
  "Change font size."
  (interactive)
  (setf (alist-get 'font default-frame-alist) "Monospace-14")
  (yh/recreate-frame))
(defun yh/use-small-font ()
  "Change font size."
  (interactive)
  (setf (alist-get 'font default-frame-alist) "Monospace-12")
  (yh/recreate-frame))
(defun yh/recreate-frame (&optional w h)
  "Recreate frame.  W, H are frame size (width and height) in pixel."
  (let* ((f (selected-frame))
         (width (or w (frame-pixel-width f)))
         (height (or h (frame-pixel-height f))))
    (make-frame `((width . (text-pixels . ,width))
                  (height . (text-pixels . ,height))))
    (delete-frame f)))

(provide 'yh-font)
;;; yh-font.el ends here
