(require 'package)


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



(provide 'yh)
