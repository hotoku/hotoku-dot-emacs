;;; moccur
(require 'color-moccur)
(require 'moccur-edit)
(defadvice moccur-edit-change-file
 (after save-after-moccur-edit-buffer activate)
 (save-buffer))
(defun moccur-grep-current-word (dir)
  (interactive
   (list (moccur-grep-read-directory)))
  (let ((word (current-word)))
     (message "abc")
     (message word)
     (moccur-grep dir (list word))))
