(defun make-symbolic-link-of-current-buffer (linkname)
  "make symbolic link of current buffer"
  (interactive "FLink: ")
  (let ((filename (buffer-file-name)))
    (make-symbolic-link filename linkname)))
