;;; yh-sql.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


;;;###autoload
(defun yh-sql-format ()
  "Format buffer with zetasql-formatter.
https://github.com/Matts966/zetasql-formatter."
  (interactive)
  (let* ((curbuf (current-buffer))
         (sql (buffer-string))
         (fpath (make-temp-file "zetasql" nil ".sql"))
         (buffer (find-file fpath)))
    (switch-to-buffer buffer)
    (insert sql)
    (save-buffer)
    (message fpath)
    (call-process-shell-command (format "zetasql-formatter %s" fpath))
    (revert-buffer t t)
    (let* ((sql2 (buffer-string)))
      (message sql2)
      (kill-buffer buffer)
      (switch-to-buffer curbuf)
      (delete-region (point-min) (point-max))
      (insert sql2))))

(provide 'yh-sql)
;;; yh-sql.el ends here
