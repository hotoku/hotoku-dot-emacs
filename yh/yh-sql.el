;;; yh-sql.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-sql-format ()
  "Format buffer with zetasql-formatter.
https://github.com/Matts966/zetasql-formatter."
  (interactive)
  (let* ((fpath (buffer-file-name)))
    (call-process-shell-command (format "zetasql-formatter %s" fpath))
    (revert-buffer t t)))

(provide 'yh-sql)
;;; yh-sql.el ends here
