(load "dired-x")


(require 'mdfind-dired)
(add-to-list 'auto-mode-alist
             '("\\.savedSearch\\'" . mdfind-dired-change-to-dired))




(defun my-dired-do-open (&optional arg)
  "In dired, invoke /usr/bin/open on the marked files.
If no files are marked or a specific numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file."
  (interactive "P")
  (let ((files (dired-get-marked-files nil arg)))
    (apply 'start-process "open_ps" nil "open" files)))
(eval-after-load "dired"
  '(define-key dired-mode-map "z" 'my-dired-do-open))

