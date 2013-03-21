(defun pgcontest-insert-template ()
  (interactive)
  (yacompile-insert-command t)
  (insert "\n\n")
  (let ((codeforces-template-path
         "~/.emacs.d/resource/contest-template.cpp"))
    (insert-file codeforces-template-path)))
(defun pgcontest-create-testfile ()
  (interactive)
  (let* ((test-file-path
          (replace-regexp-in-string "\\.[^\\.]+$" ".test" buffer-file-name)))
    (shell-command (format "touch %s" test-file-path))
    test-file-path))
(defun pgcontest-prepare ()
  (interactive)
  (let* ((test-file
          (pgcontest-create-testfile)))
    (pgcontest-insert-template)  
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (split-window-vertically)
    (other-window 1)
    (find-file test-file)
    (other-window 1)))
