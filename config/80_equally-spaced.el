(defvar equally-spaced-width 4)
(defun equally-spaced-blank-line-p ()
  (string-match
   "^[ \t]*$" (buffer-substring
	       (line-beginning-position)
	       (line-end-position))))
(defun equally-spaced-contents-line-p ()
  (not (equally-spaced-blank-line-p)))
(defun equally-spaced-goto-next (predicate)
  (while (and (< (point) (point-max))
	      (not (funcall predicate)))
    (forward-line)))
(defun equally-spaced-goto-top ()
  (goto-char (point-min))
  (equally-spaced-goto-next
   (function equally-spaced-contents-line-p)))
(defun equally-spaced-goto-next-blank ()
  (beginning-of-line)
  (equally-spaced-goto-next
   (function equally-spaced-blank-line-p)))
(defun equally-spaced-goto-next-contents ()
  (beginning-of-line)
  (equally-spaced-goto-next
   (function equally-spaced-contents-line-p)))
(defun equally-spaced-make-gap-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n")
    (equally-spaced-goto-top)
    (while (not (= (point) (point-max)))
      (let* ((begin-pos
	      (progn
		(equally-spaced-goto-next-blank)
		(point)))
	     (end-pos
	      (progn
		(equally-spaced-goto-next-contents)
		(point))))
	(goto-char begin-pos)
	(delete-region begin-pos end-pos)
	(if (< (point) (point-max))
	    (open-line equally-spaced-width))
	(forward-line equally-spaced-width)))))
