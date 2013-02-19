(defvar equally-spaced-width 4)
(defun equally-spaced-blank-line ()
  (interactive)
  (string-match
   "^[ \t]*$" (buffer-substring
	       (line-beginning-position)
	       (line-end-position))))
(defun equally-spaced-line-num ()
  (interactive)
  (+ 1 (count-lines (point-min) (line-beginning-position))))
(defun equally-spaced-last-line (direction)
  (interactive)
  (save-excursion
    (when (equally-spaced-blank-line)
      (while (and (= (forward-line direction) 0)
		  (equally-spaced-blank-line))))
    (equally-spaced-line-num)))
(defun equally-spaced-line-beginning (n)
  (save-excursion
    (goto-line n)
    (line-beginning-position)))
(defun equally-spaced-make-gap ()
  (interactive)
  (let* ((line1 (equally-spaced-last-line -1))
	 (line2 (equally-spaced-last-line 1))
	 (pos1 (equally-spaced-line-beginning (+ 1 line1)))
	 (pos2 (equally-spaced-line-beginning line2)))
    (if (> line2 line1) (delete-region pos1 pos2))
    (open-line equally-spaced-width)))
(defun equally-spaced-make-gap-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((doit t))
      (while doit
	(message (number-to-string (equally-spaced-line-num)))
	(when (equally-spaced-blank-line)
	  (equally-spaced-make-gap)
	  (forward-line equally-spaced-width))
	(setq doit (= (forward-line 1) 0))))))




;;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))




;;; via. http://d.hatena.ne.jp/rubikitch/20100210/emacs#tb
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))




