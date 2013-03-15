;;; foiltex
(defvar foiltex-page-beginning "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^")
(defvar foiltex-page-end "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$")
(defun foiltex-insert-new-page ()
  (interactive)
  (insert
   foiltex-page-beginning "
\\myhead{}
" foiltex-page-end)
  (forward-line -1)
  (beginning-of-line)
  (forward-char 8))
(defun foiltex-line-comp (target)
  (let ((len (length target))
	(p (line-beginning-position)))
    (string=
     target
     (buffer-substring p (min (point-max) (+ p len))))))
(defun foiltex-at (begin-or-end)
  "Returns whether current line is beginning(end) of foiltex page."
  (let ((target
	 (cond
	  ((equal begin-or-end 'begin) foiltex-page-beginning)
	  ((equal begin-or-end 'end) foiltex-page-end)
	  (t (error "Argument is invalid, foiltex-at: %s" begin-or-end)))))
    (foiltex-line-comp target)))
(defun foiltex-page-move (target direction)
  (beginning-of-line)
  (let* (move last-pos)
    (cond
     ((equal direction 'down)
      (setq move (lambda () (forward-line)))
      (setq last-pos (point-max)))
     ((equal direction 'up)
      (setq move (lambda () (forward-line -1)))
      (setq last-pos (point-min))))
    (while (and (/= (point) last-pos)
		(not (foiltex-line-comp target)))
      (funcall move))))
(defun foiltex-next-page ()
  (interactive)
  (when (not (string= (string (char-before (point-max))) "\n"))
    (save-excursion
      (goto-char (point-max))
      (insert "\n")))
  (forward-line)
  (foiltex-page-move foiltex-page-beginning 'down))
(defun foiltex-previous-page ()
  (interactive)
  (forward-line -1)
  (foiltex-page-move foiltex-page-end  'up))




(defvar foiltex-minor-mode-key-map (make-sparse-keymap))
(define-key foiltex-minor-mode-key-map (kbd "C-M-f") 'foiltex-next-page)
(define-key foiltex-minor-mode-key-map (kbd "C-M-b") 'foiltex-previous-page)




(define-minor-mode foiltex-minor-mode
  "A minor mode to help foiltex file editing."
  :keymap foiltex-minor-mode-key-map)
