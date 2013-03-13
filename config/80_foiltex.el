;;; foiltex
(defvar foiltex-page-beginning "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^")
(defvar foiltex-page-end "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$")
(defun foiltex-insert-new-page ()
  (interactive)
  (insert
   foiltex-page-beginning "
\\myhead{}
" foiltex-page-end)
  (forward-line -2)
  (beginning-of-line)
  (forward-char 8))
(defun foiltex-line-comp (target)
  (let ((len (length target))
	(p (point)))
    (string=
     target
     (buffer-substring p (min (point-max) (+ p len))))))
(defun foiltex-page-move (target move last-p)
  (beginning-of-line)
  (if (foiltex-line-comp target)
      (funcall move))
  (while (and (not (funcall last-p))
	      (not (foiltex-line-comp target)))
    (funcall move)))
(defun foiltex-next-page ()
  (interactive)
  (when (not (string= (string (char-before (point-max))) "\n"))
    (save-excursion
      (goto-char (point-max))
      (insert "\n")))
  (foiltex-page-move
   foiltex-page-beginning
   (lambda () (forward-line))
   (lambda () (= (point) (point-max)))))
(defun foiltex-previous-page ()
  (interactive)
  (foiltex-page-move
   foiltex-page-end
   (lambda () (forward-line -1))
   (lambda () (= (point) (point-min)))))




(defvar foiltex-minor-mode-key-map (make-sparse-keymap))
(define-key foiltex-minor-mode-key-map (kbd "C-M-f") 'foiltex-next-page)
(define-key foiltex-minor-mode-key-map (kbd "C-M-b") 'foiltex-previous-page)




(define-minor-mode foiltex-minor-mode
  "A minor mode to help foiltex file editing."
  :keymap foiltex-minor-mode-key-map)
