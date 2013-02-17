(require 'simple-hatena-mode)
(setq simple-hatena-root "~/Dropbox/hatena")
(setq simple-hatena-default-id "hotoku")
(setq simple-hatena-time-offset 6)
(setq simple-hatena-bin "hw.pl")
(set-face-foreground 'simple-hatena-markup-face "DarkSlateBlue")



(defadvice simple-hatena-electric-asterisk
  (after simple-hatena-electric-asterisk-advice activate)
  "When the second * is inputted on the head of a line, 
remove time stamp which was inserted by the function"
  (let ((pos (line-beginning-position)))
    (if (and simple-hatena-use-timestamp-permalink-flag
	     (= (current-column) 13)
	     (string-match "\\*[0-9]+\\*\\*"
			   (buffer-substring pos (+ pos 13))))
	(delete-region pos (+ pos 11)))))





(add-hook 'simple-hatena-mode-hook
	  '(lambda () (local-set-key "\C-c\C-j" 'simple-hatena-insert-tex)))
(defun simple-hatena-insert-tex ()
  (interactive)
  (insert "[tex:]")
  (backward-char))





(require 'html-helper-mode)
(require 'hatenahelper-mode)
(add-hook 'simple-hatena-mode-hook
	  '(lambda ()
	     (hatenahelper-mode 1)))
(defadvice hatenahelper-insert-sonomama-textblock
  (after hatenahelper-insert-sonomama-textblock-advice activate)
  "When quote braces are inserted move point where language is input."
  (progn (previous-line)
	 (forward-char 2)))
