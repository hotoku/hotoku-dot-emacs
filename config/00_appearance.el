;;; initial frame settings
(setq initial-frame-alist
      (append
       (list
        '(top                  . 0) ;; 表示位置
        '(left                 . 0) ;; 表示位置
        '(foreground-color     . "grey20")
        '(background-color     . "grey")
        '(border-color         . "black")
        '(mouse-color          . "white")
        '(cursor-color         . "black")
        '(vertical-scroll-bars . nil)) ;; remove scroll bar
       initial-frame-alist))




;;; default frame
(setq default-frame-alist initial-frame-alist)




;;; color of string
(set-face-foreground 'font-lock-string-face        "DarkRed")
(set-face-foreground 'font-lock-variable-name-face "IndianRed")
(set-face-foreground 'font-lock-constant-face      "RoyalBlue")
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "black")




;;; for diff
(defun set-my-diff-mode-color ()
  (set-face-foreground 'diff-removed "Blue")
  (set-face-foreground 'diff-added "Red"))
(add-hook 'diff-mode-hook
          'set-my-diff-mode-color)
(add-hook 'magit-status-mode-hook
          'set-my-diff-mode-color)




;;; not truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows t)




;;; do not use tab
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
