;;; initial frame size
(cond
 ((string= (system-name) "horikoshi-yasunori-no-iMac.local")
  (setq initial-frame-alist
        (append (list '(width . 235) '(height . 58)) initial-frame-alist))
  (split-window-horizontally)) ; use 2 pane
 ((string= (system-name) "horikoshi-yasunori-no-MacBook-Pro.local")
  (setq initial-frame-alist
        (append (list '(width . 157) '(height . 42)) initial-frame-alist))))




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




;;; font
(add-to-list 'initial-frame-alist
             '(font . "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
(create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo" 'unicode (font-spec :family "Hiragino Kaku Gothic ProN" ) nil 'append)
(add-to-list 'initial-frame-alist '(font . "fontset-menlokakugo"))
(setq face-font-rescale-alist '((".*Hiragino.*" . 1.2) (".*Menlo.*" . 1.0)))




;;; default frame
(setq default-frame-alist initial-frame-alist)




;;; color of string 
(set-face-foreground 'font-lock-string-face        "DarkRed")
(set-face-foreground 'font-lock-variable-name-face "IndianRed")
(set-face-foreground 'font-lock-constant-face      "RoyalBlue")
(set-face-foreground 'modeline "black")
(set-face-background 'modeline "white")




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
