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




;;;
(when (and (>= emacs-major-version 24) (not (null window-system)))
  (let* ((font-family "Menlo")
         (font-size 12)
         (font-height (* font-size 10))
         (jp-font-family "ヒラギノ角ゴ ProN"))
    (set-face-attribute 'default nil :family font-family :height font-height)
    (let ((name (frame-parameter nil 'font))
          (jp-font-spec (font-spec :family jp-font-family))
          (jp-characters '(katakana-jisx0201
                           cp932-2-byte
                           japanese-jisx0212
                           japanese-jisx0213-2
                           japanese-jisx0213.2004-1))
          (font-spec (font-spec :family font-family))
          (characters '((?\u00A0 . ?\u00FF)    ; Latin-1
                        (?\u0100 . ?\u017F)    ; Latin Extended-A
                        (?\u0180 . ?\u024F)    ; Latin Extended-B
                        (?\u0250 . ?\u02AF)    ; IPA Extensions
                        (?\u0370 . ?\u03FF)))) ; Greek and Coptic
      (dolist (jp-character jp-characters)
        (set-fontset-font name jp-character jp-font-spec))
      (dolist (character characters)
        (set-fontset-font name character font-spec))
      (add-to-list 'face-font-rescale-alist (cons jp-font-family 1.2)))))
