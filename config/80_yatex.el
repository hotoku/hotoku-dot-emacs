(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode -1)))



;;; tex コマンド
(defvar tex-command "mylatex.sh" ; "platex"
  "*Default command for typesetting LaTeX text.")

;;; 数式の色
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(YaTeX-font-lock-formula-face ((((class color) (background light)) (:foreground "DarkRed"))))
 '(YaTeX-font-lock-math-sub-face ((((class color) (background light)) (:foreground "DarkRed"))))
 '(YaTeX-font-lock-math-sup-face ((((class color) (background light)) (:foreground "DarkRed")))))

;;; スクリプト挿入
(defun yatex-insert-script (prefix script)
  (insert (concat prefix "{" script "}")))
(defun yatex-insert-subscript (script)
  (interactive "sscript: ")
  (yatex-insert-script "_" script))
(defun yatex-insert-superscript (script)
  (interactive "sscript: ")
  (yatex-insert-script "^" script))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-f" 'yatex-insert-subscript)
             (local-set-key "\C-c\C-g" 'yatex-insert-superscript)))
