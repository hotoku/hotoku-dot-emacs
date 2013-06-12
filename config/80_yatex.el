(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode -1)))



;;; tex コマンド
(defvar tex-command "mylatex.sh" ; "platex"
  "*Default command for typesetting LaTeX text.")

;;; 数式の色
(set-face-foreground 'YaTeX-font-lock-formula-face "DarkRed")
(set-face-foreground 'YaTeX-font-lock-math-sub-face "DarkRed")
(set-face-foreground 'YaTeX-font-lock-math-sup-face "DarkRed")

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
