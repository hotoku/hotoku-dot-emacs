(setq howm-view-title-header "#")
(setq howm-menu-file "~/.emacs.d/howm-menu.txt")
(require 'howm)
(setq howm-menu-lang 'ja)
(setq howm-keyword-file "~/.emacs.d/.howm-keys")
(autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")
(setq howm-todo-menu-types "[-+~!]")




(defun howm-list-undone-todo-in-this-buffer ()
  (interactive)
  "list all undone todos"
  (shell-command-on-region 
   (point-min) (point-max)
   "grep -E [0-9]{4}-[0-9]{2}-[0-9]{2}]\\\\+ | sort"))
(defun howm-search-forward-undone-todo ()
  (interactive)
  (let ((regex "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}]\\+"))
    (search-forward-regexp regex)
    ))




(add-hook
 'howm-mode-hook
 (lambda ()
   (local-set-key (kbd "M-s") 
                  'howm-search-forward-undone-todo)))
