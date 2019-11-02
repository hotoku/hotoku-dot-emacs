;;; meta, super key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))




(global-set-key (kbd "C-x H") 'hatenahelper-mode)
(global-set-key (kbd "C-c , ,") 'howm-menu)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "C-c ;") 'helm-mini)
(global-set-key (kbd "C-c C-r") 'run-file-as-script)
(global-set-key (kbd "C-M-_") 'indent-region)
(global-set-key (kbd "C-c C-l") 'make-symbolic-link-of-current-buffer)
(global-set-key (kbd "C-,") 'other-frame)



(global-set-key
 (kbd "C-.")
 (lambda ()
   (interactive)
   (if (and
        (= (length (window-list)) 1)
        (= (length (frame-list)) 1))
       (split-window-right)
     (ace-window 1))))



(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)




(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") 'run-file-as-script)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c ,"))))
