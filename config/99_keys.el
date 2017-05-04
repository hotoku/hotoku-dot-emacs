;;; meta, super key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))




(global-set-key (kbd "C-x H") 'hatenahelper-mode)
(global-set-key (kbd "C-c , ,") 'howm-menu)
;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-.") 'other-window-or-split)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "C-c ;") 'helm-mini)
(global-set-key (kbd "C-c C-r") 'run-file-as-script)
(global-set-key (kbd "C-M-_") 'indent-region)
(global-set-key (kbd "C-c C-l") 'make-symbolic-link-of-current-buffer)




(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)




(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") 'run-file-as-script)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c ,"))))
