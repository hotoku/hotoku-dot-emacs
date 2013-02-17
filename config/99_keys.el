;;; meta, super key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))





(global-set-key "\C-xH" 'hatenahelper-mode)
(global-set-key "\C-c,," 'howm-menu)
;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)
(global-set-key "\C-c\C-g" 'magit-status)
(global-set-key (kbd "C-,") 'other-window-or-split)
(global-set-key [?\C-1] 'delete-other-windows)
(global-set-key "\C-cd" 'flymake-display-err-menu-for-current-line)