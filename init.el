;;; load-path
(let* ((elisp-dir "~/.emacs.d/site-lisp")
       (default-directory elisp-dir))
  (add-to-list 'load-path elisp-dir)
  (normal-top-level-add-subdirs-to-load-path))
(setq load-path (cons "~/.emacs.d/auto-install" load-path))




;;; package
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)




;;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-compatibility-setup)




;;; dsvn
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(require 'vc-svn)
(require 'vc-svn17)




;;; howm
(setq howm-menu-lang 'ja)
(autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)




;;; ini
(require 'ini-mode)
(setq auto-mode-alist 
      (append 
       '(("\\.[iI][nN][iI]$" . ini-mode))
       auto-mode-alist))




;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)




;;; init-loader
(require 'init-loader)
(setq init-loader-directory "~/.emacs.d/config")
(init-loader-load)




;;; back up file
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
	    backup-directory-alist))




;;; move currrent buffer with shift + (arrow key)
(windmove-default-keybindings)




;;; server
(server-start)




;;; show-paren-mode
(show-paren-mode)




;;; 動的略語補完
(setq dabbrev-case-fold-search nil)




;;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))




;;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet-0.6.1c/snippets")




;;; for Imaxima
(autoload 'imaxima "imaxima" "Image support for Maxima." t)




;;; input backslush
(define-key global-map [?¥] [?\\])




;;; scheme
(setq scheme-program-name "gosh -i")




;;;
(require 'open-junk-file)
(setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d_%H%M%S.")




;;;
(setq load-path (cons "~/dropbox/emacs/w3m/share/emacs/site-lisp/w3m" load-path))
(require 'w3m-load)




;;;
(require 'lispxmp)




;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-c" 'anything-lisp-complete-symbol-partial-match)))




;;;
(require 'browse-kill-ring)




;;; git 
(add-to-list 'load-path "/opt/local/share/doc/git-core/contrib/emacs")
(require 'git)
(require 'git-blame)




;;;
(require 'session)
(add-hook 'after-init-hook 'session-initialize)




;;;
(require 'minibuf-isearch)




;;; haskell-mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)




;;;
(require 'magit)




;;; via. http://d.hatena.ne.jp/rubikitch/20100210/emacs#tb
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))




;;;
(require 'elscreen)
(setq elscreen-display-tab nil)




;;;
(setq sql-sqlite-program "sqlite3")




;;; flymake
(require 'flymake)
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (flymake-mode t)))
(add-hook 'haskell-mode-hook
	  '(lambda ()
	     (flymake-mode t)))




;;;
(defvar org-directory "")
(defvar w3m-command "/opt/local/bin/w3m")




;;; anything
(require 'anything-startup)




