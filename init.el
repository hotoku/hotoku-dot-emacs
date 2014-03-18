;;; load local settings
(let ((site-file "~/.emacs.d/site-config/config.el"))
  (when (file-readable-p site-file)
    (load-file site-file)))




;;; el-get install check
(setq el-get-git-install-url "https://github.com/dimitri/el-get")
(setq el-get-github-default-url-type "https")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))




;;; install necessary library by el-get
(defvar my-packages
  '(exec-path-from-shell))
(el-get 'sync my-packages)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))




(setq el-get-sources
      '((:name init-loader
               :type github
               :pkgname "emacs-jp/init-loader")
        (:name equally-spaced
               :type github
               :pkgname "hotoku/equally-spaced")
        (:name session
               :url "http://downloads.sourceforge.net/project/emacs-session/session/session-2.3.tar.gz")
        (:name qiita-el
               :description "Qiita API Library for emacs"
               :type github
               :pkgname "gongo/qiita-el")))
(setq my-packages
      (append my-packages
              '(el-get
                howm
                helm
                auto-install
                yasnippet
                browse-kill-ring
                haskell-mode
                ess
                color-moccur
                moccur-edit
                magit)
              (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-packages)
(el-get-cleanup my-packages)




;;; package
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)




;;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-compatibility-setup)
(add-to-list 'load-path auto-install-directory)




;;; dsvn
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(require 'vc-svn)




;;; howm
(require 'howm)
(setq howm-menu-lang 'ja)
(setq howm-keyword-file "~/.emacs.d/.howm-keys")
(autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org")
(setq howm-todo-menu-types "[-+~!]")




;; ;;; ini
;; (require 'ini-mode)
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.[iI][nN][iI]$" . ini-mode))
;;        auto-mode-alist))




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
(require 'dabbrev)
(setq dabbrev-case-fold-search nil)




;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)




;;; for Imaxima
(autoload 'imaxima "imaxima" "Image support for Maxima." t)




;;; input backslush
(define-key global-map [?¥] [?\\])




;;; scheme
(require 'cmuscheme)
(setq scheme-program-name "gosh -i")




;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-c" 'anything-lisp-complete-symbol-partial-match)))




;;;
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)




;;; magit
(require 'magit)




;;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)




;;; haskell-mode
;; (load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)




;;; sqlite
(require 'sql)
(setq sql-sqlite-program "sqlite3")




;;; w3m
(defvar w3m-command "/opt/local/bin/w3m")




;;; equally-spaced
(require 'equally-spaced)




;;; helm
(require 'helm-config)
(helm-mode 1)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-delete-minibuffer-contents-from-point t)
(setq helm-ff-auto-update-initial-value nil)
(define-key helm-c-read-file-map (kbd "TAB") 'helm-execute-persistent-action)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(safe-local-variable-values (quote ((run-test-target . test\.sh) (run-test-target . test/test-ofx\.py))))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
