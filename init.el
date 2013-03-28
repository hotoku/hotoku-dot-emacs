;;; el-get install check
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))




;;; install necessary library by el-get
(setq el-get-sources
      '((:name init-loader
               :type http
               :url "https://raw.github.com/gist/1021706/init-loader.el")
        (:name equally-spaced
               :type http
               :url "https://raw.github.com/gist/4975695/equally-spaced.el")
        (:name session
               :url "http://downloads.sourceforge.net/project/emacs-session/session/session-2.3.tar.gz")
        (:name foiltex-mode
               :type http
               :url "https://raw.github.com/gist/5180394/foiltex-mode.el")
        (:name qiita-el
               :description "Qiita API Library for emacs"
               :type github
               :pkgname "gongo/qiita-el")))
(defvar my-packages
      (append '(el-get
                magit
                howm
                helm
                auto-install
                yasnippet
                browse-kill-ring
                elscreen
                haskell-mode)
              (mapcar 'el-get-source-name el-get-sources)))
(el-get-cleanup my-packages)
(el-get 'sync my-packages)




;;; load-path
(let ((default-directory "~/dropbox/misc/elisp"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))




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




;;; w3m
(setq load-path (cons "~/dropbox/emacs/w3m/share/emacs/site-lisp/w3m" load-path))
(require 'w3m-load)




;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-c" 'anything-lisp-complete-symbol-partial-match)))




;;;
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)




;;; git
(add-to-list 'load-path "/opt/local/share/git-core/contrib/emacs")
(require 'git)
(require 'git-blame)
(require 'magit)




;;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)




;;; minibuf
(require 'minibuf-isearch)




;;; haskell-mode
;; (load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)




;;; elscreen
(require 'elscreen)
(setq elscreen-display-tab nil)




;;; sqlite
(require 'sql)
(setq sql-sqlite-program "sqlite3")




;;; w3m
(defvar w3m-command "/opt/local/bin/w3m")




;;; markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-command "multimarkdown")




;;; equally-spaced
(require 'equally-spaced)




;;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)




;;; auto-async-byte-compile
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)




;;; foiltex-mode
(require 'foiltex-mode)




;;; qiita
(require 'qiita)




;;; helm
(require 'helm-config)
(helm-mode 1)
