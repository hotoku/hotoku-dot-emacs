;;; load local settings
(let ((site-file "~/.emacs.d/site-config/config.el"))
  (when (file-readable-p site-file)
    (load-file site-file)))




;;; load path
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))




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
                magit
                dsvn
                scala-mode2
                ensime
                open-junk-file
                auto-complete
                auto-complete-c-headers
                markdown-mode
                iedit
                flymake-cursor)
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
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "/Users/01004106/.emacs.d/el-get/yasnippet/snippets"))
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




;;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))




;;; auto-complete
(require 'auto-complete-config)
(ac-config-default)




;;; auto-complete-c-headers
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-sources-c-headers)
  (add-to-list 'achead:include-directories
               '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1")
  (add-to-list 'achead:include-directories
               '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/6.0/include")
  (add-to-list 'achead:include-directories
               '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"))
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)




;;; minibuf-isearch
(require 'minibuf-isearch)




;;; iedit
(define-key global-map (kbd "C-c ;") 'iedit-mode)




;;; helm
(require 'helm-config)
(helm-mode 1)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-delete-minibuffer-contents-from-point t)
(setq helm-ff-auto-update-initial-value nil)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(safe-local-variable-values
   (quote
    ((run-test-target . test\.sh)
     (run-test-target . test/test-ofx\.py))))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :background "White" :foreground "purple4"))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :foreground "DarkOrchid4" :underline t))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :foreground "Black")))))
