;;; load local settings

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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


;;; using emlpa recipes
;;; c.f. https://stackoverflow.com/questions/23165158/how-do-i-install-melpa-packages-via-el-get/23169388
(require 'el-get-elpa) 
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))



(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes/")

(setq my-packages
      (append my-packages
              '(ace-window
                auto-complete
                auto-complete-c-headers
                auto-install
                browse-kill-ring
                color-moccur
                dockerfile-mode
                dired-git-info-mode
                dsvn
                el-get
                ensime
                equally-spaced
                flycheck
                flymake-cursor
                git-ps1-mode
                graphviz-dot-mode
                helm
                iedit
                init-loader
                jinja2-mode
                kotlin-mode
                lsp-mode
                magit
                markdown-mode
                markdown-preview-mode
                moccur-edit
                open-junk-file
                prettier-emacs
                projectile
                py-autopep8
                rjsx-mode
                session
                tide
                uuidgen
                web-mode
                web-server
                websocket
                yaml-mode
                yasnippet
                yatex)
              (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-packages)
(el-get-cleanup my-packages)

(require 'git-ps1-mode)
(setq git-ps1-mode-ps1-file "/usr/local/etc/bash_completion.d/git-prompt.sh")
(add-hook 'dired-mode-hook
          'git-ps1-mode)


;;;
(require 'prettier-js)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)


;;;
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))


;;;
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;;;
(add-hook 'python-mode-hook
          'which-function-mode)



;;;
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))


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
      (cons (cons "\\.*$" (expand-file-name "~/misc/backup"))
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
(autoload 'markdown-preview-mode "markdown-preview-mode"
  "markdown preview" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-preview-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch :background "ivory" :foreground "SlateGray4"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :family "MeiryoKe_UIGothic"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0 :underline t))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2)))))




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




;;; iedit
(define-key global-map (kbd "C-c ;") 'iedit-mode)




;;; helm
(require 'helm-config)
(helm-mode 1)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-delete-minibuffer-contents-from-point t)
(setq helm-ff-auto-update-initial-value nil)




(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-c h o") 'helm-occur)




;;; recentf
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))




;;; markdown-preview-mode
(autoload 'markdown-preview-mode "markdown-preview-mode.el" t)




;;; autopep8
(require 'py-autopep8)
(setq py-autopep8-options
      '("--max-line-length=300"
        "--ignore=E402"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)




;;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))




;;; flycheck
(setq flycheck-check-syntax-automatically
  '(save idle-change mode-enabled))
(setq flycheck-idle-change-delay 3)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(flycheck-add-mode 'html-tidy 'web-mode))
(setq flycheck-flake8-maximum-line-length 200)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(markdown-preview-http-port 9004)
 '(markdown-unordered-list-item-prefix "* ")
 '(python-shell-interpreter "python3")
 '(recentf-max-saved-items 2000)
 '(safe-local-variable-values
   (quote
    ((run-test-target . test\.sh)
     (run-test-target . test/test-ofx\.py))))
 '(session-use-package t nil (session))
 '(tab-width 2))
