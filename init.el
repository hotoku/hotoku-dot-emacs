;;; init.el -- initial setting up process -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; package
(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)


;;; my utilities
(defconst yh/additional-loadpath (expand-file-name "yh" user-emacs-directory))
(add-to-list 'load-path yh/additional-loadpath)
(require 'yh)


;; refresh if necessary
(yh/package-refresh-contents)


;;; use-package initialize
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ; automatically install missing packages



;;; configuration of packages
(use-package magit
  :bind (("C-c g" . magit)))

(use-package session)

(use-package company
  :config
  (global-company-mode)
  (setq-default company-idel-delay 0.01))

(use-package helm
  :bind (("M-x" . helm-M-x)
	       ("M-y" . helm-show-kill-ring)
	       ("C-x C-f" . helm-find-files)
	       ("C-c h o" . helm-occur)
	       ("C-c m" . helm-mini))
  :config
  (helm-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (setq-default
   helm-idle-delay 0.1
   helm-input-idle-delay 0.1
   helm-delete-minibuffer-contents-from-point t
   helm-ff-auto-update-initial-value nil))

(use-package smartparens
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'python-mode-hook 'turn-on-smartparens-strict-mode)
  :bind (("C-M-f" . sp-forward-slurp-sexp)
         ("C-M-g" . sp-forward-barf-sexp))
  :config
  (message "usepackage smartparens")
  (show-smartparens-global-mode t))

(use-package smartparens-config :ensure nil)

(use-package projectile
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package open-junk-file
  :commands open-junk-file)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package flycheck
  :config
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-idle-change-delay 1
   flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package markdown-mode
  :mode
  (("\\.md\\'" . markdown-mode))
  :config
  (custom-set-faces
   '(markdown-code-face ((t (:inherit fixed-pitch :background "SkyBlue1" :foreground "gray13"))))
   '(markdown-header-face ((t (:inherit font-lock-function-name-face :family "MeiryoKe_UIGothic"))))
   '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0 :underline t))))
   '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
   '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
   '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))
   '(markdown-language-keyword-face ((t (:foreground "dark green"))))
   '(markdown-markup-face ((t (:foreground "indian red"))))
   '(markdown-header-delimiter-face ((t (:foreground "indian red")))))
  ;; (add-hook 'markdown-mode-hook (lambda () (setq-local yh/indent-before-save nil)))


  (setq
   ;; commonmarker command can be installed by "gem install -V commonmarker -n <destination directory>"
   markdown-command "commonmarker --extension=autolink --extension=strikethrough --extension=table --extension=tagfilter --extension=tasklist"
   markdown-command-needs-filename t
   markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css"
                        "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
   markdown-xhtml-header-content (mapconcat 'identity
                                            '("<style><!-- CSS HERE --></style>"
                                              "<script src=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js\"></script>"
                                              "<script>hljs.initHighlightingOnLoad();</script>")
                                            "\n")
   markdown-xhtml-body-preamble "<div class=\"markdown-body\">"
   markdown-xhtml-body-epilogue "</div>"))

;;; configurations for programming languages
;; elisp
(add-hook
 'emacs-lisp-mode-hook
 '(lambda ()
    (add-hook 'before-save-hook 'yh/indent-buffer nil t)))

;;; misc
;; make backup files in a specific directory
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
	           `("\\.*\\'" . ,(expand-file-name "~/backup")))

;; meta key
(setq ns-command-modifier (quote meta)
      ns-alternate-modifier (quote super))

;; global key
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "M-u") 'revert-buffer)
(global-set-key (kbd "C-M-/") 'comment-region)
(global-set-key (kbd "C-M--") 'uncomment-region)
(global-set-key [?¥] [?\\])
(global-set-key (kbd "C-.") 'yh/other-window-or-split)

;; tab
(setq-default tab-width 2
              indent-tabs-mode nil)

;; truncate lines
(setq-default truncate-lines t)

;; always answer in y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; emacsclient
(server-start)

;; aes
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)
(blink-cursor-mode -1)

;; language
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment "Japanese")


;; change default directory for C-x C-f
(when (version< "27.0" emacs-version)
  (defun ad:helm-find-files (f prompt)
    "When uploading to Emacs 27.1, default directory for buffers like *Emacs* changes from ~ to /.
This is inconvinient when opening file at the beginning of Emacs session."
    (when (equal default-directory "/")
      (setq default-directory "~/"))
    (funcall f prompt))
  (advice-add 'helm-find-files :around 'ad:helm-find-files))



;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("246cd0eb818bfd347b20fb6365c228fddf24ab7164752afe5e6878cb29b0204e" default))
 '(package-selected-packages
   '(ace-window flycheck yasnippet open-junk-file dakrone-theme smartparens helm company session use-package))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Theme loading should be after registration of it as safe by custom-set-variables
(use-package dakrone-theme
  :config
  (load-theme 'dakrone)
  (set-face-foreground 'font-lock-comment-face "#8AE234")
  (set-face-foreground 'font-lock-string-face "IndianRed"))




(provide 'init)
;;; init.el ends here
