;; -*- lexical-binding: t -*-




;;; package
(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)


;;; my utilities
(add-to-list 'load-path (expand-file-name "yh" user-emacs-directory))
(require 'yh)


;; TODO: 最後にrefreshした日を記録して古い場合だけrefreshする
(package-refresh-contents)


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
  :commands company-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  :config
  (setq company-idel-delay 0.01))

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
  (setq
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
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; global key
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "M-u") 'revert-buffer)
(global-set-key (kbd "C-M-/") 'comment-region)
(global-set-key (kbd "C-M--") 'uncomment-region)
(global-set-key [?¥] [?\\])

;; tab
(setq-default
 tab-width 2
 indent-tabs-mode nil)

;; truncate lines
(setq-default
 truncate-lines t)

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


;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("246cd0eb818bfd347b20fb6365c228fddf24ab7164752afe5e6878cb29b0204e" default))
 '(package-selected-packages
   '(dakrone-theme smartparens helm company session use-package))
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
