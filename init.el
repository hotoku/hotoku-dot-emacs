;; -*- lexical-binding: t -*-




;;; package
(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)


;;; my functions
(add-to-list 'load-path (expand-file-name (format "%s%s" user-emacs-directory "yh")))
(require 'yh)


;; TODO: 最後にrefreshした日を記録して古い場合だけrefreshする
;; (package-refresh-contents)


;;; use-package initialize
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


(use-package magit
  :bind (("C-c g" . magit)))

(use-package session)


;;; misc
;; mark correspondig parens
(show-paren-mode)

;; make backup files in a specific directory
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
	     `("\\.*\\'" . ,(expand-file-name "~/backup")))

;; meta key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))





;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(session use-package))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
