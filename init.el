;;; init.el -- initial setting up process -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


;;; package
(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


;;; package-refresh-contents
;; Package-refresh-contents if last update is older than or equal to yesterday.
  (let* ((path (expand-file-name ".date-of-last-package-refresh-contents" user-emacs-directory))
         (today (format-time-string "%Y-%m-%d"))
         (last-date (when (file-exists-p path)
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-substring 1 11))))
         (should-update (or (not last-date)
                            (string< last-date today))))
    (when should-update
      (package-refresh-contents)
      (with-temp-buffer
        (insert today)
        (write-region (point-min) (point-max) path))))
(package-initialize)


;;; my utilities
(defconst yh/additional-loadpath (expand-file-name "yh" user-emacs-directory))
(add-to-list 'load-path yh/additional-loadpath)


;;; use-package initialize
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ; automatically install missing packages


;;; configuration of packages
(use-package yh :ensure nil)

(use-package yh-blog :ensure nil)

(use-package yh-fef :ensure nil
  :hook
  (emacs-lisp-mode . (lambda () (add-hook 'before-save-hook 'yh-fef-format-buffer nil t))))

(use-package yh-docker :ensure nil)

(use-package yh-font :ensure nil)

(use-package yh-make :ensure nil)

(use-package yh-sh :ensure nil)

(use-package yh-sql :ensure nil)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((js-mode . lsp))
  :commands
  (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-docker
  :defer t
  :config
  (defvar lsp-docker-client-packages
    '(lsp-clangd))

  (defvar lsp-docker-client-configs
    '((:server-id clangd :docker-server-id clangd-docker :server-command "/ccls/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04/bin/clangd")))

  ;; (lsp-docker-init-clients
	;;  :path-mappings '(("/Users/hotoku/projects/hotoku/lineage" . "/projects"))
	;;  :client-packages lsp-docker-client-packages
	;;  :client-configs lsp-docker-client-configs)
  :commands lsp-docker-init-clients)

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))  ; or lsp-deferred

(use-package flycheck
  :config
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-idle-change-delay 1
   flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package magit
  :bind (("C-c g" . magit))
  :custom
  (magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18) "show time of the commits"))

(use-package company
  :config
  (global-company-mode)
  (setq-default company-idel-delay 0.01))


;;; misc
;; make backup files in a specific directory
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
	           `("\\.*\\'" . ,(expand-file-name "~/backup")))

;; meta key
(setq ns-command-modifier (quote meta)
      ns-alternate-modifier (quote super))

;; global key
(progn
  (global-set-key (kbd "M-u") 'revert-buffer)
  (global-set-key (kbd "C-M-/") 'comment-region)
  (global-set-key (kbd "C-M--") 'uncomment-region)
  (global-set-key [?¥] [?\\])
  (global-set-key (kbd "C-.") 'yh/other-window-or-split))

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
(progn
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode)
  (blink-cursor-mode -1))

;; language
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment "Japanese")

;; change default directory for C-x C-f
(when (version< "27.0" emacs-version)
  (defun ad:helm-find-files (f prompt)
    "In Emacs 27.1 on Mac OS X, default directory for buffers like *Emacs* changes from ~ to /.
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
   '("57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "246cd0eb818bfd347b20fb6365c228fddf24ab7164752afe5e6878cb29b0204e" default))
 '(package-selected-packages
   '(yh-make il lsp-docker poetry gitignore-mode helm-ag pyenv afuternoon-theme afternoon-theme bazel-mode pyenv-mode-auto prettier-js dap-python py-autopep8 flymake-yaml dockerfile-mode biblio elpy haskell-mode yaml-mode json-mode gnu-elpa-keyring-update undo-tree git-ps1-mode ace-window flycheck yasnippet open-junk-file dakrone-theme smartparens helm company use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch :background "SkyBlue1" :foreground "gray13"))))
 '(markdown-header-delimiter-face ((t (:foreground "indian red"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :family "MeiryoKe_UIGothic"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0 :underline t))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-language-keyword-face ((t (:foreground "dark green"))))
 '(markdown-markup-face ((t (:foreground "indian red"))))
 '(sp-pair-overlay-face ((t (:inherit fixed-pitch :background "SkyBlue1" :foreground "gray13")))))

;; Theme loading should be after registration of it as safe by custom-set-variables
(use-package afternoon-theme
  :config
  (load-theme 'afternoon))

(provide 'init)
;;; init.el ends here
