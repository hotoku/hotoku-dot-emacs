(progn "install el-git"
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (add-to-list 'el-get-recipe-path "~/.emacs.d/recipes"))

(progn "exec-path-from-shell"
  (defvar yh/my-packages)
	(setq yh/my-packages '(exec-path-from-shell))
	(el-get 'sync yh/my-packages)
	(require 'exec-path-from-shell)
	(exec-path-from-shell-initialize))

(progn "install other packages"
  (setq yh/my-packages
	'(magit use-package browse-kill-ring session color-moccur auto-complete session
					helm open-junk-file projectile py-autopep8 yasnippet
					helm-projectile flycheck equally-spaced))
  (when (executable-find "hg")
    (add-to-list 'yh/my-packages 'yatex))
  (el-get 'sync yh/my-packages)
  (el-get-cleanup yh/my-packages))

(progn "backup"
  (setq make-backup-files t)
  (setq backup-directory-alist
	(cons (cons "\\.*$" (expand-file-name "~/backup"))
	      backup-directory-alist)))

(progn "global setting"
	(add-hook 'before-save-hook 'delete-trailing-whitespace)
	(setq-default tab-width 2)
	(show-paren-mode))

(use-package dabbrev
  :config
  (setq dabbrev-case-fold-search nil))

(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit))

(use-package equally-spaced)

(use-package hideshow
  :init
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode))

(use-package helm-config
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-idle-delay 0.1)
  (setq helm-input-idle-delay 0.1)
  (setq helm-delete-minibuffer-contents-from-point t)
  (setq helm-ff-auto-update-initial-value nil)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (global-unset-key (kbd "C-x c"))

  :bind (("M-x" . helm-M-x)
				 ("M-y" . helm-show-kill-ring)
				 ("C-x C-f" . helm-find-files)
				 ("C-c h" . helm-command-prefix)
				 ("C-c h o" . helm-occur)))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package py-autopep8
  :config
  (setq py-autopep8-options
	'("--max-line-length=300"
	  "--ignore=E402")))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package helm-projectile
	:config
	(helm-projectile-on))

(use-package flycheck
	:config
	(setq flycheck-check-syntax-automatically
				'(save idle-change mode-enabled))
	(setq flycheck-idle-change-delay 1)
	(add-hook 'after-init-hook #'global-flycheck-mode)
	(eval-after-load 'flycheck
		'(flycheck-add-mode 'html-tidy 'web-mode))
	)

(progn "global key"
  (global-set-key (kbd "C-x C-j") 'dired-jump)
  (global-set-key (kbd "M-u") 'revert-buffer))

(progn "shell script"
	(add-hook 'sh-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-j") 'yh/sh-insert-var)))
	(defun yh/sh-insert-var (var-name)
  (interactive "svariable name:")
  (insert "${" var-name "}")))

(progn "Makefile"
	(add-hook 'makefile-gmake-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-j") 'yh/make-insert-var)))
	(defun yh/make-insert-var (var-name)
  (interactive "svariable name:")
  (insert "$(" var-name ")")))

(progn "python"
	(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
	(setq flycheck-flake8-maximum-line-length 200))

(progn "emacs-lisp"
	(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (add-hook 'before-save-hook 'equally-spaced-make-gap-buffer :local t)))
	(add-hook 'emacs-lisp-mode-hook
						'hs-hide-all 100))

(progn "yatex"
	(setq auto-mode-alist
				(cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
	(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
	(add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode -1)))

	(defvar tex-command "tex2pdf"
		"*Default command for typesetting LaTeX text.")

	;; スクリプト挿入
	(defun yatex-insert-script (prefix script)
		(insert (concat prefix "{" script "}")))
	(defun yatex-insert-subscript (script)
		(interactive "sscript: ")
		(yatex-insert-script "_" script))
	(defun yatex-insert-superscript (script)
		(interactive "sscript: ")
		(yatex-insert-script "^" script))
	(add-hook 'yatex-mode-hook
						'(lambda ()
							 (local-set-key "\C-c\C-f" 'yatex-insert-subscript)
							 (local-set-key "\C-c\C-g" 'yatex-insert-superscript)
							 (local-set-key "\C-\M-a" 'foiltex-previous-page)
							 (local-set-key "\C-\M-e" 'foiltex-next-page))))

(when (eq window-system 'ns)
	(setq ns-command-modifier (quote meta))
	(setq ns-alternate-modifier (quote super))
	(global-set-key (kbd "C-;") 'helm-mini))

(when (not window-system)
	(global-set-key (kbd "C-c m") 'helm-mini))

;;; Local Variables:
;;; equally-spaced-width: 1
;;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;; End:
