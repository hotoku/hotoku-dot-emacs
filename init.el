(defmacro yh/config (desc &rest body)
  `(progn ,@body))
(put 'yh/config 'lisp-indent-function 'defun)

(yh/config "install el-git"
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (add-to-list 'el-get-recipe-path "~/.emacs.d/recipes"))

(yh/config "exec-path-from-shell"
  (defvar yh/my-packages)
  (setq yh/my-packages '(exec-path-from-shell))
  (el-get 'sync yh/my-packages)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(yh/config "install other packages"
  (setq yh/my-packages
        (delete-dups
         (append
          yh/my-packages
          '(session use-package browse-kill-ring color-moccur auto-complete session
                    helm open-junk-file projectile py-autopep8 yasnippet
                    helm-projectile flycheck equally-spaced ace-window
                    web-mode company-mode tide s dakrone-theme))))
  (when (executable-find "hg")
    (add-to-list 'yh/my-packages 'yatex))
  (when (executable-find "makeinfo")
    (add-to-list 'yh/my-packages 'magit))

  (el-get 'sync yh/my-packages)
  (el-get-cleanup yh/my-packages))

(yh/config "backup"
  (setq make-backup-files t)
  (setq backup-directory-alist
        (cons (cons "\\.*$" (expand-file-name "~/backup"))
              backup-directory-alist)))

(yh/config "functions & macros"
  (defun other-window-or-split ()
    (interactive)
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1)))

(yh/config "global setting"
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq-default tab-width 2)
  (show-paren-mode)
  (setq-default indent-tabs-mode nil))

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
    '(flycheck-add-mode 'html-tidy 'web-mode)))

(use-package ace-window
  :bind (("C-c C-q" . ace-window)))

(use-package web-mode
  :config
  (setq-default web-mode-code-indent-offset 2))

(use-package company
  :config
  (setq company-idle-delay 0.1))

(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1)
    (add-hook 'before-save-hook 'tide-format :local t))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (setq typescript-indent-level 2)

  ;; for tsx
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(yh/config "global key"
  (global-set-key (kbd "C-x C-j") 'dired-jump)
  (global-set-key (kbd "M-u") 'revert-buffer))

(yh/config "shell script"
  (add-hook 'sh-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-j") 'yh/sh-insert-var)))
  (defun yh/sh-insert-var (var-name)
    (interactive "svariable name:")
    (insert "${" var-name "}")))

(yh/config "Makefile"
  (add-hook 'makefile-gmake-mode-hook 'yh/setup-make-mode)
  (add-hook 'makefile-bsdmake-mode-hook 'yh/setup-make-mode)

  (defun yh/setup-make-mode ()
    (local-set-key (kbd "C-c C-j") 'yh/make-insert-var)
    (add-hook 'before-save-hook 'equally-spaced-make-gap-buffer :local t))
  (defun yh/make-insert-var (var-name)
    (interactive "svariable name:")
    (insert "$(" var-name ")")))

(yh/config "python"
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  (setq flycheck-flake8-maximum-line-length 200))

(yh/config "emacs-lisp"
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook
                         '(lambda () (equally-spaced-make-gap-buffer)
                            (indent-region (point-min) (point-max)))
                         :local t)
               (local-set-key (kbd "RET") 'yh/ret-in-elisp)))
  (add-hook 'emacs-lisp-mode-hook
            'hs-hide-all 100)
  (defun yh/ret-in-elisp (&optional arg inter)
    (interactive)
    (if (hs-already-hidden-p) (hs-show-block)
      (newline arg inter))))

(yh/config "yatex"
  (setq auto-mode-alist
        (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
  (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  (add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode -1)))

  (defvar tex-command "tex2pdf"
    "*Default command for typesetting LaTeX text.")

  ;; スクリプト挿入
  (defun yatex-insert-script (prefix script)
    (let ((len (length script)))
      (cond ((= 1 len) (insert (concat prefix script)))
            ((< 1 len) (insert (concat prefix "{" script "}"))))))
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
               (local-set-key "\C-\M-e" 'foiltex-next-page)
               (add-hook 'before-save-hook 'equally-spaced-make-gap-buffer :local t))))

(yh/config "javascript"
  (setq js-indent-level 2))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("afe5e2fb3b1e295e11c3c22e7d9ea7288a605c110363673987c8f6d05b1e9972" default)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(yh/config "when Emacs.app on Mac"
  (when (eq window-system 'ns)
    ;; meta key
    (setq ns-command-modifier (quote meta))
    (setq ns-alternate-modifier (quote super))

    ;; global key assign only for Emacs.app
    (global-set-key (kbd "C-;") 'helm-mini)
    (global-set-key (kbd "C-.") 'other-window-or-split)
    (define-key global-map [?¥] [?\\])

    ;; theme
    (require 'dakrone-theme)
    (load-theme 'dakrone)
    (set-face-foreground 'font-lock-comment-face "#8AE234")
    (set-face-foreground 'font-lock-string-face "IndianRed")

    ;; change default directory for C-x C-f
    (when (version< "27.0" emacs-version)
      (defun ad:helm-find-files (f prompt)
        "When uploading to Emacs 27.1, default directory for buffers like *Emacs* changes from ~ to /.
This is inconvinient when opening file at the beginning of Emacs session."
        (when (equal default-directory "/")
          (setq default-directory "~/"))
        (funcall f prompt))
      (advice-add 'helm-find-files :around #'ad:helm-find-files))))

(yh/config "when terminal"
  (when (not window-system)
    (global-set-key (kbd "C-c m") 'helm-mini)))

;;; Local Variables:
;;; equally-spaced-width: 1
;;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;; End:
