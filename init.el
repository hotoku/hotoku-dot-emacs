;; -*- lexical-binding: t -*-

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
                    web-mode company-mode tide s dakrone-theme markdown-mode
                    json-mode prettier-emacs rjsx-mode yaml-mode git-ps1-mode))))

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
    (other-window 1))
  (defun yh/dired-do-open (&optional arg)
    "In dired, invoke /usr/bin/open on the marked files.
If no files are marked or a specific numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file."
    (interactive "P")
    (let ((files (dired-get-marked-files nil arg)))
      (apply 'start-process "open_ps" nil "open" files)))
  (defun yh/insert-hash ()
    "Insert hash value of buffer string at current point. Intended using for debugging"
    (interactive)
    (insert (substring (secure-hash 'md5 (buffer-string)) 0 10)))
  (defvar yh/no-indent-extension-list '("yml"))

  (defun yh/before-save ()
    (interactive)
    (when (not (member (file-name-extension buffer-file-name) yh/no-indent-extension-list))
      (indent-region (point-min) (point-max) nil))
    (delete-trailing-whitespace))
  (defun yh/current-line ()
    (buffer-substring (point-beginning-of-line) (point-end-of-line)))
  (defun yh/all-lines ()
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line)
      (let ((ret '()))
        (while (not (= (point) (point-min)))
          (setq ret (cons (yh/current-line) ret))
          (forward-line -1))
        (cons (yh/current-line) ret))))
  (defun yh/reduce (f ls)
    (let ((len (length ls)))
      (cond
       ((= 0 len) nil)
       ((= 1 len) (car ls))
       (t (let ((ret (car ls)))
            (while (cdr ls)
              (setq ret (funcall f ret (cadr ls)))
              (setq ls (cdr ls)))
            ret)))))
  (defun yh/join (s ss)
    (yh/reduce (lambda (a b) (concat a s b)) ss)))

(yh/config "global setting"
  (add-hook 'before-save-hook 'yh/before-save)
  (setq-default
   tab-width 2
   indent-tabs-mode nil
   truncate-lines t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (show-paren-mode)
  (server-start)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode)
  (blink-cursor-mode -1))

(use-package generator
  :config
  (iter-defun yh/enumerate (iter)
    (let ((i 0))
      (iter-do (val iter)
        (iter-yield (cons i val))
        (setq i (1+ i)))))
  (iter-defun yh/filter (iter pred)
    (iter-do (val iter)
      (if (funcall pred val) (iter-yield val))))
  (iter-defun yh/iter-list (ls)
    (while ls
      (iter-yield (car ls))
      (setq ls (cdr ls))))
  (defun yh/iter-last (iter)
    (let ((ret nil))
      (iter-do (val iter)
        (setq ret val))
      ret)))

(use-package git-ps1-mode
  :config
  (let ((file (expand-file-name "site-local/git-ps1-mode.el" user-emacs-directory)))
    (when (file-exists-p file)
      (load file)
      (add-hook 'dired-mode-hook 'git-ps1-mode))))

(use-package json-mode
  :mode
  (("\\.json\\'" . json-mode)))

(use-package makefle
  :no-require t
  :config
  (defun yh/makefile-indent-line ()
    "https://emacs.stackexchange.com/questions/3074/customizing-indentation-in-makefile-mode"
    (save-excursion
      (forward-line 0)
      (cond
       ;; keep TABs
       ((looking-at "\t")
        t)
       ;; indent continuation lines to 4
       ((and (not (bobp))
             (= (char-before (1- (point))) ?\\))
        (delete-horizontal-space)
        (indent-to 4))
       ;; delete all other leading whitespace
       ((looking-at "\\s-+")
        (replace-match "")))))

  (add-hook 'makefile-mode-hook
            (lambda ()
              (setq-local indent-line-function 'yh/makefile-indent-line))))

(use-package markdown-mode
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.md.jinja\\'" . markdown-mode))
  :config
  (custom-set-faces
   '(markdown-code-face ((t (:inherit fixed-pitch :background "ivory" :foreground "SlateGray4"))))
   '(markdown-header-face ((t (:inherit font-lock-function-name-face :family "MeiryoKe_UIGothic"))))
   '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0 :underline t))))
   '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
   '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
   '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))))

(use-package prettier-js
  :config
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

(use-package rjsx-mode
  :mode
  ((".*\\.jsx?\\'" . rjsx-mode)))

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
  :bind
  (("C-c C-n" . flycheck-next-error)
   ("C-c C-p"  . flycheck-previous-error))

  :config
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-idle-change-delay 1
   flycheck-flake8-maximum-line-length 200)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (flycheck-add-mode 'html-tidy 'web-mode)
  (with-eval-after-load "python"
    (define-key python-mode-map (kbd "C-c C-p") 'flycheck-previous-error)))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

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
  (setq flycheck-flake8-maximum-line-length 200)
  (defun yh/python-do-insert-import (line)
    "Insert import sentence at the bottom of import lines"
    (let* ((last-line (yh/iter-last (yh/filter
                                     (yh/enumerate
                                      (yh/iter-list (yh/all-lines)))
                                     '(lambda (x)
                                        (yh/python-import-linep (cdr x))))))

           (line-num (car last-line)))
      (save-excursion
        (beginning-of-buffer)
        (forward-line (1+ line-num))
        (insert line "\n"))))
  (defun yh/python-import-linep (s)
    (string-match ".*import [0-9a-zA-Z]+" s))
  (defun yh/python-import (module)
    (interactive "Mmodule: ")
    (yh/python-do-insert-import (yh/join " " `("import" ,module))))
  (defun yh/python-import-from (module object)
    (interactive "Mmodule: \nMobject: ")
    (yh/python-do-insert-import (yh/join " " `("from" ,module "import" ,object)))))

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
    (if (ignore-errors (hs-already-hidden-p)) (hs-show-block)
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
    (set-face-background 'default "#202040")

    ;; change default directory for C-x C-f
    (when (version< "27.0" emacs-version)
      (defun ad:helm-find-files (f prompt)
        "When uploading to Emacs 27.1, default directory for buffers like *Emacs* changes from ~ to /.
This is inconvinient when opening file at the beginning of Emacs session."
        (when (equal default-directory "/")
          (setq default-directory "~/"))
        (funcall f prompt))
      (advice-add 'helm-find-files :around #'ad:helm-find-files))

    ;; `open` file by input 'z'
    (eval-after-load "dired"
      '(define-key dired-mode-map "z" 'yh/dired-do-open))))

(yh/config "when terminal"
  (when (not window-system)
    (global-set-key (kbd "C-c m") 'helm-mini)
    (menu-bar-mode -1)))

;;; Local Variables:
;;; equally-spaced-width: 1
;;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;; End:
