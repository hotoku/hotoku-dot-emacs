;; -*- lexical-binding: t -*-

(when (< emacs-major-version 27)
  (load (concat (expand-file-name user-emacs-directory) "early-init.el")))

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
          '(session use-package browse-kill-ring color-moccur session
                    helm open-junk-file projectile py-autopep8 yasnippet
                    helm-projectile flycheck equally-spaced ace-window
                    web-mode company-mode tide s dakrone-theme markdown-mode
                    json-mode prettier-emacs rjsx-mode yaml-mode git-ps1-mode
                    undo-tree smartparens dired-k dash f online-judge elpy
                    dockerfile-mode gnu-elpa-keyring-update lsp-java
                    lsp-ui which-key helm-lsp))))

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
  (defvar yh/no-indent-extension-list '("yml" "sql" "py"))
  (defvar yh/no-indent-directory-list `(,(expand-file-name (concat user-emacs-directory "snippet"))))
  (defvar yh/indent-before-save t)
  (defun yh/head? (s1 s2)
    (let* ((l (length s1))
           (s3 (cond ((< l (length s2)) (substring s2 0 l))
                     (t s2))))
      (string= s1 s3)))
  (defun yh/before-save ()
    (interactive)
    (when (and (not (member (file-name-extension buffer-file-name) yh/no-indent-extension-list))
               (not (seq-some (lambda (s) (yh/head? s (buffer-file-name))) yh/no-indent-directory-list))
               yh/indent-before-save)
      (indent-region (point-min) (point-max) nil))
    (delete-trailing-whitespace)
    (when (string= (buffer-substring (point-min) (+ (point-min) 2)) "#!")
      (message "true")
      (yh/make-executable)))
  (defun yh/no-indent-on-save ()
    (interactive)
    (setq-local yh/indent-before-save nil))
  (defun yh/current-line ()
    (buffer-substring (yh/point-beginning-of-line) (point-end-of-line)))
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
    (yh/reduce (lambda (a b) (concat a s b)) ss))
  (defun yh/make-executable ()
    (let ((fn (buffer-file-name))
          (process-connection-type nil))
      (start-process "yh/make-executable" nil "chmod" "u+x" fn )))
  (defun yh/point-beginning-of-line ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (point))))

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
  (blink-cursor-mode -1)
  (setenv "LANG" "ja_JP.UTF-8"))

(yh/config "global key"
  (global-set-key (kbd "C-x C-j") 'dired-jump)
  (global-set-key (kbd "M-u") 'revert-buffer)
  (global-set-key (kbd "C-M-f") 'sp-forward-slurp-sexp)
  (global-set-key (kbd "C-M-b") 'sp-barckward-slurp-sexp))

(yh/config "shell script"
  (add-hook 'sh-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-j") 'yh/sh-insert-var)
               (setq-local after-save-hook (cons 'yh/make-executable after-save-hook))))
  (defun yh/sh-insert-var (var-name)
    (interactive "svariable name:")
    (insert "${" var-name "}")))

(yh/config "Makefile"
  (defun yh/makefile-indent-line ()
    "https://emacs.stackexchange.com/questions/3074/customizing-indentation-in-makefile-mode"
    (let ((bol (= (yh/point-beginning-of-line) (point)))
          (empty (string= (yh/current-line) "")))
      (message "-- %s %s %s" bol empty (point))
      (if bol
          (progn (when empty (insert "\t")))
        (save-excursion
          (forward-line 0)
          (cond
           ;; keep TABs
           ((looking-at "\t") t (message "1 %s" (point-max)))
           ;; indent continuation lines to 4
           ((and (not (bobp))
                 (= (char-before (1- (point))) ?\\))
            (delete-horizontal-space)
            (indent-to 4)
            (message "2"))
           ;; delete all other leading whitespace
           ((looking-at "\\s-+")
            (replace-match "")
            (message "3"))
           (t (message "4 %s" (point-max))))))))

  (add-hook 'makefile-mode-hook
            (lambda ()
              (setq-local indent-line-function 'yh/makefile-indent-line)))

  (add-hook 'makefile-gmake-mode-hook 'yh/setup-make-mode)
  (add-hook 'makefile-bsdmake-mode-hook 'yh/setup-make-mode)

  (defun yh/setup-make-mode ()
    (local-set-key (kbd "C-c C-j") 'yh/make-insert-var)
    (add-hook 'before-save-hook 'equally-spaced-make-gap-buffer :local t))
  (defun yh/make-insert-var (var-name)
    (interactive "svariable name:")
    (insert "$(" var-name ")")))

(yh/config "python"
  (add-hook 'python-mode-hook '(lambda () (py-autopep8-enable-on-save) (yh/before-save)))
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
    (yh/python-do-insert-import (yh/join " " `("from" ,module "import" ,object))))
  (add-hook 'python-mode-hook
            '(lambda ()
               (local-set-key (kbd "RET") 'yh/ret-hs)))
  (setq python-shell-interpreter "python3")
  (setq elpy-rpc-python-command "python3"))

(yh/config "emacs-lisp"
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook
                         '(lambda () (equally-spaced-make-gap-buffer)
                            (indent-region (point-min) (point-max)))
                         :local t)
               (local-set-key (kbd "RET") 'yh/ret-hs))))

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

(use-package lsp-mode
  :hook
  ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-completion-enable-additional-text-edit nil))

(use-package hydra)

(use-package lsp-ui)

(use-package which-key :config (which-key-mode))

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))

(use-package dap-java :ensure nil)

(use-package helm-lsp)

(use-package lsp-treemacs)

(use-package online-judge
  :init
  (setq online-judge-executable "/usr/local/bin/oj")
  (setq online-judge-directories '("~/onedrive/procon/atcoder/")))

(use-package dockerfile-mode
  :mode
  (("Dockerfile\\'" . dockerfile-mode)))

(use-package elpy
  :init
  (elpy-enable))

(use-package blog
  :no-require t
  :if (file-exists-p (expand-file-name "site-local/blog.el" user-emacs-directory))
  :config
  (load (expand-file-name "site-local/blog.el" user-emacs-directory))
  (defun yh/blog-publish ()
    "commit change and push to remote"
    (interactive)
    (message "pushing")
    (let ((fn (buffer-file-name))
          (process-connection-type nil)
          (buf (get-buffer-create "*yh/publish-blog*"))
          (ret-val nil))
      (call-process "git" nil buf t "add" fn)
      (call-process "git" nil buf t "commit" "-m" "publish")
      (setq ret-val (call-process "git" nil buf t "push"))
      (if (= ret-val 0)
          (message "pushed")
        (message "push failed"))))
  (defun yh/blog-new (title url)
    (interactive "sblog title: \nsurl: ")
    (let* ((y (format-time-string "%Y"))
           (m (format-time-string "%m"))
           (d (format-time-string "%d"))
           (fn (format "%s-%s-%s-%s.md" y m d url)))
      (find-file (expand-file-name fn yh/blog-posts-dir))
      (insert (format "---
layout: post
title: %s
date: %s-%s-%s %s +0900
tags:
---
" title y m d (format-time-string "%H:%M:%S")))
      (beginning-of-buffer)
      (search-forward "tags:")
      (insert " ")))
  (defun yh/blog-to-other (dir-nm)
    (let*  ((path (buffer-file-name))
            (ls (split-string path "/"))
            (fn (car (last ls)))
            (jekyll-root (seq-take ls (- (length ls) 2)))
            (new-fn (reduce (lambda (x y) (concat x "/" y))
                            (append jekyll-root (list dir-nm fn)))))
      (write-file new-fn)
      (when (file-exists-p path)
        (delete-file path))))
  (defun yh/blog-to-draft ()
    (interactive)
    (yh/blog-to-other "_drafts"))
  (defun yh/blog-to-post ()
    (interactive)
    (yh/blog-to-other "_posts"))
  (defun yh/blog-preview ()
    (interactive)
    (let* ((fpath (buffer-file-name))
           (fn (file-name-nondirectory fpath))
           (y-m-d (replace-regexp-in-string
                   "-" "/"
                   (replace-regexp-in-string
                    "^\\([0-9]+-[0-9]+-[0-9]+\\).*" "\\1" fn)))
           (body (replace-regexp-in-string
                  "^[0-9]+-[0-9]+-[0-9]+-\\(.+\\)\\.md\\'" "\\1" fn))
           (url (concat "http://localhost:4000/" y-m-d  "/" body))
           (buf (get-buffer-create "*yh/publish-blog*")))
      (call-process "open" nil buf t url))))

(use-package savehist
  :config
  (savehist-mode t))

(use-package smartparens-config
  :config
  (progn (show-smartparens-global-mode t))
  (add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
  ;; https://ebzzry.io/en/emacs-pairs/
  (defmacro yh/def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(loop for (key . val) in pairs
               collect
               `(defun ,(read (concat
                               "yh/wrap-with-"
                               (prin1-to-string key)
                               "s"))
                    (&optional arg)
                  (interactive "p")
                  (sp-wrap-with-pair ,val)))))

  (yh/def-pairs ((paren . "(")
                 (bracket . "[")
                 (brace . "{")
                 (single-quote . "'")
                 (double-quote . "\"")
                 (back-quote . "`"))))

(use-package undo-tree
  :config
  (global-undo-tree-mode t))

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

(use-package conf-mode
  :config
  (defun yh/indent-ssh-config-line ()
    (goto-char (line-beginning-position))
    (delete-horizontal-space)
    (unless (looking-at "Host\\b")
      (indent-to (indent-next-tab-stop 0))))
  (defun yh/ssh-config-setup-indent ()
    (when (s-ends-with-p "/.ssh/config" (buffer-file-name))
      (setq-local indent-line-function 'yh/indent-ssh-config-line)))
  (add-hook 'conf-space-mode-hook 'yh/ssh-config-setup-indent))

(use-package git-ps1-mode
  :config
  (let ((file (expand-file-name "site-local/git-ps1-mode.el" user-emacs-directory)))
    (when (file-exists-p file)
      (load file)
      (add-hook 'dired-mode-hook 'git-ps1-mode))))

(use-package json-mode
  :mode
  (("\\.json\\'" . json-mode)
   ("\\.geojson\\'" . json-mode)))

(use-package markdown-mode
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.md.jinja\\'" . markdown-mode))
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
  (add-hook 'markdown-mode-hook (lambda () (setq-local yh/indent-before-save nil)))
  (setq markdown-command "commonmarker --extension=autolink --extension=strikethrough --extension=table --extension=tagfilter --extension=tasklist"
        markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css"
                             "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content "
<style><!-- CSS HERE --></style>
<script src=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js\"></script>
<script>hljs.initHighlightingOnLoad();</script>"
        markdown-xhtml-body-preamble "<div class=\"markdown-body\">"
        markdown-xhtml-body-epilogue "</div>"
        markdown-command-needs-filename t)
  :bind
  (("C-c C-c x" . yh/blog-preview)))

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
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-hide-all 100)
  (add-hook 'python-mode-hook 'hs-minor-mode)
  (defun yh/ret-hs (&optional arg inter)
    (interactive)
    (if (ignore-errors (hs-already-hidden-p)) (hs-show-block)
      (newline arg inter))))

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
  (setq-default web-mode-code-indent-offset 2)

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  :mode (("\\.html\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)))

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

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
    (add-hook 'before-save-hook 'prettier-js :local t))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (setq typescript-indent-level 2)

  ;; for tsx
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)

  :bind
  (("C-c s" . tide-restart-server)))

(use-package dired-k
  :config
  (defun yh/dired-revert ()
    (interactive)
    (revert-buffer)
    (dired-k))
  :bind
  (:map dired-mode-map
        ("g" . yh/dired-revert)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("afe5e2fb3b1e295e11c3c22e7d9ea7288a605c110363673987c8f6d05b1e9972" default))
 '(package-selected-packages '(helm-lsp lsp-java)))

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
 '(markdown-markup-face ((t (:foreground "indian red")))))

(yh/config "when Emacs.app on Mac"
  (when (eq window-system 'ns)
    ;; meta key
    (setq ns-command-modifier (quote meta))
    (setq ns-alternate-modifier (quote super))

    ;; global key assign only for Emacs.app
    (global-set-key (kbd "C-c m") 'helm-mini)
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
      '(define-key dired-mode-map "z" 'yh/dired-do-open))

    ;; display size
    (defun yh/use-large-font (&optional no-recreate)
      (interactive)
      (setf (alist-get 'font default-frame-alist) "Monospace-16")
      (unless no-recreate
        (yh/recreate-frame)))
    (defun yh/use-small-font (&optional no-recreate)
      (interactive)
      (setf (alist-get 'font default-frame-alist) "Monospace-12")
      (unless no-recreate
        (yh/recreate-frame)))
    (defun yh/recreate-frame ()
      (let ((f (selected-frame)))
        (make-frame)
        (delete-frame f)))
    (let ((file (expand-file-name "site-local/font.el" user-emacs-directory)))
      (when (file-exists-p file)
        (load file)))))

(yh/config "when terminal"
  (when (not window-system)
    (global-set-key (kbd "C-c m") 'helm-mini)
    (menu-bar-mode -1)))

;;; todo
;; とある.pyファイルで使ったlocal variables設定。
;; 違和感なく使えたのでinit.elに書き加える。
;; # Local Variables:
;; # eval: (define-key python-mode-map (kbd "C-M-f") 'sp-forward-slurp-sexp)
;; # eval: (define-key python-mode-map (kbd "C-M-g") 'sp-forward-barf-sexp)
;; # eval: (smartparens-strict-mode)
;; # eval: (define-key python-mode-map (kbd "C-M-/") 'comment-region)
;; # eval: (define-key python-mode-map (kbd "C-M--") 'uncomment-region)
;; # End:

;;; Local Variables:
;;; equally-spaced-width: 1
;;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;; End:
