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
          '(session use-package browse-kill-ring color-moccur session
                    helm open-junk-file projectile py-autopep8 yasnippet
                    helm-projectile flycheck equally-spaced ace-window
                    web-mode company-mode tide s dakrone-theme markdown-mode
                    json-mode prettier-emacs rjsx-mode yaml-mode git-ps1-mode
                    undo-tree smartparens dired-k dash f online-judge elpy
                    dockerfile-mode gnu-elpa-keyring-update lsp-java
                    lsp-ui which-key))))

  (when (executable-find "hg")
    (add-to-list 'yh/my-packages 'yatex))
  (when (executable-find "makeinfo")
    (add-to-list 'yh/my-packages 'magit))

  (el-get 'sync yh/my-packages)
  (el-get-cleanup yh/my-packages))

;;; Local Variables:
;;; equally-spaced-width: 1
;;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;; End:
