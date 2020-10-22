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

(el-get)

;;; Local Variables:
;;; equally-spaced-width: 1
;;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;; End:
