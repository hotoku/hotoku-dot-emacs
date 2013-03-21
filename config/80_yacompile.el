;;; via. http://www.bookshelf.jp/soft/meadow_42.html#SEC647
(defvar current-comment-prefix "#" "*Default prefix string")
(defvar current-comment-suffix "" "*Default suffix string")
(defvar current-compiler "" "*Default suffix string")
(make-variable-buffer-local 'current-comment-prefix)
(make-variable-buffer-local 'current-comment-suffix)
(make-variable-buffer-local 'current-compiler)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-map "\C-c\C-c" 'YaCompile)
             (define-key c++-mode-map "\C-c\C-c" 'YaCompile)
             (setq current-comment-prefix "/*")
             (setq current-comment-suffix "\n */")
             (setq current-compiler "g++ -g")))
(add-hook 'haskell-mode-hook
          '(lambda ()
             (define-key haskell-mode-map "\C-c\C-c" 'YaCompile)
             (setq current-comment-prefix "-- ")
             (setq current-compiler "ghc")))




(defun YaCompile ()
  (interactive)
  (require 'compile)
  (let ((cmd compile-command))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (concat "^" (regexp-quote current-comment-prefix) "!\\(.*\\)$")
           nil t)
          (setq cmd (buffer-substring
                     (match-beginning 1) (match-end 1)))))
    (setq compile-command
          (read-from-minibuffer "Compile command: "
                                cmd nil nil
                                '(compile-history . 1))))
  (compile compile-command))




(defun yacompile-insert-command (&optional with-test-file)
  (interactive)
  (let* ((file-name (file-name-nondirectory buffer-file-name))
         (file-body (replace-regexp-in-string "\\.[^\\.]+$" "" file-name))
         (file-ext  (replace-regexp-in-string ".+\\.\\([^.]+\\)" "\\1" file-name))
         (exe-file  (concat file-body ".out")))
    (insert current-comment-prefix "! "
            "if " current-compiler " " file-name " -o " exe-file ";"
            " then ./" exe-file
            (if (or with-test-file current-prefix-arg)
                (concat " < " file-body ".test")
              "")
            ";"
            " fi" current-comment-suffix)))
