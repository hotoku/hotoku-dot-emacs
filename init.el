(let* ((elisp-dir "~/.emacs.d/site-lisp")
       (default-directory elisp-dir))
  (add-to-list 'load-path elisp-dir)
  (normal-top-level-add-subdirs-to-load-path))
(setq load-path (cons "~/.emacs.d/auto-install" load-path))





(require 'init-loader)
(setq init-loader-directory "~/.emacs.d/config")
(init-loader-load)











;;; dired-x
(load "dired-x")

;;; back up file
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
	    backup-directory-alist))







;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;; ini mode
(require 'ini-mode)
(setq auto-mode-alist 
      (append 
       '(("\\.[iI][nN][iI]$" . ini-mode))
       auto-mode-alist))

;;; move currrent buffer with shift + (arrow key)
(windmove-default-keybindings)

;;; server
(server-start)

	  


;;; show-paren-mode
(show-paren-mode)

;;; 動的略語補完
(setq dabbrev-case-fold-search nil)

;;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

;;; advice for shell script mode
(defadvice sh-mode (after my-shell-script-mode-hook activate)
  (progn
    (set-face-foreground 'sh-heredoc "navy")
    (local-set-key "\C-c\C-j" 'sh-insert-var)))
(defun sh-insert-var (var-name)
  (interactive "svariable name:")
  (insert "${" var-name "}"))


;;; dsvn -- subversion client
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(require 'vc-svn)
(require 'vc-svn17)

;;; howm
(setq howm-menu-lang 'ja)
(autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)


;;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet-0.6.1c/snippets")

;;; mdfind-dired
(require 'mdfind-dired)
(add-to-list 'auto-mode-alist
             '("\\.savedSearch\\'" . mdfind-dired-change-to-dired))

;;; カーソル位置のファイルを open で開く
(defun my-dired-do-open (&optional arg)
  "In dired, invoke /usr/bin/open on the marked files.
If no files are marked or a specific numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file."
  (interactive "P")
  (let ((files (dired-get-marked-files nil arg)))
    (apply 'start-process "open_ps" nil "open" files)))

(eval-after-load "dired"
  '(define-key dired-mode-map "z" 'my-dired-do-open))



;;; for Imaxima
(autoload 'imaxima "imaxima" "Image support for Maxima." t)

;;; input backslush
(define-key global-map [?¥] [?\\])

;;; scheme
(setq scheme-program-name "gosh -i")

;;;
(set-face-foreground 'modeline "black")
(set-face-background 'modeline "white")

;;;
(require 'open-junk-file)
(setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d_%H%M%S.")

;;;
(setq load-path (cons "~/dropbox/emacs/w3m/share/emacs/site-lisp/w3m" load-path))
(require 'w3m-load)

;;;
(require 'lispxmp)

;;; diff
(defun set-my-diff-mode-color ()
  (set-face-foreground 'diff-removed "Blue")
  (set-face-foreground 'diff-added "Red"))

(add-hook 'diff-mode-hook
	  'set-my-diff-mode-color)
(add-hook 'magit-status-mode-hook
	  'set-my-diff-mode-color)

;;; moccur
(require 'color-moccur)
(require 'moccur-edit)
(defadvice moccur-edit-change-file
 (after save-after-moccur-edit-buffer activate)
 (save-buffer))
(defun moccur-grep-current-word (dir)
  (interactive
   (list (moccur-grep-read-directory)))
  (let ((word (current-word)))
     (message "abc")
     (message word)
     (moccur-grep dir (list word))))



;;; not truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-c" 'anything-lisp-complete-symbol-partial-match)))

;;;
(defun pgcontest-insert-template ()
  (interactive)
  (yacompile-insert-command t)
  (insert "\n\n")
  (let ((codeforces-template-path
	 "~/.emacs.d/resource/contest-template.cpp"))
    (insert-file codeforces-template-path)))
(defun pgcontest-create-testfile ()
  (interactive)
  (let* ((test-file-path
	  (replace-regexp-in-string "\\.[^\\.]+$" ".test" buffer-file-name)))
    (shell-command (format "touch %s" test-file-path))
    test-file-path))
(defun pgcontest-prepare ()
  (interactive)
  (let* ((test-file
	  (pgcontest-create-testfile)))
    (pgcontest-insert-template)  
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (split-window-vertically)
    (other-window 1)
    (find-file test-file)
    (other-window 1)))


;;;
;; (require 'bash-completion)
;; (bash-completion-setup)

;;;
(require 'browse-kill-ring)

;;; git 
(add-to-list 'load-path "/opt/local/share/doc/git-core/contrib/emacs")
(require 'git)
(require 'git-blame)

;;;
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;;;
(require 'minibuf-isearch)

;;;
;; (require 'evernote-mode)
;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
;; (setq evernote-ruby-command "ruby1.9")


;;; haskell-mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;;; twittering-mode (via http://fukuyama.co/twittering-mode)
;; twittering-mode読み込み
(require 'twittering-mode)
;; 起動時パスワード認証 *要 gpgコマンド
(setq twittering-use-master-password t)
;; パスワード暗号ファイル保存先変更 (デフォはホームディレクトリ)
(setq twittering-private-info-file "~/.emacs.d/twittering-mode.gpg")
;; 表示する書式 区切り線いれたら見やすい
(setq twittering-status-format "%i @%s %S
%T
%p [%@]%r %R %f%L")
;; アイコンを表示する
(setq twittering-icon-mode t)
;; アイコンサイズを変更する *48以外を希望する場合 要 imagemagickコマンド
(setq twittering-convert-fix-size 24)
;; 更新の頻度（秒）
(setq twittering-timer-interval 40)
;; ツイート取得数
(setq twittering-number-of-tweets-on-retrieval 50)
;; o で次のURLをブラウザでオープン
(add-hook 'twittering-mode-hook
          (lambda ()
            (local-set-key (kbd "o")
               (lambda ()
                 (interactive)
                 (twittering-goto-next-uri)
                 (execute-kbd-macro (kbd "C-m"))
                 ))))


;;;
(require 'magit)


;;; via. http://d.hatena.ne.jp/rubikitch/20100210/emacs#tb
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))



;;;
(require 'elscreen)
(setq elscreen-display-tab nil)



;;;
(setq sql-sqlite-program "sqlite3")




;;; flymake
(require 'flymake)
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (flymake-mode t)))
(add-hook 'haskell-mode-hook
	  '(lambda ()
	     (flymake-mode t)))








;;; anything
(defvar org-directory "")
(defvar w3m-command "/opt/local/bin/w3m")
(require 'anything-startup)
