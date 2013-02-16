;;; load path 
(defconst my-elisp-directory "~/.emacs.d/site-lisp" "The directory for my elisp file.")
(dolist (dir (let ((dir (expand-file-name my-elisp-directory)))
	       (list dir (format "%s%d" dir emacs-major-version))))
  (when (and (stringp dir) (file-directory-p dir))
    (let ((default-directory dir))
      (setq load-path (cons default-directory load-path))
      (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons "~/.emacs.d/auto-install" load-path))

;;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-compatibility-setup)
;; (auto-install-update-emacswiki-package-name t)

;;; environment variables
(setq exec-path (append (list "~/Dropbox/script"
			      "~/bin"
			      "/opt/local/bin"
			      "/opt/local/sbin"
			      "/usr/local/bin")
			exec-path))
(setenv "PATH" (concat "~/Dropbox/script:~/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:"
		       (getenv "PATH")))
(setenv "PYTHONPATH" "")
(setenv "CPLUS_INCLUDE_PATH"
	(concat '"/opt/local/include:" (getenv "CPLUS_INCLUDE_PATH")))
(setenv "CPLUS_LIBRARY_PATH"
	(concat '"/opt/local/lib:" (getenv "CPLUS_LIBRARY_PATH")))
(setenv "LIBRARY_PATH"
	(concat '"/opt/local/lib:" (getenv "LIBRARY_PATH")))
(setenv "LANG" "ja_JP.UTF-8")

;;; meta, super key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;;; initial frame size
(cond
 ((string= (system-name) "horikoshi-yasunori-no-iMac.local")
  (setq initial-frame-alist
 	(append (list '(width . 235) '(height . 58)) initial-frame-alist))
  (split-window-horizontally)) ; use 2 pane
 ((string= (system-name) "horikoshi-yasunori-no-MacBook-Pro.local")
  (setq initial-frame-alist
	(append (list '(width . 157) '(height . 42)) initial-frame-alist))))
	
;;; initial frame settings
(setq initial-frame-alist
      (append
       (list
	'(top                  . 0) ;; 表示位置
	'(left                 . 0) ;; 表示位置
	'(foreground-color     . "grey20")
	'(background-color     . "grey")
	'(border-color         . "black")
	'(mouse-color          . "white")
	'(cursor-color         . "black")
	'(vertical-scroll-bars . nil)) ;; remove scroll bar
       initial-frame-alist))

;;; font
(add-to-list 'initial-frame-alist
	     '(font . "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
(create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo" 'unicode (font-spec :family "Hiragino Kaku Gothic ProN" ) nil 'append)
(add-to-list 'initial-frame-alist '(font . "fontset-menlokakugo"))
(setq face-font-rescale-alist '((".*Hiragino.*" . 1.2) (".*Menlo.*" . 1.0)))

;;; default frame
(setq default-frame-alist initial-frame-alist)

;;; color of string 
(set-face-foreground 'font-lock-string-face        "DarkRed")
(set-face-foreground 'font-lock-variable-name-face "IndianRed")
(set-face-foreground 'font-lock-constant-face      "RoyalBlue")

;;; dired-x
(load "dired-x")

;;; back up file
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
	    backup-directory-alist))

;;; ess
(require 'ess-site)

;;; align for R
(require 'align)
(add-to-list 'align-rules-list
             '(ess-assignment-operator
               (regexp . "\\(\\s-*\\)<?<-[^#\t\n]")
               (repeat . nil)
               (modes  . '(ess-mode))))
(add-to-list 'auto-mode-alist
             '("\\.runit" . R-mode))


;;; hatena 
(require 'simple-hatena-mode)
(setq simple-hatena-root "~/Dropbox/hatena")
(setq simple-hatena-default-id "hotoku")
(setq simple-hatena-time-offset 6)
(setq simple-hatena-bin "hw.pl")
(set-face-foreground 'simple-hatena-markup-face "DarkSlateBlue")
(defadvice simple-hatena-electric-asterisk
  (after simple-hatena-electric-asterisk-advice activate)
  "When the second * is inputted on the head of a line, 
remove time stamp which was inserted by the function"
  (let ((pos (line-beginning-position)))
    (if (and simple-hatena-use-timestamp-permalink-flag
	     (= (current-column) 13)
	     (string-match "\\*[0-9]+\\*\\*"
			   (buffer-substring pos (+ pos 13))))
	(delete-region pos (+ pos 11)))))
(add-hook 'simple-hatena-mode-hook
	  '(lambda () (local-set-key "\C-c\C-j" 'simple-hatena-insert-tex)))
(defun simple-hatena-insert-tex ()
  (interactive)
  (insert "[tex:]")
  (backward-char))

;;; hatena helper mode
(require 'html-helper-mode)
(require 'hatenahelper-mode)
(global-set-key "\C-xH" 'hatenahelper-mode)
(add-hook 'simple-hatena-mode-hook
	  '(lambda ()
	     ;; other hooks must be wrote here!
	     (hatenahelper-mode 1)))
(defadvice hatenahelper-insert-sonomama-textblock
  (after hatenahelper-insert-sonomama-textblock-advice activate)
  "When quote braces are inserted move point where language is input."
  (progn (previous-line)
	 (forward-char 2)))

;;; YaCompile
;;;
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

;;; uniquify
(require 'uniquify)

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

;;; yatex
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode -1)))

;;; tex コマンド
(defvar tex-command "mylatex.sh" ; "platex"
  "*Default command for typesetting LaTeX text.")

;;; 数式の色
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(YaTeX-font-lock-formula-face ((((class color) (background light)) (:foreground "DarkRed"))))
 '(YaTeX-font-lock-math-sub-face ((((class color) (background light)) (:foreground "DarkRed"))))
 '(YaTeX-font-lock-math-sup-face ((((class color) (background light)) (:foreground "DarkRed")))))

;;; スクリプト挿入
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
	     (local-set-key "\C-c\C-g" 'yatex-insert-superscript)))
	  


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
(global-set-key "\C-c,," 'howm-menu)
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
	 "~/dropbox/misc/codeforces/template/template.cpp"))
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
;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)
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
(global-set-key "\C-c\C-g" 'magit-status)


;;; via. http://d.hatena.ne.jp/rubikitch/20100210/emacs#tb
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-,") 'other-window-or-split)



;;;
(require 'elscreen)
(setq elscreen-display-tab nil)

;;;
(global-set-key [?\C-1] 'delete-other-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sql-sqlite-program "sqlite3")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))


;;; anything
(defvar org-directory "")
(defvar w3m-command "/opt/local/bin/w3m")
(require 'anything-startup)
