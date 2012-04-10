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
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

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
	(append (list '(width . 210) '(height . 49)) initial-frame-alist))
  (split-window-horizontally)) ; use 2 pane
 ((string= (system-name) "horikoshi-yasunori-no-MacBook-Pro.local")
  (setq initial-frame-alist
	(append (list '(width . 175) '(height . 44)) initial-frame-alist))))
	



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
;;; YaTeX like compile
;;;
;;; フックを使って current-comment-prefix をセットしておく。
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (define-key c-mode-map "\C-c\C-c" 'YaCompile)
	     (define-key c++-mode-map "\C-c\C-c" 'YaCompile)
	     (setq current-comment-prefix "/*")))
;;;
;;; とかね。そうすると、"/*!g++ -o test test.c" みたいな行をバッファ先
;;; 頭からサーチして、"/*!" 以降行末までをコンパイルのコマンドとして使
;;; うわけだ。あ、"/*!g++ -o test test.c" は行頭にないとダメよ。
;;;
(defvar current-comment-prefix "#" "*Default prefix string")
(make-variable-buffer-local 'current-comment-prefix)
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
(defun yacompile-insert-command ()
  (interactive)
  (let* ((file-name (file-name-nondirectory buffer-file-name))
	 (file-body (replace-regexp-in-string "\\.[^\\.]+$" "" file-name)))
    (insert "/*! if g++ -g " file-body ".cpp -o " file-body ".out; "
	    "then ./" file-body ".out < " file-body ".test; fi\n */")))


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
(add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode nil)))

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
  (set-face-foreground 'sh-heredoc "navy"))

;;; dsvn -- subversion client
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(require 'vc-svn)

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
(setq exec-path (cons "/opt/local/bin" exec-path))
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
(add-hook 'diff-mode-hook
	  '(lambda ()
	     (set-face-foreground 'diff-removed "Blue")
	     (set-face-foreground 'diff-added "Red")))


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

(defun codeforces-insert-template ()
  (interactive)
  (yacompile-insert-command)
  (insert "\n\n")
  (let ((codeforces-template-path
	 "~/dropbox/misc/codeforces/template/template.cpp"))
    (insert-file codeforces-template-path)))



;; (require 'bash-completion)
;; (bash-completion-setup)


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
(require 'anything-startup)
