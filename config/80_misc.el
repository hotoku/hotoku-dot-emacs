;;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))




;;; via. http://d.hatena.ne.jp/rubikitch/20100210/emacs#tb
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))




;;; open junk file
(require 'open-junk-file)
;; howmの中にファイル作成
(setq open-junk-file-format "~/howm/junk/%Y/%m/%d_%H%M%S.")
;; howm用のタグを挿入
(defvar open-junk-ext-tags-alist
  '(("el" ";;" "ELISP")
    ("cpp" "//" "CPP")
    ("r" "#" "R")
    ("py" "#" "PYTHON")
    ("hs" "--" "HASKELL")
    ("scm" ";;" "SCHEME")))
(defadvice open-junk-file
  (after open-junk-file-insert-howm-comment-advice activate)
  "After open-junk-file, insert a tag into the opened buffer
to be searched by howm."
  (let* ((ext (replace-regexp-in-string "^.*\\.\\([^\\.]+\\)$" "\\1" buffer-file-name))
         (asc (assoc ext open-junk-ext-tags-alist))
         (prefix (cadr asc))
         (tag (caddr asc)))
    (insert prefix)
    (insert " [" tag "] ")))




;;; anything git
(defun anything-c-sources-git-project-for (pwd)
  (loop for elt in
        '(("Modified files (%s)" . "--modified")
          ("Untracked files (%s)" . "--others --exclude-standard")
          ("All controlled files in this project (%s)" . ""))
        collect
        `((name . ,(format (car elt) pwd))
          (init . (lambda ()
                    (unless (and ,(string= (cdr elt) "") ;update candidate buffer every time except for that of all project files
                                 (anything-candidate-buffer))
                      (with-current-buffer
                          (anything-candidate-buffer 'global)
                        (insert
                         (shell-command-to-string
                          ,(format "git ls-files $(git rev-parse --show-cdup) %s"
                                   (cdr elt))))))))
          (candidates-in-buffer)
          (type . file))))
(defun anything-git-project ()
  (interactive)
  (let* ((pwd default-directory)
         (sources (anything-c-sources-git-project-for pwd)))
    (anything-other-buffer sources
     (format "*Anything git project in %s*" pwd))))
(define-key global-map (kbd "C-;") 'anything-git-project)



;;; foiltex
(defun foiltex-insert-new-page ()
  (interactive)
  (insert "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\myhead{}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
")
  (forward-line -2)
  (beginning-of-line)
  (forward-char 8))


