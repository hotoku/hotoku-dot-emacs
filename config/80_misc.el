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
    ("scm" ";;" "SCHEME")
    ("c" "//" "C")))
(defadvice open-junk-file
  (after open-junk-file-insert-howm-comment-advice activate)
  "After open-junk-file, insert a tag into the opened buffer
to be searched by howm."
  (let* ((ext (replace-regexp-in-string "^.*\\.\\([^\\.]+\\)$" "\\1" buffer-file-name))
         (asc (assoc ext open-junk-ext-tags-alist))
         (prefix (cadr asc))
         (tag (caddr asc)))
    (insert prefix)
    (insert " %" tag)))




;;; gomi file
(defun gomi-file ()
  (interactive)
  (let* ((file (format-time-string "/tmp/%y%m%d%H%M%S.")))
    (find-file (read-string "extension? " file))))




;;; run as script
(defun run-file-as-script (&optional args)
  (interactive "P")
  (let* ((file (buffer-file-name))
         (command (if (boundp 'run-test-target) (symbol-name run-test-target) file)))
    (shell-command (format "chmod u+x %s" file))
    (if args (setq command (format "%s %s" command (read-string "input arguments: "))))
    (shell-command command)))
