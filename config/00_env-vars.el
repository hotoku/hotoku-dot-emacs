(require 'cl)
(let* ((added-dirs (list "~/Dropbox/script"
			 "~/bin"
			 "/opt/local/bin"
			 "/opt/local/sbin"
			 "/usr/local/bin")))
  (setq exec-path (append added-dirs exec-path))
  (setenv "PATH"
	  (reduce (lambda (a b) (concat a ":" b))
		  (append added-dirs
			  (list (getenv "PATH"))))))
(setenv "PYTHONPATH" "")
(setenv "CPLUS_INCLUDE_PATH"
	(concat '"/opt/local/include:" (getenv "CPLUS_INCLUDE_PATH")))
(setenv "CPLUS_LIBRARY_PATH"
	(concat '"/opt/local/lib:" (getenv "CPLUS_LIBRARY_PATH")))
(setenv "LIBRARY_PATH"
	(concat '"/opt/local/lib:" (getenv "LIBRARY_PATH")))
(setenv "LANG" "ja_JP.UTF-8")
