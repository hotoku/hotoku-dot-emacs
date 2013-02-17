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
