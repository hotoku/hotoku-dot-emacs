(require 'ucs-normalize)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)


(require 'multi-term)
(setq multi-term-program shell-file-name)
(setq term-unbind-key-list
      (append '("C-f" "C-b") term-unbind-key-list))
