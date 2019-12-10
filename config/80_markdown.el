(setq markdown-command "github-markup")
(setq markdown-command-needs-filename t)

;; htmlに仕込むCSS
(setq markdown-css-paths (list (expand-file-name "~/.emacs.d/resource/github-markdown.css")))

(setq markdown-preview-stylesheets
      (list (expand-file-name "~/.emacs.d/resource/github-markdown.css")))
