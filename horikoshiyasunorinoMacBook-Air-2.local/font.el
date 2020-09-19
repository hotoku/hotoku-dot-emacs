(when (> (x-display-pixel-width) 2000)
  (message "This display has high resolution. Setting large font.")
  (yh/use-large-font t))
