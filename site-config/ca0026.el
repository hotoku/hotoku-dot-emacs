;; http://minus9d.hatenablog.com/entry/20131103/1383475472

;; 英語
(set-face-attribute 'default nil
                    :family "Menlo" ;; font
                    :height 120)     ;; font size

;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font

;; 半角と全角の比を1:2にしたければ
(setq face-font-rescale-alist
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; Mac用フォント設定

