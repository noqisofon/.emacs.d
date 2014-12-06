;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil; -*-

(deftheme bright-forest
  "Bright Forest color theme")

(custom-theme-set-faces
 'bright-forest

 ;; 背景・文字・カーソル
 '(cursor ((t (:foreground "snow"))))
 '(default ((t (:foreground "#2b2b2b" :background "white"))))

 ;; エスケープされた文字
 '(escape-glyph ((t (:foreground "brown"))))

 ;; 選択範囲
 '(region ((t (:background "#adc3e8"))))

 ;; isearch でヒットした文字列
 '(isearch ((t (:foreground "lightskyblue1" :background "magenta3"))))
 '(isearch-fail ((t (:background "RosyBrown1"))))

 ;; モードライン
 '(mode-line ((t (:foreground "white" :background "black"))))
 '(mode-line-buffer-id ((t (:foreground nil :background nil))))
 '(mode-line-inactive ((t (:foreground "gray30" :background
                                       "gray85"))))

 ;; ハイライト
 '(highlight ((t (:foreground "gray08" :background "white"))))
 '(highlight-change ((t (:foreground "red1"))))
 '(highlight-change-delete ((t (:foreground "red1" :underline t))))
 '(hl-line ((t :background "alice blue")))

 ;; 行番号
 '(linum ((t (:foreground "grey50"))))

 ;; 関数名
 '(font-lock-function-name-face ((t (:foreground "dark olive green" :bold t))))

 ;; 型
 '(font-lock-type-face ((t (:foreground "forest green"))))

 ;; 変数名・変数の内容(文字列的な意味で)
 '(font-lock-variable-name-face ((t (:foreground "gray23"))))
 '(font-lock-doc-face ((t (:foreground "gray45"))))
 '(font-lock-string-face ((t (:foreground "blue"))))

 ;; コメント
 '(font-lock-comment-delimiter-face ((t (:foreground "gray55"))))
 '(font-lock-comment-face ((t (:foreground "gray55"))))

 ;; 特定のキーワード
 '(font-lock-keyword-face ((t (:foreground "green4" :bold t :weight bold))))

 ;; プリプロセッサ
 '(font-lock-preprocessor-face ((t (:bold t :weight bold :foreground "MediumPurple2"))))

     ;; 定数的な？
 '(font-lock-constant-face ((t (:foreground "dark green"))))

 ;; ビルトイン？(Lisp ではキーワード)
 '(font-lock-builtin-face ((t (:bold t :foreground "MediumPurple3" :weight bold))))

 ;; 括弧
 '(show-paren-match ((t (:background "gray85"))))
 '(show-paren-mismatch ((t (:foreground "white" :background "purple"))))
 )

;;; autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bright-forest)
