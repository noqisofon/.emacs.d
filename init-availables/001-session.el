;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;; デフォルトでは 30 です。
(setq history-length t)

;; 前回閉じたときの位置にカーソルを復帰するようにします。
(setq session-undo-check -1)

(require-if-exists session)
;; これがないと file-name-history に 500 個保存するまえに max-string に達します。
(setq session-globals-max-string 1000000)

;; セッションファイルは ~/ に作成します。
(setq session-save-file "~/.session")

;; (via: http://d.hatena.ne.jp/gan2/20070624/1182686652)
(setq session-initialize '(de-sageplace session keys menus places)
      session-globals-include '((kill-ring 50)
                                (session-file-alist 500 t)
                                (file-name-history 10000)))
;; 初期化後に呼び出されるフックにセッションの初期化用関数を引っ掛けます。
(add-hook 'after-init-hook 'session-initialize)
