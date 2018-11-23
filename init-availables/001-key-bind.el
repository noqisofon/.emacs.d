;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;; i-search for japanese
(define-key isearch-mode-map (kbd "C-k") 'isearch-edit-string)
;;
;; M-g で指定行にカーソルを飛ばします。
(global-set-key (kbd "M-g")
                '(lambda (x)
                   (interactive "nLine to goto: ")
                   (goto-line x)))
;;
;; C-\ でアンドゥできるようにします。
(global-set-key (kbd "C-\\") 'undo)
;;
;; C-\ でアンドゥできるようにします。
(global-set-key (kbd "C-\\") 'undo)
;; *** comment/uncomment-regeon
;; C-x ; でコメントアウト
;; C-x : でコメントをはずす
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)
;;
;; Enter キーの代わりに C-m を押すことで自動インデントを行ないます。
(global-set-key (kbd "C-m") 'newline-and-indent)
;;
;; Enter キーで改行しない時のために、C-j で開業することができます。
(global-set-key (kbd "C-j") 'newline)
;;
;; C-c t でテンポラリバッファを作成します。
(global-set-key (kbd "C-c t") 'create-temporary-buffer)
;;
;; C-x v f で 現在選択しているバッファの名前を挿入します。
(global-set-key (kbd "C-x v f") 'get-selected-buffer-name)
;;
;; C-x t d でカーソルの後に現在の時刻を ISO 8601 形式で挿入します。
(global-set-key (kbd "C-x t d") 'insert-iso8601format-datetime)
;;
;; 最近使ったファイルを別のバッファ領域に表示します。
(define-key global-map (kbd "C-c r") 'recentf-open-files)
;;
;; [zenkaku-hankaku] で入力モードを切り替える。
;; # んだけど、今現在、デスクトップ側の IM(input method) に割り込まれて使えないｗ
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)

(provide '001-key-bind)
