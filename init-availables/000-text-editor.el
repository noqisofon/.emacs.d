;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;; 通常のインデントで半角スペースを使います。
(setq-default indent-tabs-mode nil)

;; リージョンを kill-ring に入れないで削除できるようにします。
(delete-selection-mode t)

;; 他のソフトで保存した内容を自動的に読み込み直します。
(global-auto-revert-mode 1)

;; オートセーブを有効にします。
(auto-save-mode t)

(setq line-move-visual t)

;; 最下行での下移動を 15 行にします。
(setq scroll-conservatively 15)

;; 対応するカッコを色表示します。
;;(when
(show-paren-mode 1)  
;;; theme で設定します。
;; 括弧の背景を灰色にします。
;;(set-face-background 'show-paren-match-face "gray85")

;;; auto-fill モード
(let ((fill-column 300))
  (set-fill-column fill-column))
;;(setq-default auto-fill-function 'do-auto-fill)
(setq auto-fill-mode nil)

;; EOB を表示します。
(setq-default indicate-empty-lines t)

(load "002-color-theme")
(load "002-linum")
