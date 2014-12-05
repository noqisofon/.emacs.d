;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

(load "000-platform")

;; カレントディレクトリの位置を $HOME に設定します。
(cd "~/")

(let ((language "Japanese"))
  (set-language-environment language))

;; 起動時のメッセージを表示しないようにします。
(setq inhibit-startup-message t)
;; *scratch* バッファの中身を空にします。
(setq initial-scratch-message nil)

;; エラー時に画面をフラッシュさせないようにします。
(setq ring-bell-function 'ignore)

(setq visible-bell nil)

;; ログの記録量を 100000 に増やします。
(setq message-log-max 100000)
;; 履歴の長さを 100000 に増やします。
(setq history-length 100000)

;; ツールバーを消したままにします。
(tool-bar-mode 0)

;; スクロールバーを右側に表示します。
(set-scroll-bar-mode 'right)

;; line number を表示します。
(line-number-mode t)

;; column numer を表示します。
(column-number-mode t)

