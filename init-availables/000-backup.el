;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:

;;; バックアップファイル関係
;;
;; バックアップファイルを作成します。
(setq backup-inhibited nil)

;; 簡易バージョンコントロール機能を有効にします。
(setq version-control t)

(when version-control
  ;; 新しいものを 12 つまで残すようにします。
  (setq kept-new-version 12)
  ;; 古いものを 12 つまで残すようにします。
  (setq kept-old-version 12)
  ;; 古いバージョンを消す際、Emacs から尋ねないようにします。
  (setq delete-old-versions t)

;;; オートセーブファイル関係
;;
;; 
;; 
;; 終了時にオートセーブファイルを削除しないようにします。
(setq delete-auto-save-files nil)
