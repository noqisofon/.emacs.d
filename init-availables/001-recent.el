;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;; 最近使ったファイルの一覧を表示します。
(recentf-mode t)

;;
(setq recentf-save-file "~/.recentf")
;;
(setq recentf-exclude '(".recentf"))
;; メニューに表示するファイル名を 1000 までにします。
(setq recentf-max-menu-items 1000)
;; 最大 15 ファイルまで記録します。
(setq recentf-max-saved-items 15)

(provide '001-recent)
