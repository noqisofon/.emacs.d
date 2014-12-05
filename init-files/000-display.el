;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; 画面
(let ((frame-top 5)
      (frame-left 5)
      (frame-width 170)
      (frame-height 20)
      (frame-alpha 95)
      (frame-cursor-type 'bar))
  (setq default-frame-alist 
        (append `((top . ,frame-top)
                  (left . ,frame-left)
                  (width . ,frame-width)
                  (height . ,frame-height)
                  (alpha . ,frame-alpha)
                  (cursor-type . ,frame-cursor-type))
                default-frame-alist)))

;; 色の設定を有効にします。
(global-font-lock-mode t)

;;; hl-line
;;
;; 現在カーソルがある行をハイライトします。
(when (global-hl-line-mode 1)
  (let ((alice-blue "#f0f8ff"))
    ;; 現在カーソルがある行の背景を AliceBlue(#f0f8ff) にします。
    (set-face-attribute 'hl-line nil :background alice-blue)))

