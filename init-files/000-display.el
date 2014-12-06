;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;; 色の設定を有効にします。
(global-font-lock-mode t)

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


(load "003-hl-line")
