;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; hl-line
;;
(require-if-exists hl-line+)

;; 現在カーソルがある行をハイライトします。
(when (global-hl-line-mode)
  ;; (let ((light-background-color "AliceBlue"))
  ;;   ;; 現在カーソルがある行の背景を AliceBlue(#f0f8ff) にします。
  ;;   (defface hlline-face
  ;;     `((((class color)
  ;;         (background dark))
  ;;        (:background "blue" :foreground "white"))
  ;;       (((class color)
  ;;         (background light))
  ;;        (:background ,light-background-color))
  ;;       (t
  ;;        ()))
  ;;     "*Face used by hl-line.")
    (setq hl-line-face 'highlight))

(provide '003-hl-line)
