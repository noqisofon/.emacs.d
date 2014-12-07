;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; モードライン
;; 
;; line number を表示します。
(line-number-mode t)

;; column numer を表示します。
(column-number-mode t)

;; 時刻のフォーマット。
(setq display-time-string-forms
      '((let ((system-time-locale "C"))
          (format-time-string "%Y-%m-%dT%H:%M"))))

;; モードラインに現在時刻を表示します。
(if meadowp
    (display-time)
  ;; else
  (display-time-mode 1))
