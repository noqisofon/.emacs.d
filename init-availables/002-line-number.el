;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;; ** 行番号をデフォルトで表示します。
(when emacs22-p
  ;; 23 以上から linum.el が入っているので要りませんが、Meadow3 は 22 なので
  ;; require-if-exists が必要です。
  (require-if-exists linum))

(require-if-exists linum+)

;; デフォルトで linum-mode を有効にします。
(add-hook-lambda 'find-file-hook nil
                 (linum-mode 1))
(when windows-nt-p
  ;; 7 桁分の領域を確保して行番号を入れます。
  (setq linum-format "%7d"))

(global-linum-mode 1)

(provide '002-line-number)
