;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:
(push '("\\.md$"       . markdown-mode) auto-mode-alist)
(push '("\\.mrd$"      . markdown-mode) auto-mode-alist)
(push '("\\.mard$"     . markdown-mode) auto-mode-alist)
(push '("\\.markdown$" . markdown-mode) auto-mode-alist)

(setq markdown-command "markded")

(lazyload (markdown-mode) "markdown-mode"
 ;; 最近の markdown-mode はコードら辺のフォントファミリーが違うので、同じにする。
 ;; しかしながら、グローバルなフォント設定と異なる可能性があるため、その辺が課題である。
 (let ((font-family-name "Ricty")
       (font-size        (* 10 10)))
   (set-face-attribute 'markdown-code-face        nil :family font-family-name :height font-size)
   (set-face-attribute 'markdown-inline-code-face nil :family font-family-name :height font-size)))

(provide '010-markdown)
