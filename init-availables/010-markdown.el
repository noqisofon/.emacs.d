;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:
(push '("\\.md$" . markdown-mode) auto-mode-alist)
(push '("\\.mrd$" . markdown-mode) auto-mode-alist)
(push '("\\.mard$" . markdown-mode) auto-mode-alist)
(push '("\\.markdown$" . markdown-mode) auto-mode-alist)

(lazyload (markdown-mode) "markdown-mode")
