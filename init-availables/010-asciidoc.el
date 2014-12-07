;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:
(push '("\\.ascii" . asciidoc-mode) auto-mode-alist)
(push '("\\.asciidoc" . asciidoc-mode) auto-mode-alist)

(require-if-exists
 asciidoc-mode)
