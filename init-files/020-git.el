;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:
(lazyload (git-commmit-mode) "git-commmit-mode")
(lazyload (git-rebase-mode) "git-rebase-mode")
(lazyload (gitattributes-mode) "gitattributes-mode")
(lazyload (gitconfig-mode) "gitconfig-mode")
(lazyload (gitignore-mode) "gitignore-mode")

(push '("^.gitattributes$" . gitattributes-mode) auto-mode-alist)
(push '("^.gitconfig$" . gitconfig-mode) auto-mode-alist)
(push '("^.gitignore$" . gitignore-mode) auto-mode-alist)
