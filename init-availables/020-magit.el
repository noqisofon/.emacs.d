;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:
(lazyload (magit-status) "magit")

(global-set-key (kbd "C-x g") 'magit-status)

(provide '020-magit)
