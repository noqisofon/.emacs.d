;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:
(require-if-exists server

                   (when (fboundp 'server-running-p)
                     (unless (server-running-p)
                       (server-start))))

(provide '000-emacs-client)
;;; ends here
