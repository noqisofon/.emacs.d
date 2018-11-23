;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:
(eval-when-compile
  (require 'cl))

;; テンプレートが入っているディレクトリを指定します。
(setq auto-insert-directory "~/Templates/auto-insert/")

(require-if-exists autoinsert)

;; 書くファイルによってテンプレートを切り替えるようにします。
(setq auto-insert-alist
      (nconc '(
               ("\\.c$"   . ["template.c" my-template])
               ("\\.h$"   . ["template.h" my-template])
               ("\\.cpp$" . ["template.cpp" my-template])
               ("\\.hxx$" . ["template.hxx" my-template])
               ) auto-insert-alist))

(defvar template-replacements-alists
  '(
    ("%file%"              . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-with-out-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%"     . (lambda () (format "__%s_%s__" (upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))) (upcase "h"))))
    ))

(defun my-template ()
  (time-stamp)
  (mapc #'(lambda (c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacement-alists)
  (goto-char (point-max))
  (message "done."))

(add-hook 'find-file-not-found-hooks 'auto-insert)

(provide '000-autoinsert)
