;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; lexical-binding: t; -*-

;;; 042-scheme.el ---

;; Copyright (C) 2014  ned rihine

;; Author: ned rihine <ned.rihine@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(setq scheme-program-name "guile -s ")

;; ruby モード。
(push '("\\.scm$" . scheme-mode) auto-mode-alist)

(lazyload (scheme-mode) "scheme")
(lazyload (run-scheme) "run-scheme")
(require-if-exists scheme-complete)

(cond ((string= scheme-program-name "csi")
       (load "043-chicken"))

      ((string= scheme-program-name "gosh")
       (load "043-gauche"))

      ((string= scheme-program-name "gsi")
       (load "043-gambit")))

;; といいたいところなんだけど、quack はなんだかおかしいのでコメントアウト。
;; (when (not (string= scheme-program-name "csi"))
;;   ;; csi じゃなかったら quack にします。
;;   (require-if-exists quack))

(add-hook 'scheme-mode-hook (lambda ()
                              (setq lisp-body-indent 2)
                              (when (featurep 'scheme-complete)
                                (make-local-variable 'eldoc-documentation-function)
                                (setq lisp-indent-function 'scheme-smart-indent-function)
                                (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
                                (eldoc-mode))))

(provide '042-scheme)
;;; 042-scheme.el ends here
