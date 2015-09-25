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
(setq quack-default-program "gauche")

;; ruby モード。
(push '("\\.scm$" . scheme-mode) auto-mode-alist)

(lazyload (scheme-mode) "scheme-mode")
(lazyload (run-scheme) "run-scheme")
(require-if-exists scheme-complete)

(cond ((string= quack-default-program "csi")
       (load "043-chicken"))

      ((string= quack-default-program "gosh")
       (load "043-gauche"))

      ((string= quack-default-program "gsi")
       (load "043-gambit")))

(when (not (string= quack-default-program "csi"))
  (require-if-exists quack))

(add-hook 'scheme-mode-hook (lambda ()
                              (make-local-variable 'eldoc-documentation-function)
                              (setq lisp-indent-function 'scheme-smart-indent-function)
                              (setq lisp-body-indent 2)
                              (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
                              (eldoc-mode)))

(provide '042-scheme)
;;; 042-scheme.el ends here
