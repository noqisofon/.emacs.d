;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; lexical-binding: t; -*-

;;; 040-lisp-common.el ---

;; Copyright (C) 2014-2018  ned rihine

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

(setq slime-net-coding-system 'utf8-unix)
(setq inferior-lisp-program "sbcl")

;; Lisp の括弧内のインデントです。
(setq lisp-body-indent 2)

(add-hook 'lisp-mode-hook (lambda ()
                            (set (make-local-variable 'lisp-indent-function)
                                 'common-lisp-indent-function)))

(require-if-exists 042-clojure)
(require-if-exists 042-scheme)

(provide '040-lisp-common)
;;; 040-lisp-common.el ends here
