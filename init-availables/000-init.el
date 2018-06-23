;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; 000-init.el --- 

;; Copyright (C) 2014-2016  ned rihine

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

(load "001-input-method")
(load "001-key-bind")
(load "001-package")
(load "001-recent")
(load "001-session")
(load "001-editor-config")

(load "002-eshell")
(load "002-emmet")
(load "002-flycheck")
(load "002-eldoc")

(load "010-light-markup-language")
(load "020-version-control")

(load "040-programing-language")

(provide '000-init)
;;; 000-init.el ends here
