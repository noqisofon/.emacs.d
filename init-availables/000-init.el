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

(require-if-exists 001-input-method)
(require-if-exists 001-key-bind)
(require-if-exists 001-package)
(require-if-exists 001-recent)
(require-if-exists 001-session)
(require-if-exists 001-editor-config)

(require-if-exists 002-eshell)
(require-if-exists 002-emmet)
(require-if-exists 002-flycheck)
(require-if-exists 002-eldoc)

(require-if-exists 010-light-markup-language)
(require-if-exists 020-version-control)

(require-if-exists 040-programing-language)

(provide '000-init)
;;; 000-init.el ends here
