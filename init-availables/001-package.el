;;; 001-package.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2015  ned rihine

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
(require-if-exists package)

;; MELPA を追加します。
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)

;; MELPA-stable を追加します。
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Marmalade を追加します。
(add-to-list 'package-archives  '("marmalade"   . "http://marmalade-repo.org/packages/") t)

;; Org を追加します。
(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/") t)

;; 初期化します。
(package-initialize)

(provide '001-package)
;;; 001-package.el ends here
