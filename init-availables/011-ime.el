;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; 011-ime.el ---

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
(when windows-nt-p
  ;; デフォルトインプットメソッドを W32-IME にします。
  (setq default-input-method "W32-IME")

  ;; IME の初期化
  (when (fboundp 'w32-ime-initialize)
    (w32-ime-initialize)))

(provide '011-ime)
;;; 011-ime.el ends here
