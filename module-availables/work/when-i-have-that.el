;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; when-i-have-that.el ---

;; Copyright (C) 2014  ned rihine

;; Author:  <ned.rihine@gmail.com>
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
(defmacro fbound-when (fn &rest body)
  `(when (fboundp ',fn)
     ,@body))


(provide 'when-i-have-that)
;;; when-i-have-that.el ends here
