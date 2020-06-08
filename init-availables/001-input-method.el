;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; 001-input-method.el ---

;; Copyright (C) 2014-2020  ned rihine

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
(if (null (getenv "EMACS_IM"))
    (setq emacs-ime (cond
                     (windows-nt-p                           "ms-ime")
                     ((equal (getenv "GTK_IM_MODULE") "uim") "uim")
                     (:else                                  "none")))
    ;; else
    (cond
     ((eq "uim" (getenv "EMACS_IM"))
      ;; emacs の IME(Input Method Engine?) として、uim を使用する。
      (setq emacs-ime "uim"))
     ((eq "skk" (getenv "EMACS_IM"))
      (setq emacs-ime "skk"))
     ((eq "mozc" (getenv "EMACS_IM"))
      (setq emacs-ime "mozc"))
     (:else
      (setq emacs-ime "none"))))

(cond ((eq emacs-ime "uim")
       (require-if-exists 011-uim))

      ((eq emacs-ime "ms-ime")
       (require-if-exists 011-ime)))

(provide '001-input-method)
;;; 001-input-method.el ends here
