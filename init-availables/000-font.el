;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; 000-font.el ---

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
(require 'cl)

(defun generate-font-string (font-name size)
  "フォント名とフォントサイズを取って Emacs 用のフォント名形式に変換します。"
  (format "%s-%d:weight=normal" font-name size))

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (font)
             (find-font (font-spec :name font)))
           fonts))

(set-frame-font (apply #'font-candidate
                       (mapcar (lambda (font-name)
                                 (generate-font-string font-name 10))
                               '("Ricty" "Migu 2M" "Migu 1M" "TakaoGothic" "IPAGothic" "MS Gothic"))))

(provide '000-font)
;;; 000-font.el ends here
