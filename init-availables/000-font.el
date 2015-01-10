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
;; フォント名を指定して使用するフォントを設定します。
(defun setting-font (font-name &optional size)
  (unless size
    (setq size 13))
  (let* ((fontset-name font-name)
         (ascii-font font-name)
         (multibyte-font font-name)
         (height (* size 10))
         (font (format "%s-%d:weight=normal:slant=normal" ascii-font size))
         (ascii-fontspec (font-spec :family ascii-font))
         (multibyte-fontspec (font-spec :family multibyte-font)))
    (let ((fontset (create-fontset-from-ascii-font font nil fontset-name)))
      (set-face-attribute 'default nil :family ascii-font)
      (set-fontset-font fontset 'japanese-jisx0213.2004-1 multibyte-fontspec)
      (set-fontset-font fontset 'japanese-jisx0213-2 multibyte-fontspec)
      ;; 半角カナ
      (set-fontset-font fontset 'katakana-jisx0201 multibyte-fontspec)
      ;; 分音符付きラテン
      (set-fontset-font fontset '(#x0080 . #x024F) ascii-fontspec)
      ;; ギリシャ文字
      (set-fontset-font fontset '(#x0370 . #x03FF) ascii-fontspec))))

(if (not darwinp)
    (let ((font-name)
          (found-flag nil)
          (after-font-name-list '("Ricty" "Migu 1M" "TakaoGothic" "IPAGothic" "MS Gothic")))
      (while after-font-name-list
        ;; x:xs を設定します。
        (setq font-name (car after-font-name-list)
              after-font-name-list (cdr after-font-name-list))
        (when (not found-flag)
          ;; font-name が存在するかどうか調べます。
          (if (find-font (font-spec :family font-name))
              (progn
                (setting-font font-name)
                ;; 存在したら、found-flags に t を設定して find-font をスキップするようにします。
                (setq found-flag t))))))
  ;; else
  (set-face-font 'default "fontset-ricty"))

(provide '000-font)
;;; 000-font.el ends here
