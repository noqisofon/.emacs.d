;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; 011-input-method.el ---

;; Copyright (C) 2020  ned rihine

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
(require-if-exists uim)
(require-if-exists uim-leim)

;; TODO: 何らかの方法で選択している入力エンジン？(mozc とか anthy とか)を知ることができたらそれを元に切り替える。
(let ((default-im-name (string-trim-right (shell-command-to-string "uim-sh -e default-im-name"))))

  (cond ((equal default-im-name "mozc")
         (require-if-exists 011-mozc))
        (:else
         ;; 現在はとりあえず Anthy 固定。
         (load-library "anthy")

         (setq default-input-method "japanese-anthy-uim"))))


(provide '011-uim)
