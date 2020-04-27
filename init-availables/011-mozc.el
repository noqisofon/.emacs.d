;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; 011-mozc.el --- 

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
(require-if-exists mozc


                   (setq default-input-method "japanese-mozc")
  
                   ;;
                   ;; 変換候補の表示スタイルに候補のリストを入力中文字列の側に表示する 'overlay モードを選択します。
                   ;; 設定できるシンボルには以下の 2 つがあります:
                   ;;
                   ;; - 'overlay
                   ;; - 'echoarea
                   ;;
                   (setq mozc-candidate-style 'overlay)

                   (add-hook 'mozc-mode-hook
                             (lambda ()
                               (define-key mozc-mode-map (kbd "<zenkaku-hankaku>") 'toggle-input-method))))

(provide '011-mozc)
