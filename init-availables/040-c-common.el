;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; lexical-binding: t; -*-

;;; 040-c-common.el ---

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
(add-hook-lambda 'c-mode-common-hook
 ;; cc-mode スタイルにします。
 (c-set-style "cc-mode")
 ;; インデントは空白文字で行ないます。
 (setq indent-tabs-mode nil)
 ;; `;' を押すと自動で改行されないようにします。
 (setq c-auto-newline nil)
 ;; タブキーでインデントを行います。
 (setq c-tab-always-indent t)
 ;; タブ幅を 4 にします。
 (setq tab-width 4)
 ;; ブロック内の文の字下げを指定します。
 (setq c-indent-level tab-width)
 ;; if 文の then 節や while 文の本体の様に、文の中で始まる文に加える字下げの数を指定します。
 (setq c-continued-statement-offset tab-width)
 ;; 開き中カッコで始まる行に加える字下げの数を指定します。
 (setq c-brace-offset tab-width)
 ;; 関数の引数宣言の字下げを指定します。
 (setq c-argdecl-indent tab-width)
 ;; ラベルや case、default のある文に加える字下げの数を指定します。
 (setq c-label-offset 0)

 ;; コメントだけの行は 0 にしておきます。
 ;;;; こうすると、コメントだけ一番下に下がってしまいます。
 (setq c-comment-only-line-offset 0)

 ;; 引数リストの閉じ括弧もインデントします。
 (c-set-offset 'arglist-close 0)
 ;; public などのアクセス修飾子は -3 インデントします。
 (c-set-offset 'access-label -3)
 ;; switch 構文のラベルは c-basic-offset だけインデントします。
 (c-set-offset 'case-label '+)
 ;; 継続行は c-basic-offset 分のインデントを行います。
 (c-set-offset 'statement-cont '+)
 ;; クラスのメンバー初期化リストの 1 行目は c-basic-offset 分インデントします。
 (c-set-offset 'member-init-intro '+))


(provide '040-c-common)
;;; 040-c-common.el ends here
