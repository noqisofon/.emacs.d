;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; lexical-binding: t; -*-

;;; 040-haskell.el ---

;; Copyright (C) 2015-2016  ned rihine

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
(push '("\\.hs$"     . haskell-mode) auto-mode-alist)
(push '("\\.lhs$"    . literate-haskell-mode) auto-mode-alist)
(push '("\\.cabel$"  . haskell-cabal-mode) auto-mode-alist)

(push '("runghc"     . haskell-mode) interpreter-mode-alist)
(push '("runhaskell" . haskell-mode) interpreter-mode-alist)

(require-if-exists haskell-mode-autoloads)

(lazyload (haskell-mode) "haskell-mode")
(lazyload (haskell-cabal) "haskell-cabal")

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

(provide '040-haskell)
;;; 040-haskell.el ends here
