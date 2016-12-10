;;; 040-fsharp.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  ned rihine

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
;;
(setq inferior-fsharp-program (if windows-nt-p
                                  "c:/Program Files (x86)/Microsoft SDKs/F#/4.0/Framework/v4.0/fsi"
                                "/usr/bin/fsharpi"))
(setq fsharp-compiler (if windows-nt-p
                          "c:/Program Files (x86)/Microsoft SDKs/F#/4.0/Framework/v4.0/fsc"
                        "/usr/bin/fsharpc"))

;; fsharp-tab-always-indent
;; TAB キーを押すと:
;;         t: いつもインデントされます。
;;     not t: されない？
(setq-default fsharp-tab-always-indent t)

;; fsharp-indent-offset
;; インデントサイズは 2 です。
(setq-default fsharp-indent-offset 2)

(setq-default fsharp-continuation-offset 2)

(setq-default fsharp-smart-indentation t)

(lazyload (fsharp-mode) "fsharp-mode")
;; F# モード。
(push '("\\.fs[iylx]?$" . fsharp-mode) auto-mode-alist)

(provide '040-fsharp)
;;; 040-fsharp.el ends here
