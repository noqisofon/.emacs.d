;;; 000-auto-complete.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2015  ned rihine

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

;; ;; auto-complete を自動で始まらないようにします？
;; (setq ac-auto-start nil)

;; (require-if-exists auto-complete)
;; (require-if-exists auto-complete-config)

;; ;; (global-auto-complete-mode t)
;; ;; (ac-config-default)

;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict/auto-complete")
(require-if-exists company)

(global-company-mode)

(provide '000-auto-complete)
;;; 000-auto-complete.el ends here
