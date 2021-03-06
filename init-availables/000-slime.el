;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; 000-slime.el ---

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
;;

(when windows-nt-p
  (setq sbcl-version (getenv "SBCL_VERSION")))
;; ;; 
;; (setq sbcl-version 
;;       (let ((output (shell-command-to-string "sbcl --version | awk '{ print $2; }'")))
;;         ;; 何故か改行が入ってしまうため、substring している。
;;         (substring output 0 6)))
;; ;; )

(setq sbcl-program (if windows-nt-p
                       (format "C:/Program Files/Steel Bank Common Lisp/%s/sbcl.exe" sbcl-version)
                     "/usr/bin/sbcl"))

(setq inferior-lisp-program sbcl-program)
;; 
(setq slime-contribs '(slime-repl slime-fancy slime-banner))

(require-if-exists slime
                   (add-hook 'slime-mode-hook 'set-up-slime-ac))

(require-if-exists ac-slime
                   (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))


(provide '000-slime)
;;; 000-slime.el ends here
