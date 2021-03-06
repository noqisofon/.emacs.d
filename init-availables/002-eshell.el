;;; 002-eshell.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  ned rihine

;; Author: ned rihine <rihine@leviatan>
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
(let ((emacsclient "emacsclient"))
  ;; eshell で yaourt -Syua する時に使用する。
  ;; vim だとうまくいかないため。
  (setenv "VISUAL" emacsclient)
  (setenv "EDITOR" emacsclient))

;; eshell のプロンプトを zsh っぽくする。
(setq eshell-prompt-function
      (lambda ()
        (concat (user-login-name) "@" (system-name)
                " "
                (eshell/pwd)
                (if (= (user-uid) 0) " # " " % "))))

(provide '002-eshell)
;;; 002-eshell.el ends here
