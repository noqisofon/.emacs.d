;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; 000-elscreen.el ---

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
(when window-system
  (require-if-exists elscreen)
  (require-if-exists elscreen-server)

  ;; タブを表示します(非表示にする場合は nil を設定する)。
  (setq elscreen-display-tab t)

  ;; 自動でスクリーンを作成します。
  (defmacro elscreen-create-automatically (ad-do-it)
    `(if (not (elscreen-one-screen-p))
         ,ad-do-it
       (elscreen-create)
       (elscreen-notify-screen-modification 'force-immediately)
       (elscreen-message "New screen is automatically created")))

  (defadvice elscreen-next (around elscreen-create-automatically activate)
    (elscreen-create-automatically ad-do-it))

  (defadvice elscreen-previous (around elscreen-create-automatically activate)
    (elscreen-create-automatically ad-do-it))

  (defadvice elscreen-toggle (around elscreen-create-automatically activate)
    (elscreen-create-automatically ad-do-it))

  (defun elscreen-current-directory ()
    (let* (current-dir
           (active-file-name
            (with-current-buffer
                (let* ((current-screen (car (elscreen-get-conf-list 'screen-history)))
                       (property (cadr (assoc current-screen
                                              (elscreen-get-conf-list 'screen-property)))))
                  (marker-buffer (nth 2 property)))
              (progn
                (setq current-dir (expand-file-name (cadr (split-string (pwd)))))
                (buffer-file-name)))))
      (if active-file-name
          (file-name-directory active-file-name)
        current-dir)))


  (defun non-elscreen-current-directory ()
    (let* (current-dir
           (current-buffer
            (nth 1 (assoc 'buffer-list
                          (nth 1 (nth 1 (current-frame-configuration))))))
           (active-file-name
            (with-current-buffer current-buffer
              (progn
                (setq current-dir (expand-file-name (cadr (split-string (pwd)))))
                (buffer-file-name)))))
      (if active-file-name
          (file-name-directory active-file-name)
        current-dir))))


(provide '000-elscreen)
;;; 000-elscreen.el ends here
