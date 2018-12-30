;;; 040-type-script.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018  ned rihine

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
(require-if-exists typescript)
(require-if-exists tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(push '("\\.ts$" . tide-mode) auto-mode-alist)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (defun typescript-setup ()
;;   (setq typescript-indent-level 4)
;;   (flycheck-mode t)
;;   ;; (flycheck-typescript-tslint-setup)
;;   (tss-setup-current-buffer))

;; (when (fboundp 'tss-config-default)
;;   (tss-config-default))

;; (add-hook 'typescript-mode-hook 'typescript-setup)
;; (add-hook 'kill-buffer-hook     'tss--delete-process t)

(provide '040-type-script)
;;; 040-type-script.el ends here
