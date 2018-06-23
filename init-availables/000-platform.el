;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; XEmacs
;;
(defun xemacsp ()
  (equal (featurep 'xemacs) 'xemacs))

(defvar xemacsp (xemacsp))
    
(defvar xemacs-no-mule-p (and (not (featurep 'mule))
                       xemacsp))
;;; Emacs
;;
(defun emacs-major-version-p (major-version)
  (equal emacs-major-version major-version))
    
(setq emacs20-p (and (emacs-major-version-p 20)
                     (null xemacsp))
      emacs21-p (and (emacs-major-version-p 21)
                     (null xemacsp))
      emacs22-p (and (emacs-major-version-p 22)
                     (null xemacsp))
      emacs23-p (and (emacs-major-version-p 23)
                     (null xemacsp))
      emacs24-p (and (emacs-major-version-p 24)
                     (null xemacsp)))

;;; Meadow
;;
(defun meadowp ()
  (featurep 'meadow))
    
(defvar meadowp (meadowp))
    
(defvar meadow1-p (and meadowp emacs20-p))
(defvar meadow2-p (and meadowp emacs21-p))
(defvar meadow3-p (and meadowp emacs22-p))

;;; OS 判別
;;
(defun system-type-p (system-symbol)
      (eq system-type system-symbol))
    
(defvar linuxp       (system-type-p 'gnu/linux))
(defvar darwinp      (system-type-p 'darwin))
(defvar usg-unix-v-p (system-type-p 'usg-unix-v))
(defvar windows-nt-p (system-type-p 'windows-nt))
(defvar ms-dos-p     (system-type-p 'ms-dos))

(provide '000-platform)
