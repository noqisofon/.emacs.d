;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Macros:
;; @see http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html
(defmacro add-hook-lambda (name &rest body)
  "lambda を書かずに、/'.*/ の次の引数に関数をズラズラ書けます。"
  `(add-hook ,name #'(lambda () ,@body)))

(defmacro global-set-key-lambda (key args &rest body)
  "lambda を書かずに関数をそのまま書くことができる global-set-key です。"
  `(global-set-key ,key (lambda ,args ,@body)))

(defmacro append-to-list (to list)
  "append して setq します。"
  `(setq ,to (append ,list ,to)))

(defmacro require-if-exists (library &rest body)
  "ライブラリがあったら require します。"
  `(when (locate-library ,(symbol-name library))
     (require ',library)
     ,@body))

(defmacro eval-safe (&rest body)
  "安全な評価です。評価に失敗してもそこで止まったりしません。"
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

(defmacro lazyload (func library-name &rest body)
  "遅延ロードします。"
  `(when (locate-library ,library-name)
     ,@(mapcar (lambda (f) `(autoload ',f ,library-name nil t)) func)
     (eval-after-load ,library-name
       '(progn
          ,@body))))

(defun load-safe (load-lib)
  "安全な load です。読み込みに失敗してもそこで止まったりしません。"
  (let ((load-status (load load-lib t)))
    (or load-status
        (message (format "[load-safe] failed %s" load-lib)))
    load-status))

;; via: http://www.sodan.org/~knagano/emacs/dotemacs.html
;; locate-library してから autoload します。
(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))

(provide 'e-arrows-macro)
;;; e-arrows-macro.el ends here
