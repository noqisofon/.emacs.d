;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

(require 'e-arrows-macro)
(require-if-exists when-i-have-that)

(require-if-exists 000-platform)
(require-if-exists 000-locale)

(require-if-exists xyzzy-like-title)
(require-if-exists bookshelf)

;; カレントディレクトリの位置を $HOME に設定します。
(cd "~/")

;; 起動時のメッセージを表示しないようにします。
(setq inhibit-startup-message t)
;; *scratch* バッファの中身を空にします。
(setq initial-scratch-message nil)

;; エラー時に画面をフラッシュさせないようにします。
(setq ring-bell-function 'ignore)

(setq visible-bell nil)

;; ログの記録量を 100000 に増やします。
(setq message-log-max 100000)
;; 履歴の長さを 100000 に増やします。
(setq history-length 100000)

;; ツールバーを消したままにします。
(tool-bar-mode 0)

(defun after-byte-compile ()
  (if (file-newer-than-file-p (concat user-emacs-directory "init.el") (concat user-emacs-directory "init.elc"))
      (byte-compile-file (concat user-emacs-directory "init.el"))
    (byte-recompile-directory (concat
                               user-emacs-directory "module-availables") 0)
    (byte-recompile-directory (concat user-emacs-directory "init-availables") 0)
    (byte-recompile-directory (concat user-emacs-directory "private") 0)))

;; 終了時にバイトコンパイルを行います。
(add-hook 'kill-emacs-query-functions 'after-byte-compile)

(require-if-exists 000-display)
(require-if-exists 000-font)
(require-if-exists 000-text-editor)
(require-if-exists 000-scroll-bar)
(require-if-exists 000-backup)
(require-if-exists 000-mode-line)
(require-if-exists 000-emacs-client)
(require-if-exists 000-autoinsert)
(require-if-exists 000-elscreen)
(require-if-exists 000-auto-complete)
(require-if-exists 000-slime)

(provide '000-environment)
