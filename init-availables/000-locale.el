;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:
(let ((language "Japanese"))
  (set-language-environment language))

;;; Char code:
(if windows-nt-p
    (let ((default-coding 'japanese-shift-jis-dos)
          (system-coding 'iso-2022-jp)
          (clipboard-coding 'utf-16le-dos))
      ;;
      (when (fboundp 'set-w32-system-coding-system)
        (set-w32-system-coding-system system-coding))
      ;; デフォルトの文字コードです。
      (when (fboundp 'set-default-coding-systems)
        (set-default-coding-systems default-coding))
      ;; デフォルトのファイルバッファの文字コードを設定します。
      (setq buffer-file-coding-system default-coding)
      ;; 端末の文字コードです。
      (set-terminal-coding-system default-coding)
      ;; Windows NT の内部文字コードは UTF-16LE なので、クリップボードのエンコードを utf-16le-dos にしておきます。
      (set-clipboard-coding-system clipboard-coding)
      ;; 通常キーボードを使用して打つ文字です。
      (set-keyboard-coding-system default-coding)
      ;; 新規作成するバッファのエンコードです。
      (prefer-coding-system default-coding))
  ;; else
  (let ((default-coding 'utf-8-unix))
    ;; デフォルトの文字コードです。
    (when (fboundp 'set-default-coding-systems)
      (set-default-coding-systems default-coding))
    ;; デフォルトのファイルバッファの文字コードを設定します。
    (setq buffer-file-coding-system default-coding)
    ;; 端末の文字コードです。
    (set-terminal-coding-system default-coding)
    ;; クリップボードのエンコードです。
    (when (fboundp 'set-clipboard-codng-system)
      (set-clipboard-codng-system default-coding))
    ;; 新規作成するバッファのエンコードです。
    (when (fboundp 'prefer-coding-system-coding)
      (prefer-coding-system-coding default-coding))))

(provide '000-locale)
;;; 000-locale.el ends here
