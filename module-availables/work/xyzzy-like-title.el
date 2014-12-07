;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;; 現在選択中のバッファがファイルからできているかどうか判別します。
(defun selected-buffer-from-file-p ()
  "現在選択中のバッファがファイルからできているかどうか判別します。"
  (let ((selected-buffer-filename (buffer-file-name))
        (selected-buffer-name (buffer-name)))
    (if selected-buffer-filename t nil)))

(when (not meadowp)
  (defun update-title-caption ()
    "タイトルキャプションを更新します。"
    (setq frame-title-format (let ((file-name (buffer-file-name))
                                   (name (buffer-name)))
                               (if file-name
                                   (concat "%f - " invocation-name " " emacs-version "@" system-name)
                                 (concat "%b - " invocation-name " " emacs-version "@" system-name)))))
  (update-title-caption))

;;; %f だと、フルパス名。 %b ならバッファの名前。

;; 現在は meadow 用に update-title-caption 関数を定義していますが、
;; 他の Emacen では定義していないので、ガード句でくるんでいます。
;; meadow 以外でも同じようにしたい場合は 同じ名前の関数を定義してください。
(when (fboundp 'update-title-caption)
  ;; switch-to-buffer の後に frame-title-format の値を更新します。
  (defadvice switch-to-buffer
      (after switch-to-buffer-after-update-the-title-captions first () activate)
    (message (buffer-name))
    (update-title-caption)))

;; ファイルを保存した時にもタイトルの更新を行うようにします。
(add-hook 'after-save-hook 'update-title-caption)

;; ファイルを開いた時にもタイトルの更新を行うようにします。
(add-hook 'find-file-hook 'update-title-caption)

(provide 'xyzzy-like-title)
;;; xyzzy-like-title.el ends here
