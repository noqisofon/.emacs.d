;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-


;; ** スクリプトを保存する時，自動的に chmod +x を行なうようにする
;;
;; 次のようなコードを ~/.emacs に加えておくと、
;; + ファイルの先頭に #! で始まる行が含まれている
;; + ファイル名の先頭がピリオド以外
;; の場合，実行用のスクリプトファイルであると見なして、保存時に実行許可属性を自動的に設定します。
(defun make-file-executable ()
  "Make the file of this buffer executable, when it is a script source."
  (save-restriction
    (widen)
    (if (string= "#!" (buffer-substring-no-properties 1 (min 3 (point-max))))
        (let ((name (buffer-file-name)))
          (or (equal ?. (string-to-char (file-name-nondirectory name)))
              (let ((mode (file-modes name)))
                (set-file-modes name (logior mode (logand (/ mode 4) 73)))
                (message (concat "Wrote " name " (+x)"))))))))
(add-hook 'after-save-hook 'make-file-executable)


;; 日付を挿入します。
(defun insert-iso8601format-datetime ()
  "Insert the date the current cursor location."
  (interactive)
  (insert (let ((time-zone (format-time-string "%Z"))
                (iso8601-text (format-time-string "%Y-%m-%dT%H:%M:%S%Z")))
            (if (equal time-zone "")
                (concat iso8601-text (car (cdr (current-time-zone))))
              iso8601-text))))


;; 現在選択中のバッファのファイル名、あるいはバッファ名を返します。
(defun get-selected-buffer-name ()
  "現在選択中のバッファのファイル名、あるいはバッファ名を返します。"
  (interactive)
  (let ((selected-buffer-filename (buffer-file-name ))
        (selected-buffer-name (buffer-name)))
    (if (not selected-buffer-filename)
        selected-buffer-name
      ;; else
      selected-buffer-filename)))

;; テンポラリバッファを作成し、それをウィンドウに表示します。
(defun create-temporary-buffer ()
  "テンポラリバッファを作成し、それをウィンドウに表示します。"
  (interactive)
  ;; *temp* なバッファを作成し、それをウィンドウに表示します。
  (switch-to-buffer (generate-new-buffer "*temp*"))
  ;; セーブが必要ないことを指定します？
  (setq buffer-offer-save nil))

;; *scratch* バッファは簡単な処理に便利なバッファです。
;; しかし，そのバッファの内容を 別名で保存したり、kill したりすると，*scratch* バッファが消えてしまい，また作らないといけません。
;; 面倒です。 
;; そこで，以下のような設定が便利です。これを入れておくと、*scratch*バッファを C-x C-s で保存時には *scratch* バッファを作成してくれます。
;; また，C-x k で kill すると，*scratch*バッファの内容をすべて消してくれます。 非常に便利です。
(defun bookshelf-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(defun bookshelf-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (bookshelf-make-scratch 0) nil)
                        t))))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (function (lambda ()
                      (unless (member "*scratch*" (bookshelf-buffer-name-list))
                        (bookshelf-make-scratch 1)))))

(provide 'bookshelf)
;;; bookshelf.el ends here
