.emacs.d の概要
========================================================

## ディレクトリ構成

~/.emacs.d を src_topdir とすると、こんな感じになっています。

    $(src_topdir)/
                  README.md
                  init-files/
                  init.el
                  module-available/
                  snippets/
                  private/

### 主なポイント

- `README.md` はこのファイルです。
- `init-files/` は Emacs Lisp のパッケージをロードするファイル用のディレクトリです。
- `init.el` は `./init-files/000-init.el` を require するだけのファイルです。
- `module-available/` は Emacs Lisp のパッケージを置いておくディレクトリです。
- `snippets/` は yasnippet のすにぺっとを保存しておきます。
- `private/` は `profile.el` などの基本的に Git で管理しないファイルを置いておくディレクトリです。
