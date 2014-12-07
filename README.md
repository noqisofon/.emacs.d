.emacs.d の概要
========================================================

## ディレクトリ構成

~/.emacs.d を src_topdir とすると、こんな感じになっています。

    $(src_topdir)/
                  COPYING.GPLv3
                  README.md
                  init-availables/
                  init.el
                  module-availables/
                  snippets/
                  themes/
                  private/

### 主なポイント

- `COPYING.GPLv3` はライセンスファイルです。
- `README.md` はこのファイルです。
- `init-availables/` は Emacs Lisp のパッケージをロードするファイル用のディレクトリです。
- `init.el` は `./init-files/000-init.el` を require するだけのファイルです。
- `module-availables/` は Emacs Lisp のパッケージを置いておくディレクトリです。
- `snippets/` は yasnippet のスニペットを保存しておきます。
- `themes/` は Emacs のテーマを置いておきます。
- `private/` は `profile.el` などの基本的に Git で管理しないファイルを置いておくディレクトリです。
