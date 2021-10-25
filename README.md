# unio
Keyword searcher for S-expression.

<img alt="unio symbol" src="https://github.com/biofermin2/unio/blob/main/unio2-1e.png">

## usage
※この説明は主にlinux user向けです。

roswellを使って、githubのこのリポジトリからダイレクトに
~/.roswell/local-projects/
以下にダウンロードする。
```shell
$ ros install biofermin2/unio
```

quicklispを使って、local-projectsに入れたライブラリを呼び出す。
```common-lisp
(ql:quickload :unio) 
```

### seek
文字列となっているS式からキーワードを検索し、
該当する箇所を抜き出す。

```common-lisp
(unio:seek "string keyword" "string list")
```
またオプションとしてskinが設定出来ます。
それで検索文字列を含むS式よりも外側の括弧を指定出来ます。
デフォルト値は０となっています。
但し、外側に括弧がそれ以上指定出来ない場合はエラーとなります。

### seek-file
文字列の代わりにファイル名を指定します。
複数ファイルにあるS式のうち検索文字列を含む箇所を抜き出します。

```common-lisp
(unio:seek-files "string keyword" "file1" "file2"..."file-n")
```
