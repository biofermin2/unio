# unio
Keyword searcher for S-expression.

S式のためのキーワード検索ツール

<img alt="unio symbol" src="https://github.com/biofermin2/unio/blob/main/unio2-1e.png">

this library was developed for searching keywords in deep nested symbolic-expression like a onion.
so I named this package from onion in Latin language.

このライブラリは玉ねぎのようにネストが深すぎて、ほしいデータをピックアップ出来ないという
問題を解決するために作りました。unioという名前はラテン語の玉ねぎから取りました。

## setup
this guidance made for linux users.

※この説明は主にlinux user向けです。

this library can install by roswell from github repository as the follow line.

roswellを使って、githubのこのリポジトリからダイレクトに
~/.roswell/local-projects/
以下にダウンロードします。

```shell
$ ros install biofermin2/unio
```

then it can load by quicklisp like this.

quicklispを使って、local-projectsに入れたライブラリを呼び出します。

```common-lisp
(ql:quickload :unio) 
```
that's it.

以上で、unioライブラリが使えるようになります。

## usage
### seek
this function can search keywords from string list or symbol-expression.

文字列あるいはS式からキーワードを検索し、該当する箇所を抜き出します。

```common-lisp
(seek 'key1.key2.key3...key-n 'obj <skin> <rm-dup>)
;; <> means optional arguments.
;; key is string or symbol 
```
there is a skin option, you can put your favorite positive integer as far as possible.
the option is able to select out layer S-expression.
default number is 0.

またオプションとしてskinが設定出来ます。
それで検索文字列を含むS式よりも外側の括弧を指定出来ます。
デフォルト値は０となっています。

anyway,you can use this function like this.

使用例は以下の通りです。

```common-lisp
;; set sample list(string or s-exp)
(setq lst '((((((hoge (foo1) bar))))(((foo2 foo3)))(((hoge (foo4)) bar))))) ; => ((((((HOGE (FOO1) BAR)))) (((FOO2 FOO3))) (((HOGE (FOO4)) BAR))))

;; pickup the all word include "foo"
(seek 'foo lst)				; => ((foo1) (foo2 foo3) (foo4))NIL

;; you can specify several keywords in one time like this.
(seek 'foo1.foo4 lst)			; => ((foo1))((foo4))NIL

(seek 'foo lst 1)			; => ((hoge (foo1) bar) ((foo2 foo3)) (hoge (foo4)))NIL
(seek 'foo lst 2)			; => (((hoge (foo1) bar)) (((foo2 foo3))) ((hoge (foo4)) bar))NIL

```
If the skin option is specified, 
but there are no parentheses outside,
it will not be displayed.

skinオプションで指定しても外側に括弧が存在しない場合、
表示されなくなりますので、ご注意下さい。

and duplicated data have removed in normal.
but if you use rm-dup option, you can avoid it.
in that time,you have to set skin option too.
but maybe you don't use this option.

また、通常重複したデータは削除されていますが、
rm-depオプションを使えば重複データを削除しないようにも出来ます。
その際はskinオプションも必ず入力して下さい。
ただ、普通は使わないと思います。

```common-lisp
;; if you don't want to use remove-duplicate function, you should set nil as the option.
(seek 'foo lst 0 nil)			; => 
((foo1) (foo2 foo3) (foo2 foo3) (foo4)) NIL
(seek 'foo lst 1 nil)			; => 
((hoge (foo1) bar) ((foo2 foo3)) ((foo2 foo3)) (hoge (foo4))) NIL

```

### seek-files
this macro can search in several files like the grep command.

grepのように文字列の代わりにファイル名を指定します。
複数ファイルにあるS式のうち検索文字列を含む箇所を抜き出します。

```common-lisp
(seek-files "string keyword" "file1" "file2"..."file-n")
```
the actual usage is as follows.

実際の使い方は下記の通りです。

```common-lisp
(seek-files 'def
    "~/a.lisp"
    "~/b.lisp"
    "~/c.lisp")		; => 
((defun test4 (x) (print x)) (defun main ()) (defpackage :myapp
  (:use :cl)
  (:export :main))
 (defun test3 (x)
  (let ((a 1) (b 0))
    (lambda (x) (+ a b x))))
			       (defun main () (test3 7))) NIL
```
in paticular, I don't support regular expression,
but as you see,it will also pickup the words that contain keyword.

特に正規表現に対応はしてませんが、ご覧の通り、
検索キーワードを含むワードもピックアップしてくれます。

### sets
if you want to bind above evaluated data in a variable,
you can use this macro for it as same as setq, setf.

seekやseek-filesで評価したS式を変数に格納して利用したい場合、
setsマクロが使えます。使い方はsetqやsetfと同じような書き方で使えます。

```common-lisp
;;(sets var sexp)

(sets a (seek "foo" lst))						   ; => ((FOO1) (FOO2 FOO3) (FOO4))
(car a)									   ; => (FOO1)
(cdr a)									   ; => ((FOO2 FOO3) (FOO4))

```

have a good symbolic-expression life with unio.

それではunioでS式生活をお楽しみ下さい。


## update history
[2021-11-27] 0.2.6 setsマクロから隠しindexを削除。warningの原因になっていたため。

[2021-11-21] 0.2.5 １度に複数キーワードを検索出来るように仕様変更。またキーワードも文字列ではなくシンボルでも可に変更。

[2021-11-20] 0.2.4 listが文字列だけでなく、S式の場合でも処理出来るように変更。seek-filesでキーワードがシンボルに変換されていたのを修正

[2021-11-02] 0.2.3 sets macroの追加

[2021-10-30] 0.2.2 seekの出力形式を文字列のリストからS式のリストに変更

[2021-10-29] 0.2.1 seek関数に隠しオプションrm-dupを追加 seek-filesにあったバグの修正

[2021-10-28] 0.2.0 リファクタリングでloop数など減らし型チェックも付け、５つあった関数を２つに統合。

[2021-10-11] 0.1.0 orep完成。unioと名付ける。

[2021-10-07] 0.0.0 開発コードorepで開発開始

## todo
- seek-filesする際、ファイルが純粋なS式で構成されていない（例えばシェバングなど含んでいる）場合、エラーとなりNILだけが返されるので、それに対応する。
- seek-filesを使う際に、正規表現でパスネーム指定出来るようにする。（現状、１つ１つファイル名を指定して上げなくてはいけない）。
- ヘルプも表示出来るようにする。
- コマンドラインから実行出来るようにする。
- だいたい開発が落ち着いたら、型指定などの最適化をする。
