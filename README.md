# unio
Keyword searcher for S-expression.

S式のためのキーワード検索ツール

<img alt="unio symbol" height="200" width="400" src="https://github.com/biofermin2/unio/blob/main/unio3.svg">

this library was developed for searching keywords in deep nested symbolic-expression like a onion.
so I named this package from onion in Latin language.

このライブラリは玉ねぎのようにネストが深すぎて、ほしいデータをピックアップ出来ないという
問題を解決するために作りました。unioという名前はラテン語の玉ねぎから取りました。

Tools like grep will show you the lines that contain the keyword you searched for.

However, when using lisp, there are many times when you want to know not only the line containing the search keyword,

but also the S-expression.

unio picks up all the S-expressions that contain the search keyword.

In this way, unio will pick up all S-expressions that contain the search keyword. 

You can also expand the range of S-expressions you want to pick up by specifying layers.

grepといったツールを使うと検索したキーワードを含む行を表示してくれます。

しかし、lispを使っていると、検索キーワードを含む行だけではなく、S式を知りたいと思う事が多々あったりします。

unioではその様に検索キーワードを含むS式を全てピックアップしてくれます。

またレイヤー指定でピックアップしたいS式の範囲を広げる事も出来ます。

<a href="https://youtu.be/pPUw737Ln-A" alt="この画像をクリックすれば動画を拡大して見る事が出来ます。"><img alt="execute example" src="https://github.com/biofermin2/unio/blob/main/unio-3.gif"></a>

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

that's it.

セットアップは以上です。

## usage
unioの使い方には２通りあります。

１．コマンドラインツールとして
他のunixコマンド同様にshellのコマンドラインから対象ファイルを指定してS式検索出来ます。
$ unio "<検索キーワード>" <検索対象ファイル>

２．ライブラリとして
ソースコード内でquicklispから読み込んでunioの機能を使います。
本来こちらの機能をメインに開発していたのでキーワード等で
より高度なS式検索を実行出来ます。

では以下にそれぞれの詳細な設定や使い方の説明をします。

### １．as ros script
コマンドラインツールとしての使い方

if you wanna use unio as like grep command,

grepのようにscriptとして使いたい場合は

```shell
$ cd ~/.roswell/local-projects/biofermin2/unio/roswell/
$ ros build unio.ros
$ mv unio ~/.roswell/bin/

;; example of use
;; $ unio <search keyword> <object>
$ unio defun ~/test/*.lisp
❯ unio defun *.lisp
;;; a.lisp
((defun test-a (x)
  (lambda (x) (* x x))))
;;; b.lisp
((defun test-b (x)
  (print x)))
;;; c.lisp
((defun test-c (x)
  (let ((a 1)
        (b 0))
    (lambda (x) (+ a b x))))
 (defun main ()
(test-c 7)))

 
```
### 2.as library

ライブラリとしての使い方

The following instructions are for use within the source code.

以下の説明はソースコード内で利用したい場合です。
```
then it can load by quicklisp like this.

ソースコード内でunioを使いたい時は、
quicklispを使ってライブラリを呼び出します。

```common-lisp
(ql:quickload :unio)

;; パッケージでunioを指定しておくといちいちunio:のprefixを書かなくて良くなる
(defpackage :hoge-sample
  (:use :cl :unio ...))
(in-package :hoge-sample)

```

#### seek
this function can search keywords from string list or symbol-expression.

文字列あるいはS式からキーワードを検索し、該当する箇所を抜き出します。

```common-lisp
(seek "key" "obj")  

```
This is enough for basic use in source code.
The following are more advanced usage,
but I recommend learning them after you've become comfortable with the basic.

基本ソースコード上で使うのであるならこれだけ覚えておけば十分です。
以下はより高度な使い方ですが、それは使い慣れてからをオススメ致します。

there is a skin keyword, you can put your favorite positive integer as far as possible.
the keyword is able to select out layer S-expression.
default number is 0.

またキーワードとしてskinが設定出来ます。
それで検索文字列を含むS式よりも外側の括弧を指定出来ます。
デフォルト値は０となっています。

anyway,you can use this function like this.

使用例は以下の通りです。

```common-lisp
;; set sample list(string or s-exp)
(setq lst "((((((hoge (foo1) bar))))(((foo2 foo3)))(((hoge (foo4)) bar))))") ; => "((((((hoge (foo1) bar))))(((foo2 foo3)))(((hoge (foo4)) bar))))"

;; unio pickup the all words include "foo"
(seek "foo" lst)							     ; => ("(foo1)" "(foo2 foo3)" "(foo4)")
NIL

(seek "foo" lst :skin 1)			; => ("(hoge (foo1) bar)" "((foo2 foo3))" "(hoge (foo4))")
NIL
(seek "foo" lst :skin 2)			; => ("((hoge (foo1) bar))" "(((foo2 foo3)))" "((hoge (foo4)) bar)")
NIL

```
If the skin keyword is specified, 
but there are no parentheses outside,
it will not be displayed.

skinキーワードで指定しても外側に括弧が存在しない場合、
表示されなくなりますので、ご注意下さい。

and duplicated data have removed in normal.
but if you use rm-dup option, you can avoid it.
but maybe you don't use this option.

また、通常重複したデータは削除されていますが、
depキーワードを使えば重複箇所もそのまま表示出来ます。
ただ、普通は使わないと思います。

```common-lisp
;; if you don't want to use remove-duplicate function, you should set nil.
(seek "foo" lst :dup t)			; => ("(foo1)" "(foo2 foo3)" "(foo2 foo3)" "(foo4)")
NIL

```
Finally, I also added a new str keyword.
The default value is t, but if you change it to nil,
string chage to a list too.This allows the returned 
expression to be used as-is in the next expression.

最後に新しくstrキーワードも追加しました。
このキーワードのデフォルト値はtですが、nilに変更すると、
文字列からリストに変更されます。これにより返された式を
そのまま次の式で使う事なども出来ます。

```common-lisp
(seek "foo" lst :str nil)		; => ((foo1) (foo2 foo3) (foo4))
NIL
```


have a good symbolic-expression life with unio.

それではunioでS式生活をお楽しみ下さい。


## update history
[2022-09-03] 0.3.3 seek-filesの廃止、その代わりファイル対応用の関数string-catを追加。ros scriptもそれに合わせて修正。

[2022-06-26] 0.3.0 完全検索のキーワードオプション追加、seekに３回使われているloopマクロをmap関数で書き直し。評価結果をダイレクトに利用出来るように変更。
                    コマンドライン実行出来るようにros scriptとバイナリファイルも追加

[2022-05-25] 0.2.8 seek functionのlambda-listをoptionalからkeywordに変更。strキーワードの追加。

[2021-12-06] 0.2.7 seekの複数キーワード対応廃止。評価形式変更修正。seek-filesで正規表現対応。

[2021-11-27] 0.2.6 setsマクロから隠しindexを削除。warningの原因になっていたため。

[2021-11-21] 0.2.5 １度に複数キーワードを検索出来るように仕様変更。またキーワードも文字列ではなくシンボルでも可に変更。

[2021-11-20] 0.2.4 listが文字列だけでなく、S式の場合でも処理出来るように変更。seek-filesでキーワードがシンボルに変換されていたのを修正

[2021-11-02] 0.2.3 sets macroの追加

[2021-10-30] 0.2.2 seekの出力形式を文字列のリストからS式のリストに変更

[2021-10-29] 0.2.1 seek関数に隠しオプションrm-dupを追加 seek-filesにあったバグの修正

[2021-10-28] 0.2.0 リファクタリングでloop数など減らし型チェックも付け、５つあった関数を２つに統合。

[2021-10-11] 0.1.0 orep完成。unioと名付ける。

[2021-10-07] 0.0.0 開発コードorepで開発開始
