# xyttr 変更履歴

## 2013/03/05  v1.2.0

* Twitter API ver.1.1 に対応 (#6)
* ver.1.1 で`public_timeline`, `retweeted_by_me`, `retweeted_to_me`の各APIが削除されたため
  それぞれ対応するコマンドを削除
* DMバッファでsend-messageコマンドを実行すると送り先が正しく設定されない問題を修正
* 投稿バッファプラグイン (~/.xyttr/postbuf.l) を API ver.1.1 に対応

## 2012/01/03  v1.1.3

* xyttr-listコマンドのリスト選択肢に自作リストを追加
* DMの削除に対応 (通常のツイートと同じくdキー)
* ~/.xyttr/config.l のサンプルを追加 (postbuf.l)


## 2011/12/21  v1.1.2

* タイムラインのリロード時に実行するフック `*timeline-reload-hook*` を追加
* xyttr-listコマンドに引数としてリスト名を渡して起動できるように修正
* search-timelineで`in_reply_to_status_id`をツイートデータに含めるよう修正


## 2011/12/01  v1.1.1

* タイムラインリロード時のリクエスト処理を非同期に
* favoriteのトグル処理を非同期に
* expand-focused-url コマンド追加
* 購読中リストの取得タイミング変更
* リファレンス(site-lisp/xyttr/reference.md)追加
* ~/.xyttr/config.l のサンプルを追加 (site-lisp/xyttr/dot_xyttr_example以下)


## 2011/10/30  v1.0.0

* NetInstaller対応
* APIリクエスト時https接続を使用するように変更
* 非同期版APIリクエスト関数を追加
* DirectMessages関連API&コマンド追加


## 2011/07/13

* 日本語ハッシュタグ対応


## 2011/04/22

* list関連API&コマンド追加


## 2010/10/30

* お気に入り関連API&コマンド追加


## 2010/10/07

* githubで公開
