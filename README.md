# xyttr

xyzzy上で動作するミニマムなtwitterクライアント

## Features

### タイムライン表示
* home timeline
* public timeline
* user timeline
* mentions
* retweeted by_me, of_me, to_me
* search -- `xyttr:*search-lang*` に指定した言語で検索
* search-global
* favorites
* list

### 投稿
* tweet 
* mention
* reply-to
* retweet -- 公式RT
* tweet-with-quote -- 非公式RT (or QT)
* reply-with-quote -- 引用して返信

### その他
* favorites
* destroy-status


## Install
(NetInstaller対応予定)

NetInstallerからxml-http-requestとxl-jsonをインストールし、
[xl-oauth](http://github.com/youz/xl-oauth)のoauth.l と xyttr.l を`*load-path*`に配置します。

.xyzzyには

    (require 'xyttr)
    (setq xyttr:*default-user* "your-name"
          xyttr:*auto-reload* 600)

と追記しておきます。


## Usage

    M-x xyttr

初回起動時にブラウザ経由でOAuth認証を行います。
取得したaccess tokenは~/.xyttr フォルダ下に"token_<ユーザー名>"というファイル名で保存されます。


## Keymap

+ タイムライン表示
    - M -- @関連
    - U -- ユーザータイムライン
    - L -- リスト
    - F -- お気に入り
    - / -- twitter検索 (`xyttr:*default-lang*` に指定した言語で検索)
    - s -- 同上
    - S -- twitter検索 (言語指定なしで検索)
    - R -- リロード (新着取得)
    - M-r -- オートリロード on / off
    - J -- ページ追加 (過去分取得)
    - Q -- 閉じる

+ ポスト
    - u -- tweet
    - @ -- 言及
    - ` -- 返信
    - rt -- 公式RT
    - rr -- 引用して返信
    - ru -- 引用してツイート (非公式RT)
    - f -- ☆ on / off
    - D -- 削除

+ カーソル
    - j -- 次
    - k -- 前
    - TAB -- 次のリンク
    - l -- 同上
    - h -- 前のリンク

+ その他
    - RET -- リンク(url, ユーザーTL, ハッシュタグ)を開く
    - C -- ステータスのURLをクリップボードにコピー
    - p -- 言及先のステータスをポップアップ表示


## Todo
* Netinstaller対応
* マルチアカウント
* list購読
* 非同期リクエスト

## Author
Yousuke Ushiki (<citrus.yubeshi@gmail.com>)

[@Yubeshi](http://twitter.com/Yubeshi/) / [@xyttr](http://twitter.com/xyttr/)

## Copyright
MIT License を適用しています。
