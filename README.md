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
(Netinstaller対応予定)
NetInstallerからxml-http-requestとxl-jsonをインストールし、
[xl-oauth](http://github.com/youz/xl-oauth)のoauth.l と xyttr.l を*load-path*に配置します。

.xyzzyには

    (require 'xyttr)
    (setq xyttr:*default-user* "your-name"
          xyttr:*auto-reload* 600)

と追記しておきます。


## Usage

    M-x xyttr

初回起動時にブラウザ経由でOAuth認証を行います。
取得したaccess tokenは~/.xyttr フォルダ下に"token_<ユーザー名>"というファイル名で保存されます。


## Key Bindgings

* C -- copy-status-url
* D -- destroy-status
* f -- toggle-favorite
* h -- previous-link
* J -- timeline-append-page
* j -- forward-entry
* k -- backward-entry
* l -- next-link
* M -- xyttr-mentions
* p -- show-parent-tweet
* Q -- close-buffer
* R -- timeline-reload
* M-r -- toggle-auto-reload
* rt -- retweet
* ru -- tweet-with-quote
* rr -- reply-with-quote
* u -- tweet
* @ -- mention
* ` -- reply-to
* / -- xyttr-search
* s -- xyttr-search
* S -- xyttr-search-global
* RET -- open-link
* TAB -- next-link
* M-Down -- next-link
* M-Up -- previous-link


## Todo
* マルチアカウント
* list
* reload-async

## Authoer
Yousuke Ushiki (<citrus.yubeshi@gmail.com>)

## Copyright
MIT License を適用しています。

