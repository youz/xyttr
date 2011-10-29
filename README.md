# xyttr

xyzzy上で動作するミニマムなtwitterクライアント


## Install

- NetInstallerをよりインストール
  下記のURLのパッケージリストを登録し、パッケージ`*scrap*`よりインストールして下さい。
  http://youz.github.com/xyzzy/package.l

- 手動インストール
  xyttr.l を`*load-path*`に配置してください。

※依存ライブラリ[xml-http-request](http://miyamuko.s56.xrea.com/xyzzy/xml-http-request/intro.htm), [json](http://miyamuko.s56.xrea.com/xyzzy/json/intro.htm), [xl-oauth](http://github.com/youz/xl-oauth)を別途インストールしておく必要があります。

### .xyzzy設定例

    (require 'xyttr)
    (setq xyttr:*default-user* "your-name"
          xyttr:*auto-reload* 600)


## Usage

    M-x xyttr

初回起動時にブラウザ経由でOAuth認証を行います。
取得したaccess tokenは~/.xyttr フォルダ下に"token_<ユーザー名>"というファイル名で保存されます。


## Keymap

+ タイムライン表示
    - M -- @関連
    - M-M -- DirectMessages
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
    - ` (shift + @) -- 返信
    - rt -- 公式RT
    - rr -- 引用して返信
    - ru -- 引用してツイート (非公式RT)
    - dm -- DM
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
* 非同期リクエストに切り替え
* list購読
* マルチアカウント

## Author
Yousuke Ushiki (<citrus.yubeshi@gmail.com>)

[@Yubeshi](http://twitter.com/Yubeshi/) / [@xyttr](http://twitter.com/xyttr/)

## Copyright
MIT License を適用しています。

