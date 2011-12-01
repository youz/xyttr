# xyttr リファレンス

xyttrのコマンドとxyttrパッケージ内の関数/マクロのリファレンス

## 起動コマンド

以下のコマンドは全てuserパッケージ内で定義されています。`M-x コマンド名` で起動できます。

- xyttr

    ホームを表示します。
    他のタイムラインバッファからは`H`で呼び出せます。

- xyttr-mentions

    メンション/リプライを表示します。
    他のタイムラインバッファからは`M`で呼び出せます。

- xyttr-messages

    ダイレクトメッセージを表示します。
    他のタイムラインバッファからは`Alt+M`で呼び出せます。

- xyttr-list

    minibufferのプロンプトで指定したリスト(購読中の物に限る)を表示します。
    リストは`@username/listname`の形式で指定します。
    complete+が導入してあれば、@までタイプすれば購読中の全リストが
    候補として表示されます。
    他のタイムラインバッファからは`L`で呼び出せます。

- xyttr-favorites

    お気に入りを表示します。
    (v1.1.0現在次ページの表示が正しく取得できません)
    他のタイムラインバッファからは`F`で呼び出せます。

- xyttr-user

    指定ユーザーのツイートを表示します。
    名前を省略した場合、自分(`xyttr:*default-user*`)のツイートを表示します。
    他のタイムラインバッファからは`U`で呼び出せます。

- xyttr-search

    `xyttr:*search-lang*`に指定した言語でtwitter検索します。
    `xyttr:*search-lang*`のデフォルト値は"ja"です。
    他のタイムラインバッファからは`s`で呼び出せます。

- xyttr-search-global

    言語指定なしでtwitter検索します。
    他のタイムラインバッファからは`S`または`/`で呼び出せます。

- xyttr-retweeted-by-me

    リツイートしたツイートを表示します。

- xyttr-retweeted-to-me

    フォロイーがリツイートしたツイートを表示します。

- xyttr-retweeted-of-me

    誰かにリツイートされた自分のツイートを表示します。


## タイムラインバッファ用コマンド

以下のコマンドは全てxyttrパッケージ内で定義され、
`*xyttr-timeline-keymap*`に登録されています。

### 移動系

- forward-entry

    下のツイートへ移動 (`j`キー)

- backward-entry

    上のツイートへ移動 (`k`キー)

- next-link

    次のリンク(@ユーザー名, #ハッシュタグ, URL)へ移動 (`l`キー)

- previous-link

    前のリンク(@ユーザー名, #ハッシュタグ, URL)へ移動 (`h`キー)

- open-link

    カーソル下のリンクを開く (`Return`キー)


### ポスト系

- tweet

    ツイートを投稿します。(`u`キー)

- mention

    カーソル下のツイートの投稿主に対するメンションを投稿します。(`@`キー)

- reply-to

    カーソル下のツイートへの返信を投稿します。(`Shift+@`)

- reply-with-quote

    カーソル下のツイートに対し、引用付きの返信を投稿します。(`r-r`)

- retweet

    カーソル下のツイートをリツイート(公式RT)します。(`r-t`)

- tweet-with-quote

    カーソル下のツイートを引用して投稿(=非公式RT)します。(`r-u`)

- send-message

    カーソル下のツイートの投稿主へDMを送ります。(`d-m`)

- toggle-favorite

    お気に入りをトグルします。(`f`キー)

- destroy-status

    ツイートを削除します。(`D`キー)


### リロード

- timeline-reload

    新着を取得します。(`R`キー)

- toggle-auto-reload

    自動リロード(新着取得)のon/offを切り替えます。(`Alt+r`)

- timeline-append-page

    次ページを取得します。(`J`キー)


### その他

- close-timeline-buffer

    タイムラインバッファを閉じます。(`Q`キー)

- copy-status-url

    カーソル下のツイートのURLをクリップボードにコピーします。(`C`キー)

- expand-focused-url

    カーソル下の短縮URLを展開したURLに置き換えます。(`e`キー)
    多重短縮URLはHTTPステータス30xが返される限り繰り返し展開しますが、
    前置数引数を指定すると最大展開回数を前置引数の値に制限します。
    例: `M-1 e` とすると1回だけ展開します。

- show-irt-status

    カーソル下のツイートの返信先のツイートをポップアップします。(`p`キー)



## Twitter REST API関数
Twitter REST APIを利用するための関数です。全てxyttrパッケージよりexportされています。

### タイムライン

- api-public-timeline (&key trim_user include_entities)
- api-public-timeline-async (&key trim_user include_entities onsuccess onfailure oncomplete handler)
- api-home-timeline (&key since_id max_id count page trim_user include_entities)
- api-home-timeline-async (&key since_id max_id count page trim_user include_entities onsuccess onfailure oncomplete handler)
- api-mentions (&key since_id max_id count page trim_user include_rts include_entities)
- api-mentions-async (&key since_id max_id count page trim_user include_rts include_entities onsuccess onfailure oncomplete handler)
- api-user-timeline (&key user_id screen_name since_id max_id count page trim_user include_rts include_entities)
- api-user-timeline-async (&key user_id screen_name since_id max_id count page trim_user include_rts include_entities onsuccess onfailure oncomplete handler)
- api-search (&key q lang rpp page max_id since_id since geocode show_ser result_type)
- api-search-async (&key q lang rpp page max_id since_id since geocode show_ser result_type onsuccess onfailure oncomplete handler)

- api-show-status (&key id)
- api-show-status-async (&key id onsuccess onfailure oncomplete handler)

### ポスト

- api-update (&key status in_reply_to_status_id lat long place_id display_coordinates)
- api-update-async (&key status in_reply_to_status_id lat long place_id display_coordinates onsuccess onfailure oncomplete handler)
- api-destroy (&key id)
- api-destroy-async (&key id onsuccess onfailure oncomplete handler)

### ReTweet

- api-retweet (&key id)
- api-retweet-async (&key id onsuccess onfailure oncomplete handler)
- api-retweeted-by-me (&key since_id max_id count page trim_user include_entities)
- api-retweeted-by-me-async (&key since_id max_id count page trim_user include_entities onsuccess onfailure oncomplete handler)
- api-retweeted-to-me (&key since_id max_id count page trim_user include_entities)
- api-retweeted-to-me-async (&key since_id max_id count page trim_user include_entities onsuccess onfailure oncomplete handler)
- api-retweeted_by (&key id count page)
- api-retweeted_by-async (&key id count page onsuccess onfailure oncomplete handler)
- api-retweeted_by/ids (&key id count page)
- api-retweeted_by/ids-async (&key id count page onsuccess onfailure oncomplete handler)
- api-retweets (&key id count)
- api-retweets-async (&key id count onsuccess onfailure oncomplete handler)
- api-retweets-of-me (&key since_id max_id count page trim_user include_entities)
- api-retweets-of-me-async (&key since_id max_id count page trim_user include_entities onsuccess onfailure oncomplete handler)

### Direct Message

- api-direct-messages (&key since_id max_id count page)
- api-direct-messages-async (&key since_id max_id count page onsuccess onfailure oncomplete handler)
- api-direct-messages-destroy (&key id)
- api-direct-messages-destroy-async (&key id onsuccess onfailure oncomplete handler)
- api-direct-messages-new (&key user text)
- api-direct-messages-new-async (&key user text onsuccess onfailure oncomplete handler)
- api-direct-messages-sent (&key since_id max_id count page)
- api-direct-messages-sent-async (&key since_id max_id count page onsuccess onfailure oncomplete handler)

### Favorites

- api-favorites (&key id page)
- api-favorites-async (&key id page onsuccess onfailure oncomplete handler)
- api-favorites-create (&key id)
- api-favorites-create-async (&key id onsuccess onfailure oncomplete handler)
- api-favorites-destroy (&key id)
- api-favorites-destroy-async (&key id onsuccess onfailure oncomplete handler)


### List

- api-list-create (&key user name mode description)
- api-list-create-async (&key user name mode description onsuccess onfailure oncomplete handler)
- api-list-delete (&key user list_id)
- api-list-delete-async (&key user list_id onsuccess onfailure oncomplete handler)
- api-list-index (&key user cursor)
- api-list-index-async (&key user cursor onsuccess onfailure oncomplete handler)
- api-list-info (&key user list_id)
- api-list-info-async (&key user list_id onsuccess onfailure oncomplete handler)
- api-list-memberships (&key user list_id)
- api-list-memberships-async (&key user list_id onsuccess onfailure oncomplete handler)
- api-list-statuses (&key user list_id since_id max_id per_page page)
- api-list-statuses-async (&key user list_id since_id max_id per_page page onsuccess onfailure oncomplete handler)
- api-list-subscriptions (&key user cursor)
- api-list-subscriptions-async (&key user cursor onsuccess onfailure oncomplete handler)
- api-list-update (&key user list_id name mode description)
- api-list-update-async (&key user list_id name mode description onsuccess onfailure oncomplete handler)


各APIの機能/引数の意味は https://dev.twitter.com/docs/api や
http://watcher.moe-nifty.com/memo/docs/twitterAPI50.txt 等を参照してください。
REST APIのパス名/パラメータ名がほぼそのまま関数名/引数名になっています。

関数名の最後に`-async`とある物は非同期リクエストを行う関数で、それ以外は
同期リクエストを行います。

同期版関数はリクエストが成功するとレスポンス(JSON)をjson:json-decodeに渡し、
デコードされたリストを返します。
リクエストが失敗した場合はcondition `xyttr::request-error`を投げます。

非同期版関数は実行するとキャンセルオジェクト(xhr::xhr-cancel-ticket)を返し、
すぐに制御が戻ります。キャンセルオブジェクトをxhr:xhr-abort関数に渡すと
リクエストを中止します。

リクエストが成功すると、レスポンスをjson:json-decodeしてonsuccessに
指定したコールバック関数に渡します。onsuccess省略時はmessage関数で
httpステータスコードをステータスバーに表示します。

リクエストが失敗した場合、onfailureに指定したコールバック関数に
レスポンステキスト,HTTPステータスコード(数値),レスポンスヘッダー(連想リスト)を
渡します。onfailure省略時はcondigtion `xyttr::request-error`を投げます。
アラートは表示されませんが、*Trace Output*バッファで確認が可能です。



## タイムラインデータ

タイムラインの各種パラメータ(使用API, APIパラメータ, リロード間隔等)は
xyttr::timeline構造体で定義され、ローカル変数`xyttr::buffer-timeline`に
保存されます。

### xyttr::timeline構造体のスロット

-  user        -- 未使用 (マルチアカウント対応の為にuser.screen_nameを格納する予定)
-  tokens      -- 未使用 (マルチアカウント対応の為にoauth access tokenを格納する予定)
-  mode        -- タイムラインモード名 (:home-timeline, :user-timeline, etc)
-  apifunc     -- 使用API (#'api-home-timeline-async, #'api-user-timeline-async, etc)
-  params      -- リロード/次ページ取得時にapifuncへ渡す基本パラメータ
-  auto-reload -- 自動リロードの間隔 (秒数 or nil)
-  unread      -- 未読件数
-  request     -- リロード/次ページ取得時のリクエスト中にキャンセルチケットが
-  alldata     -- 全tweetデータのリスト
-  last-id     -- 最新tweetのstatus id (リロード時に使用)
-  first-id    -- 最古tweetのstatus id (次ページ取得時に使用)
-  page        -- 未使用


## ユーティリティーマクロ/関数
xyttrをカスタマイズ/拡張する際に便利な関数/マクロです。
全てxyttrパッケージで定義されていて、exportはされていません。

- *macro* w/uniq (gsyms &body body)

    Arcの同名マクロを移植した物で、OnLisp等で紹介されている
    [with-gensyms](http://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/classicMacros.html)と同等のマクロです。

- *macro* whenlet (var expr &body body)

    Arcの同名マクロを移植した物です。
    `(whenlet var expr body...)` は

        (let ((var expr))
          (when var
            body...)))

    と同じ意味になります。

- *macro* whilet (var test &body body)

    Arcの同名マクロを移植した物です。
    `(whilet var expr body...)` は

        (let (var)
          (while (setq var expr)
            body...)))

    と同じ意味になります。


- *macro* json-value (obj key)

    JSONリスト(JSON文字列をjson:json-decodeで変換して得られる連想リスト)の
    フィールド値を取得します。
    keyは `foo.bar.baz` のようにJavaScript風の指定が可能です。
    配列には対応していません。
    
        (json-value (api-show-status :id tweet-id) user.screen_name)


- *macro* w/json ((&rest keys) obj &body body)

    JSONリストobjのフィールド値をkeys中の同名シンボルに束縛し、bodyを評価します。
    
        (w/json (user.name text id) (api-show-status :id tweet-id)
          (popup-string (format nil "@~A: ~A" user.screen_name text) (point)))


- *function* load-plugin (plugin &optional force)

    ~/.xyttr フォルダ中にあるlispファイルをロードします。
    読み込み済みのファイルは無視しますが、forceオプションに
    t を指定すると再ロードします。
    
        (load-plugin "show-gist")  ; ~/.xyttr/show-gist.l をload


- *function* expand-short-url (url &optional d)

    短縮URLを展開します。
    多重短縮URLはHTTPステータス30xが返される限り繰り返し展開しますが、
    第2引数dに数値を指定すると展開回数を最大d回に制限します。


- *function* entry-point (&optional (p (point)))

    タイムラインバッファー上で、指定した位置(省略時は現在のカーソル位置)に
    該当するツイートの開始位置とツイート情報(JSONリスト)を多値で返します。


- *macro* w/entry ((&rest keys) &body body)

    タイムラインバッファー上で、現在のカーソル位置に該当するツイートのデータを
    w/jsonの形式で受け取り、bodyを評価します。

    - カーソル下のツイートをクリップボードにコピーする例

        (defun copy-tweet ()
          (interactive)
          (w/entry (user.screen_name text)
            (let ((s (format nil "@~A: ~A" user.screen_name text)))
              (copy-to-clipboard s)
              (message "Copied: ~S" s))))


- *function* focused-url

    カーソル下にあるURLを返します。
    カーソル下の文字列がURLでない場合、nilを返します。


- *function* focused-hashtag

    カーソル下にあるハッシュタグ文字列を返します。(`#`は含みません)
    カーソル下の文字列がハッシュタグでない場合、nilを返します。

- *function* focused-user

    カーソル下にあるユーザー名を返します。(`@`は含みません)
    カーソル下の文字列がユーザー名でない場合、nilを返します。

- *function* status-url

    タイムラインバッファー上で、現在のカーソル位置に該当するツイートのURLを返します。


- *macro* w/buffer-modifying ((&optional buf) &body body)

    read-onlyなバッファを一時的に書き込み可能にしてbodyを評価します。


- *macro* define-api (name params &key auth method apiurl path key)

    Twitter REST API関数を定義します。
    同期版 api-{name} と 非同期版 api-{name}-async の2つの関数が生成され、
    xyttrパッケージよりexportされます。
    
    * name -- 関数名
    * params -- リクエストパラメータ
    * auth -- OAuth認証ヘッダの必要の有無 (省略時は t)
    * method -- HTTPメソッド (省略時は get)
    * apiurl -- リクエスト先ホスト名 (省略時は "api.twitter.com")
    * path -- リソースURLのパス部分 (省略不可)
    * key -- 関数を指定すると、リクエスト結果(jsonリスト)をその関数に通してから返します。

----
    ;; xyttr.l より抜粋
    (define-api update
      (status in_reply_to_status_id lat long place_id display_coordinates)
      :method post
      :path "/1/statuses/update.json")
    
    (define-api destroy (id)
      :method post
      :path (format nil "/1/statuses/destroy/~D.json" id))

    (define-api search
      (q lang rpp page max_id since_id since until
       geocode show_user result_type)
      :apiurl *search-url*
      :path "/search.json"
      :key #'search-result-to-statuses)

