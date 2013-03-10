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

    minibufferのプロンプトで指定したリストを表示します。
    リストは自作の物と購読中の物から`@username/listname`の形式で指定します。
    (手入力は煩わしいので[http://white.s151.xrea.com/wiki/index.php?script%2Fcomplete%2B](complete+)の導入を推奨します)
    他のタイムラインバッファからは`L`で呼び出せます。

- xyttr-favorites

    お気に入りを表示します。
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

- xyttr-retweets-of-me

    他のユーザーにリツイートされた自分のツイートを表示します。


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

- api-home-timeline (&key count since_id max_id trim_user exclude_replies contributor_details include_rts)
- api-home-timeline-async (&key count since_id max_id trim_user exclude_replies contributor_details include_rts onsuccess onfailure oncomplete handler)
- api-mentions (&key count since_id max_id trim_user contributor_details include_entities)
- api-mentions-async (&key count since_id max_id trim_user contributor_details include_entities onsuccess onfailure oncomplete handler)
- api-user-timeline (&key user_id screen_name count since_id max_id trim_user exclude_replies contributor_details include_rts)
- api-user-timeline-async (&key user_id screen_name count since_id max_id trim_user exclude_replies contributor_details include_rts onsuccess onfailure oncomplete handler)
- api-search (&key q lang count page max_id since_id until geocode result_type include_entities)
- api-search-async (&key q lang count page max_id since_id until geocode result_type include_entities onsuccess onfailure oncomplete handler)

- api-show-status (&key id)
- api-show-status-async (&key id onsuccess onfailure oncomplete handler)

### ポスト

- api-update (&key status in_reply_to_status_id lat long place_id display_coordinates trim_user)
- api-update-async (&key status in_reply_to_status_id lat long place_id display_coordinates trim_user onsuccess onfailure oncomplete handler)
- api-destroy (&key id trim_user)
- api-destroy-async (&key id trim_user onsuccess onfailure oncomplete handler)

### ReTweet

- api-retweet (&key id)
- api-retweet-async (&key id onsuccess onfailure oncomplete handler)
- api-retweets (&key id count)
- api-retweets-async (&key id count onsuccess onfailure oncomplete handler)
- api-retweets-of-me (&key since_id max_id count page trim_user include_entities)
- api-retweets-of-me-async (&key since_id max_id count page trim_user include_entities onsuccess onfailure oncomplete handler)

### Direct Message

- api-direct-messages (&key since_id max_id count include_entities skip_status)
- api-direct-messages-async (&key since_id max_id count include_entities skip_status onsuccess onfailure oncomplete handler)
- api-direct-messages-destroy (&key id include_entities)
- api-direct-messages-destroy-async (&key id include_entities onsuccess onfailure oncomplete handler)
- api-direct-messages-new (&key user_id screen_name text)
- api-direct-messages-new-async (&key user_id screen_name text onsuccess onfailure oncomplete handler)
- api-direct-messages-sent (&key since_id max_id count page include_entities)
- api-direct-messages-sent-async (&key since_id max_id count page include_entities onsuccess onfailure oncomplete handler)
- api-direct-messages-show (&key id)
- api-direct-messages-show-async (&key id onsuccess onfailure oncomplete handler)

### Favorites

- api-favorites (&key user_id screen_name count since_id max_id include_entities)
- api-favorites-async (&key user_id screen_name count since_id max_id include_entities onsuccess onfailure oncomplete handler)
- api-favorites-create (&key id include_entities)
- api-favorites-create-async (&key id include_entities onsuccess onfailure oncomplete handler)
- api-favorites-destroy (&key id include_entities)
- api-favorites-destroy-async (&key id include_entities onsuccess onfailure oncomplete handler)


### List

- api-list-create (&key name mode description)
- api-list-create-async (&key name mode description onsuccess onfailure oncomplete handler)
- api-list-delete (&key list_id slug owner_screen_name owner_id)
- api-list-delete-async (&key list_id slug owner_screen_name owner_id onsuccess onfailure oncomplete handler)
- api-list-index (&key user_id screen_name)
- api-list-index-async (&key user_id screen_name onsuccess onfailure oncomplete handler)
- api-list-info (&key list_id slug owner_screen_name owner_id)
- api-list-info-async (&key list_id slug owner_screen_name owner_id onsuccess onfailure oncomplete handler)
- api-list-memberships (&key user_id screen_name cursor filter_to_owned_lists)
- api-list-memberships-async (&key user_id screen_name cursor filter_to_owned_lists onsuccess onfailure oncomplete handler)
- api-list-statuses (&key list_id slug owner_screen_name owner_id since_id max_id count include_entities include_rts)
- api-list-statuses-async (&key list_id slug owner_screen_name owner_id since_id max_id count include_entities include_rts onsuccess onfailure oncomplete handler)
- api-list-subscriptions (&key user_id screen_name count cursor)
- api-list-subscriptions-async (&key user_id screen_name count cursor onsuccess onfailure oncomplete handler)
- api-list-update (&key list_id slug name mode description owner_screen_name owner_id)
- api-list-update-async (&key list_id slug name mode description owner_screen_name owner_id onsuccess onfailure oncomplete handler)


## Help

- api-rate-limit-status (&key resuources)
- api-rate-limit-status-async (&key resources onsuccess onfailure oncomplete handler)
- api-help-configuration ()
- api-help-configuration-async (&key onsuccess onfailure oncomplete handler)
- api-help-languages ()
- api-help-languages-async (&key onsuccess onfailure oncomplete handler)


各APIの機能/引数の意味は https://dev.twitter.com/docs/api を参照してください。
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
-  request     -- リロード/次ページ取得時のリクエスト中にキャンセルオブジェクトが格納されます
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

        (defun copy-tweet ()
          "カーソル下のツイートをクリップボードにコピーする"
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


- *macro* define-api (name params &key auth method path key)

    Twitter REST API関数を定義します。
    同期版 api-{name} と 非同期版 api-{name}-async の2つの関数が生成され、
    xyttrパッケージよりexportされます。
    
    * name -- 関数名
    * params -- リクエストパラメータ
    * auth -- OAuth認証ヘッダの必要の有無 (省略時は t)
    * method -- HTTPメソッド (省略時は get)
    * path -- リソースURLのパス部分 (省略不可)
    * key -- 関数を指定すると、リクエスト結果(jsonリスト)をその関数に通してから返します。

----
    ;; 使用例 (xyttr.lより抜粋)
    (define-api update
      (status in_reply_to_status_id lat long place_id display_coordinates trim_user)
      :method post
      :path "/statuses/update.json")
    
    (define-api destroy (id trim_user)
      :method post
      :path (format nil "/statuses/destroy/~D.json" id))


- *macro* define-tl-command (name params &key interactive buffer-name api-func api-params auto-reload hook)

    タイムラインコマンドを定義します。
    
    * name -- コマンド関数名
    * params -- コマンドパラメータ
    * interactive -- パラメータの受け取り方を指定するinteractive-string (paramsがnilの場合は不要)
    * buffer-name -- タイムラインバッファ名
    * api-func -- ツイートデータを取得するためのtimeline系api関数
    * api-params -- api-funcに渡すパラメータ
    * auto-reload -- 自動リロード間隔(秒) 省略時は`xyttr:*auto-reload*`の値
    * hook -- タイムライン用バッファ生成後に実行するhook関数(シンボル)

    api-funcは非同期版の関数を指定します。
    api関数を自作する場合はキーワード引数onsuccessとonfailureで
    コールバックを受け取るように定義してください。

----
    ;; 使用例 (xyttr.lより抜粋)
    (define-tl-command xyttr ()
      :buffer-name "*tw: home*"
      :api-func #'api-home-timeline-async
      :api-params (:count 50 :include_entities "true"))
    
    (define-tl-command xyttr-user (user)
      :interactive "sUser: @"
      :buffer-name (format nil "*tw: ~:[@~A~;mine~]*" (string= "" user) user)
      :api-func #'api-user-timeline-async
      :api-params (:screen_name user :count 50 :include_rts t
                   :include_entities "true"))
    
    (define-tl-command xyttr-search (q)
      :interactive "sSearch Twitter: "
      :buffer-name (format nil "*tw? ~A*" q)
      :api-func #'api-search-async
      :api-params (:q q :count 50 :lang *search-lang*
                   :include_entities "true"))
