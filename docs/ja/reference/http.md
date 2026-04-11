[English](../../reference/http.md)

# HTTP リファレンス

## 型

| 型 | 説明 |
|------|-------------|
| `HttpRequest` | 受信 HTTP リクエストの不透明ハンドル |
| `HttpResponse` | 送信 HTTP レスポンスの不透明ハンドル |
| `HttpClientResponse` | HTTP クライアントレスポンスの不透明ハンドル |

`HttpRequest` はサーバーフレームワークから提供されます。`HttpResponse` は `response()` で作成します。`HttpClientResponse` はクライアント関数（`http_get`、`http_post`、`http_request`）から返されます。

## 関数（`http` パッケージ）

これらの関数は明示的なインポートが必要です:

```python
from http import listen, method, path, header, body, body_bytes, query, query_all, cookie, cookies, form_field, form_file, form_fields, response
```

### サーバー

| 関数 | シグネチャ | 説明 |
|----------|-----------|-------------|
| `listen` | `(host: str, port: int, handler: function(HttpRequest) -> HttpResponse) -> Unit` | 指定アドレスで HTTP サーバーを起動します。accept ループでブロックし、リクエストごとに `handler` を呼び出します。 |
| `listen` | `(host: str, port: int, handler: function(HttpRequest) -> HttpResponse, max_requests: int) -> Unit` | `max_requests` 件のリクエストを処理した後に停止する HTTP サーバーを起動します。`async function` + `block_on()` によるライフサイクル管理が可能になります。 |
| `listen` | `(host: str, port: int, handler: function(HttpRequest) -> HttpResponse, max_requests: int, port_callback: function(int) -> Unit) -> Unit` | 上記と同じですが、`bind` + `listen` 成功後に実際にバインドされたポートで `port_callback` を呼び出します。OS が割り当てるエフェメラルポートを使用するにはポート `0` を指定します。 |

### リクエストアクセサ

| 関数 | シグネチャ | 説明 |
|----------|-----------|-------------|
| `method` | `(req: HttpRequest) -> str` | HTTP メソッドを返します（例: `"GET"`、`"POST"`）。 |
| `path` | `(req: HttpRequest) -> str` | クエリ文字列を除いたリクエストパスを返します（例: `"/search?q=hello"` の場合 `"/search"`）。 |
| `header` | `(req: HttpRequest, key: str) -> Option<str>` | リクエストヘッダーの値を返します（大文字小文字を区別しない検索）。見つからない場合は `None` を返します。 |
| `body` | `(req: HttpRequest) -> str` | リクエストボディを文字列として返します。最初の NUL バイトで切り詰められます。バイナリデータには `body_bytes` を使用してください。 |
| `body_bytes` | `(req: HttpRequest) -> List<u8>` | リクエストボディをバイトリストとして返します。バイナリセーフで、NUL を含むすべてのバイトを保持します。 |
| `query` | `(req: HttpRequest, key: str) -> Option<str>` | クエリパラメータの値を返します。見つからない場合は `None` を返します。値は自動的に URL デコードされます。 |
| `query_all` | `(req: HttpRequest) -> Map<str, str>` | すべてのクエリパラメータをマップとして返します。キーと値は自動的に URL デコードされます。 |
| `cookie` | `(req: HttpRequest, name: str) -> Option<str>` | 名前で指定した Cookie の値を返します。見つからない場合は `None` を返します。 |
| `cookies` | `(req: HttpRequest) -> Map<str, str>` | すべての Cookie をマップとして返します。 |
| `form_field` | `(req: HttpRequest, name: str) -> Option<str>` | マルチパートフォームのテキストフィールドの値を返します。見つからない場合は `None` を返します。 |
| `form_file` | `(req: HttpRequest, name: str) -> Option<Map<str, str>>` | ファイルアップロード情報を `Option` として返します。`Some(map)` には `"filename"`、`"content_type"`、`"data"` のキーが含まれます。見つからない場合は `None` を返します。 |
| `form_fields` | `(req: HttpRequest) -> Map<str, str>` | すべてのマルチパートフォームのテキストフィールドをマップとして返します。 |

### レスポンスビルダー

| 関数 | シグネチャ | 説明 |
|----------|-----------|-------------|
| `response` | `(status: int, headers: Map<str, str>, body: str) -> HttpResponse` | 指定したステータスコード、ヘッダー、ボディで HTTP レスポンスを作成します。 |

## 使用例

### 基本的な HTTP サーバー

```python
from http import listen, method, path, header, body, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> HttpResponse:
    m = method(req)
    p = path(req)
    if p == "/hello":
        return response(200, {"Content-Type": "text/plain"}, "Hello, World!")
    if p == "/echo":
        b = body(req)
        return response(200, {"Content-Type": "text/plain"}, b)
    return response(404, {"Content-Type": "text/plain"}, "Not Found")
)
```

### `async function` によるノンブロッキングサーバー

```python
from http import listen, path, response

async function start_server(port: int) -> str:
    listen("127.0.0.1", port, (req: HttpRequest) -> HttpResponse:
        p = path(req)
        if p == "/api/health":
            return response(200, {"Content-Type": "application/json"}, "{\"status\": \"ok\"}")
        return response(404, {"Content-Type": "text/plain"}, "Not Found")
    )
    return "done"

t = start_server(8080)
# サーバーはバックグラウンドタスクとして実行される
```

### リクエスト制限付きサーバー（`max_requests`）

```python
from http import listen, path, response, http_get, status, body

port_holder = [0]
function on_port(p: int) -> Unit:
    port_holder[0] = p

async function start_server() -> str:
    listen("127.0.0.1", 0, (req: HttpRequest) -> HttpResponse:
        return response(200, {"Content-Type": "text/plain"}, "Hello!")
    , 1, on_port)  # 1 リクエスト後に停止; on_port にバインドされたポートを通知
    return "done"

t = start_server()
sleep(100)  # サーバーの起動を待機
port = port_holder[0]

case http_get("http://127.0.0.1:" + to_str(port) + "/"):
    Ok(resp):
        print(body(resp))  # "Hello!"
    Err(e):
        print("error")

result = block_on(t)  # サーバーは 1 リクエスト後に終了; block_on が完了する
```

### クエリパラメータの読み取り

```python
from http import listen, path, query, query_all, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> HttpResponse:
    p = path(req)
    if p == "/search":
        case query(req, "q"):
            Some(q):
                return response(200, {"Content-Type": "text/plain"}, "Search: " + q)
            None:
                return response(400, {"Content-Type": "text/plain"}, "Missing query parameter: q")
    return response(404, {"Content-Type": "text/plain"}, "Not Found")
)
```

### ヘッダーの読み取り

```python
from http import listen, header, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> HttpResponse:
    case header(req, "Authorization"):
        Some(token):
            return response(200, {"Content-Type": "text/plain"}, "Authenticated: " + token)
        None:
            return response(401, {"Content-Type": "text/plain"}, "Unauthorized")
)
```

### フォーム送信の処理

```python
from http import listen, form_field, form_file, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> HttpResponse:
    case form_field(req, "username"):
        Some(name):
            case form_file(req, "avatar"):
                Some(file_info):
                    filename = file_info["filename"]
                    return response(200, {"Content-Type": "text/plain"}, "Hello " + name + ", file: " + filename)
                None:
                    return response(400, {"Content-Type": "text/plain"}, "No file uploaded")
        None:
            return response(400, {"Content-Type": "text/plain"}, "Missing username")
)
```

### Cookie の読み取り

```python
from http import listen, cookie, cookies, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> HttpResponse:
    case cookie(req, "session_id"):
        Some(sid):
            return response(200, {"Content-Type": "text/plain"}, "Session: " + sid)
        None:
            return response(401, {"Content-Type": "text/plain"}, "No session")
)
```

## 動作仕様

- `listen()` はアドレスにバインドし、リスニングを開始して accept ループに入ります。
- 引数 3 つで呼び出した場合、accept ループは無期限に実行されます。
- 引数 4 つで呼び出した場合（`max_requests`）、指定した数のリクエストを処理した後にサーバーが停止します。`max_requests` は正の整数でなければなりません。これにより `async function` + `block_on()` によるライフサイクル管理が可能になります。不正なリクエスト（サイレントにスキップされる）は制限のカウントに含まれません。
- 引数 5 つで呼び出した場合（`max_requests`、`port_callback`）、`bind` + `listen` 成功後に `port_callback` が実際にバインドされたポートで同期的に呼び出されます。並列テストでのポート競合を避けるため、ポート `0`（OS が割り当てるエフェメラルポート）と組み合わせて使用できます。
- サーバーはデフォルトで HTTP/1.1 キープアライブをサポートします。単一の接続で複数のリクエストを処理できます。サーバーは各リクエストの `Connection` ヘッダーを確認します: `Connection: close` が送信された場合、レスポンス後に接続が閉じられます。それ以外の場合、接続は後続のリクエストのために維持されます。アイドル接続は 5 秒のタイムアウト後に閉じられます。
- `Content-Length` がヘッダーマップに指定されていない場合、レスポンスに自動的に追加されます。
- サーバーは `Content-Length` ベースのボディ読み取りと `Transfer-Encoding: chunked` デコードを含む HTTP/1.1 をサポートします。
- リクエストに `Transfer-Encoding: chunked` が存在する場合、ボディは自動的にデコード・結合されます。Ry コードは完全なボディを透過的に受け取ります。
- `Transfer-Encoding: chunked` と `Content-Length` の両方が存在する場合、リクエストは不正として拒否されます（RFC 9112 準拠）。
- チャンクレスポンスを送信するには、`response()` に渡すヘッダーマップに `"Transfer-Encoding": "chunked"` を含めます。ボディは自動的にチャンク形式でエンコードされます。
- `header()` によるヘッダー検索は大文字小文字を区別しません。
- `path()` はクエリ文字列を除いたパスを返します。クエリパラメータは `query()` または `query_all()` で別途アクセスします。
- クエリパラメータの値は自動的に URL デコードされます（`%20` → スペース、`+` → スペース）。
- 重複するクエリパラメータキーの場合、最初の値が返されます。
- `cookie()` と `cookies()` は `Cookie` ヘッダーを `;` で分割し、各ペアを最初の `=` で分割してパースします。名前と値の先頭と末尾の空白はトリムされます。
- 重複する Cookie 名の場合、最初の値が返されます。
- Cookie の値には `=` 文字を含めることができます（最初の `=` のみが名前と値の区切りとなります）。
- `form_field()`、`form_file()`、`form_fields()` は `multipart/form-data` リクエストボディをパースします。パースは遅延実行され、最初の呼び出し時にパースされてキャッシュされます。
- `boundary` パラメータは `Content-Type` ヘッダーから抽出され、クォート付きとクォートなしの両方の値をサポートします。
- `Content-Disposition` に `filename` を持つパートはファイルアップロードとして扱われ、持たないパートはテキストフィールドとして扱われます。
- 重複するフィールド/ファイル名の場合、最初の値が返されます。
- `form_file()` は `"filename"`、`"content_type"`、`"data"` のキーを持つ `Some(map)` を返すか、フィールドが見つからない場合は `None` を返します。パートに `Content-Type` が指定されていない場合、デフォルトで `"application/octet-stream"` になります。
- マルチパートでないリクエストの場合、フォーム関数は `None`（`form_field`、`form_file`）または空のマップ（`form_fields`）を返します。

## サポートされるステータスコード

以下のステータスコードには標準の理由フレーズがあります（RFC 9110）:

| コード | 理由 |
|------|--------|
| 100 | Continue |
| 101 | Switching Protocols |
| 200 | OK |
| 201 | Created |
| 202 | Accepted |
| 203 | Non-Authoritative Information |
| 204 | No Content |
| 205 | Reset Content |
| 206 | Partial Content |
| 300 | Multiple Choices |
| 301 | Moved Permanently |
| 302 | Found |
| 303 | See Other |
| 304 | Not Modified |
| 307 | Temporary Redirect |
| 308 | Permanent Redirect |
| 400 | Bad Request |
| 401 | Unauthorized |
| 403 | Forbidden |
| 404 | Not Found |
| 405 | Method Not Allowed |
| 406 | Not Acceptable |
| 408 | Request Timeout |
| 409 | Conflict |
| 410 | Gone |
| 411 | Length Required |
| 413 | Content Too Large |
| 414 | URI Too Long |
| 415 | Unsupported Media Type |
| 416 | Range Not Satisfiable |
| 417 | Expectation Failed |
| 422 | Unprocessable Content |
| 426 | Upgrade Required |
| 429 | Too Many Requests |
| 500 | Internal Server Error |
| 501 | Not Implemented |
| 502 | Bad Gateway |
| 503 | Service Unavailable |
| 504 | Gateway Timeout |
| 505 | HTTP Version Not Supported |

その他のステータスコードは理由フレーズとして `"Unknown"` を使用します。

## HTTP クライアント

### クライアント関数

| 関数 | シグネチャ | 説明 |
|----------|-----------|-------------|
| `http_get` | `(url: str) -> Result<HttpClientResponse, Error>` | 指定した URL に HTTP GET リクエストを送信します。 |
| `http_post` | `(url: str, body: str, headers: Map<str, str>) -> Result<HttpClientResponse, Error>` | ボディとヘッダーを指定して HTTP POST リクエストを送信します。 |
| `http_request` | `(method: str, url: str, headers: Map<str, str>, body: str) -> Result<HttpClientResponse, Error>` | カスタムメソッドで HTTP リクエストを送信します。 |

### レスポンスアクセサ

| 関数 | シグネチャ | 説明 |
|----------|-----------|-------------|
| `status` | `(resp: HttpClientResponse) -> int` | HTTP ステータスコードを返します。 |
| `body` | `(resp: HttpClientResponse) -> str` | レスポンスボディを文字列として返します。最初の NUL バイトで切り詰められます。バイナリデータには `body_bytes` を使用してください。 |
| `body_bytes` | `(resp: HttpClientResponse) -> List<u8>` | レスポンスボディをバイトリストとして返します。バイナリセーフで、NUL を含むすべてのバイトを保持します。 |
| `header` | `(resp: HttpClientResponse, key: str) -> Option<str>` | レスポンスヘッダーの値を返します（大文字小文字を区別しない検索）。見つからない場合は `None` を返します。 |
| `http_client_response_free` | `(resp: HttpClientResponse) -> Unit` | レスポンスと関連メモリを解放します。レスポンスの使用が終わったら呼び出してください。 |

### クライアントの使用例

```python
from http import http_get, http_post, status, body, header

# シンプルな GET リクエスト
case http_get("http://example.com/api/data"):
    Ok(resp):
        s = status(resp)
        b = body(resp)
        print(to_str(s) + ": " + b)
    Err(e):
        print("Request failed")

# ボディとヘッダー付きの POST リクエスト
headers: Map<str, str> = {"Content-Type": "application/json"}
case http_post("http://example.com/api/data", "{\"key\": \"value\"}", headers):
    Ok(resp):
        print(body(resp))
    Err(e):
        print("Request failed")
```

### クライアントの動作仕様

- `http://` と `https://` の両方の URL をサポートします。HTTPS はシステム CA バンドル証明書検証による TLS を使用します。
- `Host` ヘッダーは URL に基づいて自動的に追加されます。
- `Connection: close` が常に送信されます。各リクエストは個別の TCP 接続を使用します。
- `Content-Length` は常に自動的に追加されます（空のボディには `0` を含む）。ユーザーが指定した `Content-Length` ヘッダーは正しい値で上書きされます。
- レスポンスボディの読み取りは `Content-Length`、`Transfer-Encoding: chunked`、または接続クローズまでの読み取りをサポートします。
- レスポンスヘッダーの検索は大文字小文字を区別しません。
- 接続タイムアウトは 5 秒、受信タイムアウトは 30 秒です。
- 接続失敗、無効な URL、不正なレスポンスの場合は `Err` を返します。
- `HttpClientResponse` は割り当てられたメモリ（ヘッダー、ボディ）を所有しています。メモリリークを避けるため、使用後に `http_client_response_free()` を呼び出してください。

### リダイレクトの動作

HTTP クライアント関数はリダイレクトレスポンス（`Location` ヘッダー付きの 3xx）を自動的にフォローします。

- **サポートされるステータスコード**: 301、302、303、307、308
- **最大リダイレクト数**: 10（超過した場合は `Err` を返す）
- **メソッド変換**（RFC 9110 準拠）:
  - 301、302: `POST` は `GET` に変更される（ボディは破棄）。その他のメソッドは維持
  - 303: メソッドは常に `GET` に変更される（ボディは破棄）
  - 307、308: メソッドとボディは維持
- **URL 解決**: `Location` ヘッダーの絶対 URL、プロトコル相対（`//...`）、絶対パス（`/...`）、相対パスをサポート
- ユーザー指定のヘッダーは各リダイレクトホップで再送信されますが、機密ヘッダー（`Authorization`、`Proxy-Authorization`、`Cookie`）はクロスオリジンリダイレクト（異なるホストまたはポート）ではストリップされます
- `Location` ヘッダーが欠落しているか空の場合、リダイレクトレスポンスがそのまま返されます
- 呼び出し元はすべてのリダイレクトをフォローした後の最終レスポンスのみを受け取ります

## エラー処理

- `listen()` は `bind()` が失敗した場合（例: ポートが既に使用中）にランタイムエラーを発生させます。
- 不正なリクエストやキープアライブ接続のアイドルタイムアウトにより接続が閉じられます。その後サーバーは新しい接続の受け入れを再開します。
- ハンドラ関数は常に `HttpResponse` を返す必要があります。デフォルトのレスポンスはありません。
- クライアント関数は `Result<HttpClientResponse, Error>` を返します。成功と失敗を処理するには `match` を使用してください。
