[English](../../reference/net.md)

# ネットワーク（TCP）リファレンス

## 型

| 型 | 説明 |
|------|-------------|
| `TcpListener` | TCP サーバーソケットの不透明ハンドル |
| `TcpStream` | TCP 接続の不透明ハンドル |
| `TlsStream` | TLS 暗号化された TCP 接続の不透明ハンドル |

両方の型は不透明ポインタです。直接構築することはできません。`TcpListener`/`TcpStream` を取得するには `bind()` または `connect()` を使用し、`TlsStream` には `tls_connect()` を使用します。

## 関数（`net` パッケージ）

これらの関数は明示的なインポートが必要です:

```python
from net import bind, listen, accept, connect, listener_port, shutdown, set_timeout, set_recv_timeout, set_send_timeout, tls_connect
```

| 関数 | シグネチャ | 説明 |
|----------|-----------|-------------|
| `bind` | `(host: str, port: int) -> Result<TcpListener, Error>` | 指定アドレスにバインドされた TCP サーバーソケットを作成します。失敗時は `Err` を返します。動的割り当てにはポート `0` を使用します。 |
| `listen` | `(listener: TcpListener, backlog: int) -> Result<Unit, Error>` | 受信接続のリスニングを開始します。失敗時は `Err` を返します。 |
| `accept` | `(listener: TcpListener) -> Result<TcpStream, Error>` | 新しい接続を受け入れます。クライアントの接続を最大 1 秒待ちます。タイムアウトまたは失敗時は `Err` を返します。 |
| `connect` | `(host: str, port: int) -> Result<TcpStream, Error>` | リモート TCP サーバーに接続します。5 秒後にタイムアウトします。タイムアウトまたは失敗時は `Err` を返します。 |
| `listener_port` | `(listener: TcpListener) -> int` | リスナーが実際にバインドされているポート番号を返します。ポート `0`（OS が割り当てるポート）でバインドした場合に便利です。 |
| `shutdown` | `(listener: TcpListener) -> Unit` | リスナーに接続受け入れの停止を通知します。保留中の `accept()` は最大 1 秒以内に返されます。 |
| `tls_connect` | `(host: str, port: int) -> Result<TlsStream, Error>` | TLS 暗号化でリモートサーバーに接続します。サーバー証明書をシステム CA バンドルに対して検証します。接続またはハンドシェイク失敗時は `Err` を返します。 |
| `set_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | 受信と送信の両方のタイムアウトをミリ秒単位で設定します。 |
| `set_recv_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | 受信タイムアウトをミリ秒単位で設定します。この時間内にデータが届かない場合、`recv()` は `Err` を返します。 |
| `set_send_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | 送信タイムアウトをミリ秒単位で設定します。 |

## 組み込みオーバーロード関数

これらの関数は組み込みで、TCP ソケット型で動作します。インポートは不要です。

| 関数 | シグネチャ | 説明 |
|----------|-----------|-------------|
| `send` | `(stream: TcpStream\|TlsStream, data: List<u8>) -> Result<int, Error>` | TCP または TLS 接続を通じてバイトを送信します。成功時は送信バイト数を含む `Ok` を返し、失敗時は `Err` を返します。 |
| `recv` | `(stream: TcpStream\|TlsStream, max: int) -> Result<List<u8>, Error>` | 最大 `max` バイトを受信します。接続クローズ時は空のリストを含む `Ok` を返し、エラー時は `Err` を返します。 |
| `close` | `(handle: TcpStream\|TlsStream) -> Unit` | TCP または TLS ストリームを閉じます。 |
| `close` | `(handle: TcpListener) -> Unit` | TCP リスナーを閉じます。 |

## 使用例

### エコーサーバー

```python
from net import bind, listen, accept, connect
from io import str_to_bytes, bytes_to_str

# サーバー
match bind("127.0.0.1", 8080):
    case Ok(server):
        match listen(server, 128):
            case Ok(_):
                match accept(server):
                    case Ok(conn):
                        match recv(conn, 4096):
                            case Ok(data):
                                match send(conn, data):
                                    case Ok(_):
                                        ...
                                    case Err(e):
                                        print(e.message)
                            case Err(e):
                                print(e.message)
                        close(conn)
                    case Err(e):
                        ...
            case Err(e):
                print("listen failed")
        close(server)
    case Err(e):
        print("bind failed")
```

### クライアント

```python
match connect("127.0.0.1", 8080):
    case Ok(conn):
        match send(conn, str_to_bytes("hello")):
            case Ok(_):
                ...
            case Err(e):
                print(e.message)
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(s):
                        print(s)
                    case Err(e):
                        print(e.message)
            case Err(e):
                print(e.message)
        close(conn)
    case Err(e):
        print("connect failed")
```

### `async fn` による並行エコーサーバー

```python
from net import bind, listen, accept, connect, listener_port
from io import str_to_bytes, bytes_to_str

async fn echo_server(server: TcpListener) -> str:
    match accept(server):
        case Ok(conn):
            match recv(conn, 4096):
                case Ok(data):
                    match send(conn, data):
                        case Ok(_):
                            ...
                        case Err(e):
                            ...
                case Err(e):
                    ...
            close(conn)
        case Err(e):
            ...
    close(server)
    return "done"

match bind("127.0.0.1", 0):
    case Ok(server):
        match listen(server, 1):
            case Ok(_):
                port = listener_port(server)
                t = echo_server(server)
                # ... port を使用するクライアントコード ...
                block_on(t)
            case Err(e):
                ...
    case Err(e):
        ...
```

## タイムアウト設定

デフォルトでは、カスタムタイムアウトが設定されていない場合、`recv()` は 30 秒のタイムアウトを使用します。デフォルトを上書きするには `set_timeout()`、`set_recv_timeout()`、または `set_send_timeout()` を使用します:

```python
from net import connect, set_recv_timeout

match connect("127.0.0.1", 8080):
    case Ok(conn):
        set_recv_timeout(conn, 5000)  # 5 秒のタイムアウト
        match recv(conn, 4096):
            case Ok(data):
                ...
            case Err(e):
                print("timeout or error")
        close(conn)
    case Err(e):
        print("connect failed")
```

タイムアウトを無効にする（無期限に待機する）には `0` を渡します。

## エラー処理

- `close()` を除くすべての TCP 関数は `Result<T, Error>` を返します。失敗を処理するには `Ok`/`Err` で `match` を使用してください。
- `recv()` はピアによって接続が閉じられた場合は空の `List<u8>` を含む `Ok` を返し、実際のエラー（タイムアウト、ソケットエラー）の場合は `Err` を返します。
- `close()` はソケットを閉じてハンドルを解放します。閉じた後のハンドルの使用は未定義動作です。

## バイト変換

TCP 操作は `List<u8>` を使用します。文字列とバイトリストの変換には `io` パッケージの `str_to_bytes()` と `bytes_to_str()` を使用してください。
