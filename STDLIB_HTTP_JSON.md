# Стратегия для stdlib — HTTP, JSON и другие «батарейки»

Этот документ — обсуждение того, как добавлять «реальные» библиотеки
вроде HTTP и JSON-парсера в orto. Это не одна задача, а ось развития:
выбрать политику сейчас, чтобы каждая последующая библиотека следовала
ей.

---

## Вопрос: где живёт код?

Три радикально разных подхода:

### Подход А — всё в C через FFI (Python style)

`http.orto` — это тонкий wrapper над libcurl / libhttp_parser / libuv-
подобной C-библиотекой. Mы пишем `extern fn` declarations, всю работу
делает C.

**Pro:** мало кода в orto, проверенные C-реализации, моментально готово.

**Con:**
- Stdlib становится списком FFI-зависимостей. Программа на orto тащит
  liburing + libcurl + libssl + ... — у пользователя всё это должно
  быть в системе.
- Семантика языка перестаёт быть видимой. `http_get(url)` это чёрный
  ящик. Мы теряем способность рассуждать о памяти, ошибках, async-
  поведении из кода на orto.
- Linear types не помогают, C игнорирует наши гарантии.
- Дебаггер показывает C-стек.
- Кросс-компиляция становится зависимой от target's C ecosystem.

### Подход Б — Zig style: всё в нашем языке поверх io_uring

`http.orto` написан **на orto**: парсит байты руками, формирует
запросы конкатенацией строк, читает/пишет через `await io_uring`.
Никакой libcurl — только сам orto и его syscall'ы.

**Pro:**
- Один язык от main до tcp socket. Программист читает один синтаксис.
- Linear types и Region работают сквозь весь stack. HTTP-парсер
  выделяет буферы в Region пользователя, никаких скрытых malloc.
- Errors через `Result[T]` — единая модель.
- Async-поведение видимое: `for conn in accept_stream()` это HTTP
  сервер, никакой магии под капотом.
- Кросс-компиляция тривиальна — нужен только orto компилятор и
  liburing.

**Con:**
- Большой объём кода. HTTP/1.1 parser ~500 LOC, TLS — десятки тысяч
  (это слишком много).
- Производительность зависит от качества нашей реализации.
- Мы повторяем работу проверенных C-библиотек.

### Подход В — гибрид: примитивы в C, протоколы в orto

Низкоуровневые вещи без выбора (TLS handshake, low-level CPU
crypto) — через C extern. Протокольный layer (HTTP-запрос как
байты, JSON как структура) — на orto.

**Pro:** баланс. Видимость семантики там где нужно, C только где
нет реалистичной альтернативы.

**Con:** двойственность — где провести границу? Размытое правило
требует случайных решений.

---

## Что делает Zig

Zig в stdlib имеет:
- `std.net` — TCP/UDP через sockets, **на Zig**. Поверх syscalls.
- `std.http` — HTTP/1.1 client + server, **на Zig**. Поверх std.net.
- `std.json` — JSON parser/encoder, **на Zig**.
- `std.crypto` — основные crypto primitives (ChaCha20, Poly1305,
  SHA, Curve25519), **на Zig**. Aes-через-hardware intrinsics.
- TLS — **на Zig**. std.crypto.tls — Zig имплементация TLS 1.2/1.3.

Zig **не использует** libcurl, libssl, libhttp_parser, либо аналог.
Их stdlib полностью самодостаточна — это сознательное решение Andrew
Kelley.

Аргумент: «hermetic compilation» — твоя программа собирается из
одного источника, не зависит от каких-то системных libs которые
могут отсутствовать или быть несовместимыми.

Что Zig использует из C: libc для базовых syscalls (на платформах
где syscall ABI стабилен только через libc) и нескольких компонент
вроде LLVM при сборке. Run-time зависимостей на C-libs у Zig
программы — почти ноль.

---

## Что делает Go

Полная противоположность. Go всё пишет на Go (`net/http`, `crypto/tls`,
`encoding/json`), но runtime в crypto активно использует ассемблер
для производительности.

---

## Что делает Rust

Hybrid. Стандартная библиотека минимальная: `std::net::TcpStream`
(syscall wrapper), но HTTP/JSON/TLS — это crates (`reqwest`,
`serde_json`, `rustls`). Эти crates написаны на чистом Rust.

---

## Что выберем мы

Подход Б (Zig style), с оговорками.

**Почему:**

1. **Hermetic builds.** Программа на orto должна компилироваться в
   статический бинарь который запускается без `apt install libcurl4`.
   liburing — единственная run-time C-зависимость (и для неё уже
   делается аргумент потому что без неё нет async). Добавлять ещё
   одну зависимость, потом ещё одну — это путь Python.

2. **Видимость семантики.** Один из главных принципов orto — нет
   магии (`CLAUDE.md`: «когда мы добавляем поведение, оно должно
   быть видимым в коде»). FFI-черные ящики противоречат этому. Если
   программист читает наш HTTP клиент, он должен видеть:
   ```orto
   let req = format_bytes(r, "GET / HTTP/1.1\r\n...");
   await orto_async_send(fd, array_data(req), len(req));
   let buf = array(r, 4096, to_byte(0));
   let n = await orto_async_recv(fd, array_data(buf), 4096);
   ```
   Это **прозрачно**. Через libcurl — непрозрачно.

3. **Linear types работают сквозь.** Если HTTP-парсер написан на C,
   он не понимает наш Region/Task. Все buffers будут heap-allocated
   через malloc. Это разрушает модель: пользователь думает что
   region-based, а под капотом GC-like behaviour.

4. **Размер кода управляем.** HTTP/1.1 parser — это ~500 LOC, JSON
   parser — ~300 LOC. Это окей. TLS — да, много, **но это и в Zig
   много**, и это honest tradeoff. Для V1 можно TLS не делать,
   и брать HTTPS через C-side TLS (как escape hatch).

---

## Конкретный план

### Фаза N — стандартные сетевые примитивы (без HTTP)

В io_uring через async extern:
- `orto_async_socket(domain, type_, protocol) -> Task[int]`
- `orto_async_connect(fd, addr, addrlen) -> Task[int]`
- `orto_async_send(fd, buf, n, flags) -> Task[int]`
- `orto_async_recv(fd, buf, n, flags) -> Task[int]`
- `orto_async_accept(srv) -> Task[int]`
- `orto_accept_stream(srv) -> Stream[int]` (multishot)
- `orto_recv_stream(fd) -> Stream[int]` (multishot, для full-duplex
  connection где данные приходят непредсказуемо)

Эти extern имплементируются в `examples/iouring_helpers.c` уже частично
сейчас. Расширяем.

Чисто на orto:
- `net::ipv4(a, b, c, d, port) -> SockAddr` — упаковывает struct sockaddr_in
- `net::resolve_dns(name) -> Result[SockAddr]` — synchronous syscall в getaddrinfo через FFI (или async через io_uring getaddrinfo если ядро поддерживает)

### Фаза N+1 — HTTP/1.1 client

Полностью на orto. Структура:
- `http::Request { method: Method, path: Array[byte], headers: Array[Header], body: Array[byte] }`
- `http::Response { status: int, headers: Array[Header], body: Array[byte] }`
- `http::format_request(r: Region, req: Request) -> Array[byte]` — сериализует в bytes.
- `http::parse_response(r: Region, bytes: Array[byte]) -> Result[Response]` — парсит response state machine'ом.
- `http::get(r: Region, host: Array[byte], port: int, path: Array[byte]) -> Result[Response]` — high-level wrapper: resolve + connect + send + recv + parse.

Use of Region для buffer ownership. Response.body это slice в region пользователя. Никакой скрытой памяти.

State machine HTTP parser — это где-то 300-500 LOC orto. Доступно.

### Фаза N+2 — HTTP/1.1 server

```orto
fn handle(conn: int, req: Request) -> Response {
    Response { status: 200, headers: ..., body: ... }
}

fn main() -> int {
    let srv = net::listen_tcp(8080);
    for conn in orto_accept_stream(srv) {
        if conn < 0 { break }
        spawn http::serve(conn, handle);
    }
    0
}
```

`http::serve` — на orto. Парсит запрос, вызывает handler, форматирует
response, шлёт.

### Фаза N+3 — JSON

Полностью на orto:
- `json::Value = enum { JNull, JBool(bool), JNum(float), JStr(Array[byte]), JArr(Array[JValue]), JObj(Array[(Array[byte], JValue)]) }`
  — рекурсивный ADT.
- `json::parse(r: Region, bytes: Array[byte]) -> Result[Value]` — state machine parser.
- `json::format(r: Region, v: Value) -> Array[byte]` — обратное.

JSON — ~300 LOC orto. Простая state machine.

### Фаза N+4 — TLS

**Здесь делаем исключение.** TLS — ~30K LOC даже минимально (см. BoringSSL,
rustls). Это слишком много чтобы писать на orto в V1.

Варианты:
1. **TLS через libtls (LibreSSL portable):** одна C-зависимость, чище
   чем OpenSSL. Программа на orto тащит libtls.so при HTTPS.
2. **TLS через системный OpenSSL/BoringSSL:** распространено, но API
   уродливое.
3. **TLS через Zig std.crypto.tls FFI:** если есть способ.
4. **Native TLS на orto:** долгосрочный проект. Сначала пишем все
   crypto primitives (требует SIMD intrinsics, hardware AES — а
   этого в orto пока нет), потом сам TLS state machine.

Для V1: **выбираем (1)**. HTTPS клиент через libtls. Документируем как
исключение.

В V2 можем пилить native TLS.

---

## Что нужно в языке чтобы это работало

HTTP/JSON парсеры — это state machines. Не сильно требовательны к
языку. Что им нужно сейчас и чего может не быть:

- ✅ Array[byte] — есть.
- ✅ Pattern matching на байтах — есть (literal patterns).
- ✅ Recursive ADT (для JSON Value) — есть.
- ✅ Region для buffer ownership — есть.
- ✅ Result для error propagation — есть.
- ⚠️ Bignum / arbitrary precision int — нет. JSON numbers могут не
  помещаться в int64. Для V1 ограничимся int64 и flag overflow как
  Err.
- ⚠️ Hashmaps — нет в stdlib. JSON object как Array[(key, value)] —
  O(n) lookup. Для V1 OK для маленьких объектов; для production
  нужен hashmap. Можно добавить как builtin.
- ⚠️ String manipulation — есть базовое (`str::concat`, `slice`,
  `find_substr` etc.), но не полное (нет regex). Для HTTP/JSON
  parsing хватит ручной state machine.
- ❌ Closures — нет. HTTP server `route("/foo", |req| ...)` нельзя
  написать без closures. Альтернатива — function pointers + struct
  для captured state. Это работает но менее эргономично.
- ❌ Generics над функциями — есть `fn id[T](x: T) -> T`, но
  callback-style API через generics требует closures.

**Вывод**: V1 stdlib HTTP/JSON делается без closures и без hashmaps.
Эргономика будет немного хуже Go, но семантика прозрачна.

---

## Рекомендуемая последовательность

1. Сначала закрыть V1 Stage 3 (async/I/O) — done.
2. Расширить io_uring stdlib externs до полного network set (socket,
   connect, send, recv, accept, multishot accept/recv).
3. **HTTP/1.1 client + server на orto.** ~1000 LOC stdlib.
4. **JSON на orto.** ~300 LOC stdlib.
5. **TLS через libtls FFI** как escape hatch.
6. (потом) hashmap builtin для production-grade парсеров.
7. (потом) closures если pattern «callback с captured state»
   станет реально нужным.

---

## Открытый вопрос: stdlib vs ecosystem

В Go всё в stdlib. В Rust всё в crates. У нас сейчас всё в `examples/`
которые подгружаются через `use`. Это работает для маленького проекта,
но если мы захотим публичную экосистему — нужен package manager.

Это **отдельная ось** (модулям нужен registry, версионирование,
build system) и сейчас не блокирует HTTP/JSON. Откладываем до того
момента когда у нас появятся реальные пользователи и третьи стороны
захотят публиковать пакеты.

Для V1: всё в `examples/` (текущее), модули загружаются locally.
Запиливать registry — не раньше чем у нас будет N >= 10 серьёзных
пользователей.
