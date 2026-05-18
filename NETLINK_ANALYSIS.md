# Сможет ли orto написать netlink-уровень кода

Сравнение с `vishvananda/netlink` (Go) — самая популярная Go-библиотека
для Linux netlink. Прочитан 4 файла, проанализирована каждая фича.

---

## Резюме

**~90% синхронной netlink-функциональности экспрессабельно сегодня**
на orto. Главные блокеры — threading и closures, которые нужны только
для goroutine-based subscription API (`Subscribe(ch, done)`).

Для **CLI-инструмента**, **config-демона**, **init-системы** —
single-threaded netlink клиент пишется на orto без новых compiler
фичей. См. `examples/netlink_msg.orto` — message construction +
parsing работает.

---

## По файлам

### `nl/nl_linux.go` — socket setup + request/response engine

**Что использует:**
- Syscalls: `Socket(AF_NETLINK, SOCK_RAW, protocol)`, `Bind`, `Sendto`, `Recvfrom`, `setsockopt`, `Close`.
- Binary: byte-order detection at runtime, `binary.BigEndian.PutUint32`.
- `unsafe.Pointer` для cast `[]byte` ↔ struct.
- `sync.Mutex`, `atomic.AddUint32`, `runtime.LockOSThread`.

**В orto сегодня:**
- ✅ Syscalls: `extern fn` + `linear struct Socket {fd: int}` + `drop_Socket`.
- ✅ Binary: explicit per-byte writes (`std::bin`).
- ⚠️ Reinterpret cast — нет, мы копируем. Минорный perf hit.
- ❌ Mutex/atomic — **no threading axis**.
- ❌ `LockOSThread` для netns — нет.

### `nl/addr_linux.go` — `IfAddrmsg` serialization (72 строки)

**В orto сегодня — fully expressible**, ~30 строк. Используем `struct
IfAddrmsg { ... }` + хелперы из `std::bin`. См. `netlink_msg.orto` —
точно этот pattern для NlMsgHdr.

### `link_linux.go` — link operations + parsing (3461 строки)

**Что использует:**
- Сообщения building через `NewIfInfomsg() + AddRtAttr()` — повторяющийся 6-строчный pattern.
- **Polymorphic `Link` interface** (Bridge, Bond, Veth, ...).
- `binary.Read` через reflection.

**В orto сегодня:**
- ✅ Setters (`LinkSetUp`, `LinkSetMTU`) — 8 строк каждая.
- ✅ Polymorphism — sum-type `enum Link { Bridge(BridgeData), Bond(...), Veth(...), ... }`. **Лучше** чем Go interface для kernel API (closed-world).
- ⚠️ `binary.Read` reflection — нет; пишется по полю вручную (60 строк для `parseLinkStatistics64`). Tedious, но честно.

### `route_linux.go` (1426-1520) — `RouteSubscribe` через goroutines

**Что использует:** `go func() {...}()`, `chan<- RouteUpdate`, `<-done`, `defer`.

**В orto сегодня — NOT expressible.** Это требует threading и closures.

**Workaround:** API дизайнится как pull (`next_event(s) -> Option[RouteUpdate]`), caller сам loop'ит. Это не worse — Linux `epoll` и Rust `mio` работают так — но это **другая** библиотека.

---

## Что хорошо ложится на orto

| Фича | Orto подход |
|---|---|
| Сокет | `linear struct Socket {fd: int}` + `drop_Socket = close(fd)` — **строго лучше** чем Go's `Close()`-by-discipline (нельзя забыть fd). |
| Сообщение | `Array[byte]` в per-request `Region` — точно "build, send, parse, throw away" lifecycle. |
| TLV serialization | `std::bin` write_u16/u32/_le/_be. По 6-8 строк на тип. |
| Link kinds (Bridge, Bond, ...) | `enum Link { ... }` closed-world. Лучше чем Go interface. |
| `net.IP` / `net.HardwareAddr` | Это just `Array[byte]`. Бесплатно. |
| Error coding | `enum Result[T, E]` через convention. |

---

## Что не хватает (stdlib, не compiler)

1. **Hashmap** — `map[int]*SocketHandle` в Go. Без него — linear scan через `Array[(K, V)]`. Strikingly painful at scale.

2. **Variadic format** — `fmt.Errorf("...%d", x)` много где. Сейчас `concat_all` с `int_to_bytes`. Работает но многословно.

3. **Generic Result/Option для linear types** — пока workaround: возвращать raw int + wrap manually (см. `sys.orto:fd_wrap`).

---

## Что не хватает (compiler)

1. **Threading + closures** — для goroutine-style subscriptions. **Не критично**, можно жить с pull API. Если делать — большая архитектурная работа (atomics, OS thread model, capture analysis).

2. **`unsafe.Pointer` reinterpret cast** — Go castит `[]byte → struct*` без копирования. У нас copy. Perf hit на больших messages, но мысленно проще без unsafe.

3. **Reflection** — `binary.Read(stats)` рефлексирует struct fields. У нас — per-field code. Нечестно — это reflection не нужен в pure-systems языке (philosophy).

---

## Идеи для языка из netlink анализа

**Нужны:**
- Хешмап в stdlib.
- Variadic format() или string interpolation.
- Generic `Result[T, E]` (через linear container если T линеен).

**Возможно полезно:**
- Bit-flags syntax `0b00000001` или хотя бы named bitwise ops (`|`, `&` для int).
- Hex literals `0x1234` для readable constants.
- `union` тип для C interop (хотя сейчас через `*byte` + bin offset работает).

**НЕ нужно:**
- Сам `unsafe.Pointer` reinterpret — наш explicit-bytes подход чище.
- Reflection — антипаттерн для systems.
- Open-world interface — закрытые sum types выиграют для kernel API.
- Implicit allocator — Region передаётся явно, OK для netlink (per-request region).
- GC — никогда.

---

## Threading discussion

`linear struct Thread { id: int }` + `drop_Thread = pthread_join(...)`
— natural. Spawn принимает function pointer (extern fn signature),
не closure для v1. Это ограничивает но дает шаг вперёд.

Channels между потоками — тоже linear handle с send/recv operations.
Sender owns Sender, receiver owns Receiver. Один-к-одному. Запрещаем
sharing на уровне типов.

Это **expressible** через текущий linear types model, без atomic
primitives на уровне языка (atomics через extern fn + memory barriers
от C). Большая работа но не require новой концепции.

Подходит как roadmap item когда понадобится.
