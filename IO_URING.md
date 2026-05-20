# Направление: io_uring как основной путь I/O

## Цель

orto компилируется под современный Linux. Не под Windows, не под
macOS, не под старые ядра. **Все I/O — file, socket, timer, pipe —
проходит через io_uring**, не через классические блокирующие
syscalls. Это даёт:

- одну точку API для всех видов I/O (нет split file vs socket);
- batching и async без extra языковых концепций (когда дойдём);
- меньше context switches на нагруженных workloads;
- современную семантику отмены через `IORING_OP_ASYNC_CANCEL`.

Прямые syscalls остаются только для:
- setup (`io_uring_setup`, `io_uring_enter`);
- того что в io_uring нет: process control (`fork`, `execve`,
  `wait`), signals, memory (`mmap`, `mprotect`).

Целевые ядра: **Linux 5.6+**. Если фичи нужны новее — поднимем
минимум.

---

## Стадии

### Stage 1 (сделано) — synchronous façade

`examples/iouring.orto` + `examples/iouring_helpers.c`:

- `linear struct Ring { handle: *byte }` — kernel ring buffer пара.
- `ring_new(entries) -> Ring`, `drop_Ring` авто-вызывает
  `io_uring_queue_exit`.
- Operations: `uring_read`, `uring_write`, `uring_open`, `uring_close`,
  `uring_fsync`, `uring_socket`, `uring_connect`, `uring_accept`,
  `uring_send`, `uring_recv`.
- Каждая op: submit one SQE + wait one CQE + return `res`.

Demo `examples/file_io_uring.orto` — file roundtrip через ring,
**ни одного прямого syscall** на I/O в orto-коде. Работает.

Бенефит на этой стадии — близко к нулю (overhead больше чем прямой
syscall). Цель — proof of concept и стабильный API.

### Stage 2 (план) — батч + ручной async

Расширить с одной operation в полёте до многих:

- `uring_submit_read(r, fd, buf, off) -> SubmissionId` — кладёт SQE,
  возвращает идентификатор. Не submit-ит и не ждёт.
- `uring_flush(r) -> int` — `io_uring_submit`, отправляет всё что
  накопилось в SQ kernel-у.
- `uring_wait_one(r) -> Completion { id, res }` — ждёт один CQE.
- `uring_wait_all(r, n) -> Array[Completion]` — ждёт N.

Это даёт реальный benefit io_uring: программист batch'ит N ops,
один syscall submit, then drain completions in order.

API всё ещё synchronous-blocking но операции теперь parallel.

### Stage 3 (будущее) — `async fn` + `await`

Большая работа. Coroutines (state machine, stack-less).

```orto
async fn read_file(r: Region, path: Array[byte]) -> Array[byte] {
    let fd = await uring_open(path, o_rdonly(), 0);
    let buf = array(r, 4096, to_byte(0));
    let n = await uring_read(fd, buf, 0);
    await uring_close(fd);
    slice(buf, 0, n)
}
```

Compiler раскрывает в state machine. Runtime — один thread + ring
events drive поллинг. Multiple `async fn` running concurrently на
одном потоке. Это то ради чего весь подход.

Требуется:
- `async fn`/`await` syntax (~200 строк parser/check).
- State machine генерация в emit (~400 строк).
- Runtime scheduler (~300 строк C).
- Тщательно с linear types — нельзя suspend в середине scope
  владеющего линейным значением без явного move в frame.

Не блокирующий шаг для текущей работы — отдельный roadmap item.

---

## Что НЕ меняется в Stage 1

- `sys.orto` остаётся. Direct syscalls работают, но **не**
  идиоматический путь. Используются только для setup, process control.
- `Fd` linear struct — пара с io_uring fd. `uring_open` возвращает
  raw int, юзер wraps в `Fd` через `fd_wrap` если хочет linear-drop.
- Memory model (Region, linear types) — unchanged.
- Modules, pattern matching, всё остальное — то же.

io_uring это **новое крыло**, не переделка ядра языка.

---

## Build

io_uring требует liburing на dev-машине:

```
apt-get install liburing-dev   # Ubuntu/Debian
dnf install liburing-devel     # Fedora
```

Сборка demo:

```
dune exec bin/main.exe -- examples/file_io_uring.orto
cc examples/file_io_uring.c examples/iouring_helpers.c \
   -luring -fsanitize=address,leak -g -o /tmp/file_io_uring
/tmp/file_io_uring
```

В будущем driver научится автоматически линковать `liburing` и
helpers — это nice-to-have, не блокирует.
