# orto roadmap

Куда мы идём. Порядок — приоритет, не жёсткий план.

См. также:
- `TODO.md` — мелкие технические долги.
- `structure.html` — карта понятий компилятора.
- `CLAUDE.md` — философия языка и режим работы.

---

## ✓ Что есть сейчас

**Память:**
- Один линейный примитив — `Region`.
- Три способа создать: `region(N)` (heap), `stack_region(N)` (стек, N литерал), `aligned_region(N, A)` (posix_memalign, A литерал power-of-2).
- `Array[T]` — копируемая ручка в Region. Gen-check + bounds-check на доступ.
- `slice(a, lo, hi)` — sub-handle в тот же Region, без копирования.
- Slab allocator под капотом — никаких leak'ов, slots переиспользуются.

**Линейные типы (resource ownership):**
- `linear struct Socket { fd: int }` / `linear enum ...` — типы помечены как resource.
- Обязательная `fn drop_<TypeName>(x: TypeName) -> int` в том же модуле. Компилятор требует.
- `let y = x` где x линейный — compile error. Aliasing запрещён.
- `let mut x = ...` линейного — compile error.
- Линейные нельзя класть в data position (поле обычной struct, generic param, элемент Array).
- `drop(x)` — explicit consume, запускает destructor.
- Auto-drop в конце scope если не consumed.
- Branch divergence — нельзя забыть drop в ветке.
- Region — частный случай linear типа: автогенерируемый `drop_Region`.

**Pattern matching:**
- Match по int, bool, byte, Array[byte], ADT.
- Literal patterns: `42`, `-3`, `true`, `"hello"`.
- Bind pattern: `x => body` биндит scrutinee к x (lowercase ident).
- Or-patterns: `1 | 2 | 3 =>`, `Red | Green | Blue =>`.
- Guards (non-ADT): `x if x > 0 => ...`. Guard не считает arm как "covering" — может быть тот же ctor/literal unguarded дальше.
- Exhaustivity:
  - ADT — все ctor покрыты или catch-all.
  - Bool — true и false unguarded, или catch-all.
  - Int/Byte/Bytes — catch-all обязателен.

**Типы:**
- `int`, `bool`, `byte`, `float`, `struct`, `enum`, `fn(...) -> ...`.
- `float` — IEEE 754 double (C `double`). NaN/Infinity по IEEE: `x != x` ловит NaN.
- Generics с параметрами `[T, U, ...]`.
- Параметрический полиморфизм, monomorphization.
- Запрет линейных типов в data position (поле, вариант, type-arg).
- Запрет линейных типов в позиции generic параметра (Region не пройдёт через `[T]`).
- `to_int(byte|float) -> int`, `to_byte(int) -> byte`, `to_float(int) -> float` — явные конверсии.
- `+ - * /` работают для int и float (оба операнда того же типа). `%` — только int.
- `match` на float запрещён (NaN/zero edge cases); используй `if` или bind+guard.

**Строки:**
- `Array[byte]` — единственный тип строки. Никаких String/&str/CString/Cow.
- Литералы `"hello"` живут в статическом регионе (slot 0, never freed). Дедуплицируются.
- Escape sequences: `\n \t \r \0 \\ \" \'`.
- `to_int(b: byte) -> int`, `to_byte(n: int) -> byte` — явная конверсия.
- Операции (concat, eq, find, parse, etc.) — программист пишет как обычные функции, принимающие Region. Появятся в stdlib когда модули.

**Raw pointers `*T` (escape hatch для FFI):**
- Тип `*T` — копируемый, без gen-check, без bounds-check.
- `c_alloc[T](n) -> *T` — malloc(n*sizeof(T)).
- `c_free(p)` — free; программист сам решает когда.
- `*p` deref, `p[i]` индекс, `p[i] := v` запись.
- `null_ptr[T]() -> *T`, `is_null(p) -> bool` — для NULL-returning C-API.
- `array_data(a: Array[T]) -> *T` — отдать байты Array в libc/C-функцию.

**I/O — через io_uring:**
- Целевые ядра: Linux 5.6+. Не Windows, не macOS, не старые ядра.
- Все file/socket/timer/pipe I/O идут через `iouring.orto` ring API.
- Direct syscalls (`sys.orto`) остаются только для setup и того что в io_uring нет (process control, signals).
- Stage 1 (есть): synchronous façade — один SQE submit + один CQE wait per call. `examples/file_io_uring.orto` показывает.
- Stage 2 (есть): batched submit + multiple in-flight ops. `examples/iouring_batch.orto`.
- Stage 3 (в работе): ring-native completion-based concurrency. `await`, `await all { }`, `spawn`, `yield`, `Stream[T]`. Без `async`-раскраски, без `Future`/`Pin`. См. `STAGE3_ASYNC.md`.
  - Фаза 1 (есть): синтаксис — лексер/парсер/AST принимают `await`/`await all`/`spawn`/`yield`. Чекер бросает «not yet implemented».
  - Фазы 2–7: типизация, индуцированная линейность контейнеров (`Array[Task]`), стейт-машина, рантайм-диспетчер, multishot streams, yield/cancel.

**Управление:**
- Всё — выражения. `if`/`match`/`let` возвращают значения.
- Dead-name tracking — use-after-move = compile error.
- Ветки `if`/`match` обязаны сходиться в одном live-set.
- Tail-position consume — bare имя в хвосте функции = move.
- `else if` цепочки без вложенных скобок.
- `if cond { ... }` без else допустим (implicit else = int 0).
- `while cond { body }` — циклы. `break` / `continue` внутри.
- `let mut x = ...; x := v;` — изменяемые биндинги. Запрещён `mut` для Region (избегаем утечек через reassign).
- Trailing `;` перед `}` отбрасывает значение выражения, блок возвращает int 0.

**Модули:**
- Один файл = один модуль, имя из имени файла (`str.orto` → `str`).
- `use foo::bar;` или `use foo::{a, b, c};` — selective import.
- Driver автоматически подгружает referenced модули из той же директории. Циклы — compile error.
- Mangling: `concat` в `str.orto` становится `str__concat`. References в `use'й`-щем модуле резолвятся прозрачно.
- Builtin names (`Array`, `Region`, `Option`, `Some`, `None`, `byte`) и `main` не мангляются.
- Externs не мангляются (имя в C = имя в orto), дедуплицируются по имени.

---

## → Дорога вперёд

Базовая ось языка закрыта. Что осталось — это **stdlib** (это уже
orto-код, не compiler) и крупные архитектурные шаги, требующие
обсуждения с Francois.

### 1. Stdlib *(в работе)*

Сейчас в `examples/` есть прото-stdlib:
- `str.orto` — операции над `Array[byte]`: eq, find, parse_int, concat, concat_all, split_byte, bytes_join, trim, to_lower/upper, c_str, etc.
- `io.orto` — print, println, putchar.
- `sys.orto` — `linear Fd` + syscall wrappers (open, read, write, close, socket, bind, sendto, recvfrom, etc.).
- `bin.orto` — binary read/write helpers (u16/u32 LE/BE) для netlink/network protocols.

Что нужно добавить (по приоритету из netlink анализа в `NETLINK_ANALYSIS.md`):
- **Hashmap** — линейный scan `Array[(K, V)]` болезнен at scale. Hand-written without generics, или через monomorphization. Большая stdlib работа.
- `std::gen_arena` — generational arena для resource pools, evicting caches.
- `std::slab` — slab pool для homogeneous-size объектов.
- `std::ring` — ring buffer.
- `std::result` — convention для error handling (или сами enum'ы).

---

## ◯ Открытые дизайн-вопросы

Эти не блокируют. Каждый — серьёзная архитектурная работа, требующая обсуждения.

- **Closures / lambdas.** Сейчас только именованные функции. Closures открывают callbacks, higher-order patterns (`map`, `fold`, `filter`). Усложнение — capture analysis, runtime representation (fat pointer), interaction с linear типами.

- **Threading.** Уже expressible через linear типы — `linear struct Thread { id: int } drop_Thread = pthread_join`. Channels — `linear Sender`, `linear Receiver` с send/recv. Atomic primitives через `extern fn` (memory barriers от C). Не требует новой концепции — большая работа в stdlib + extern wrappers. См. `NETLINK_ANALYSIS.md`.

- **Variadic format `format(r, "...", a, b, c)`** — сильно болит в практике (netlink error messages, debug print). Без неё `concat_all + int_to_bytes` chains.

- **Generic Option/Result для linear types** — сейчас `Option[Fd]` запрещён (Fd linear, Option не linear). Workaround: возвращать raw int + wrap manually (`fd_wrap`). Чище — разрешить linear-aware generic instantiation: если T linear, container становится linear.

- **`format(r, "...", a, b, c)` variadic.** Текущее `concat_all(r, array(r, [...]))` многословно. Variadic + типизированные args существенно улучшат, но variadic — серьёзная фича.

- **Nested patterns в match.** `Some(0) =>`, `Some(_) =>`. Сейчас `Some(x)` биндит x, литерал на месте не работает. Закроется guards (есть!) на 80%; nested cleaner но big refactor.

- **Guards в ADT match.** Сейчас запрещены — пользователь пишет `if` внутри arm body. Чтобы разрешить, нужен `goto`-based fallthrough в ADT switch emit.

- **Type-level alignment / sizes.** `Region[Page]` vs `Region[Default]` через тип. Сейчас alignment runtime через keyword.

- **Implicit allocator / `alloc fn`.** Обсуждалось — решено НЕ делать. Видимость аллокации (`r` параметр) — якорь философии orto.

---

## Чего **не** будет

- **GC.** GC — это сдача. У нас линейный Region и компиляторная проверка lifetime'ов.
- **Borrow checker в Rust-стиле.** У нас runtime gen-check вместо статических lifetime'ов. Проще для программиста, цена — несколько ns на доступ.
- **Trait/type classes.** Только параметрический полиморфизм. Если нужна "одна функция для разных типов" — обычные generics.
- **Implicit conversions.** Никаких неявных кастов. Программист всё пишет руками.
- **Множественные стратегии аллокации в одной программе как "выбор аллокатора".** У нас Region. Если C-interop — raw pointers escape hatch.

---

## Долгосрочно

Когда базовый язык устаканится и Piano (проект Francois) начнёт активно использовать orto — посмотрим что реально болит и добавим минимально.

Никаких "фич ради фич". Каждая новая ось должна закрывать **реальную** боль из практики.
