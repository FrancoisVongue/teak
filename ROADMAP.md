# orto roadmap

Куда мы идём. Порядок — приоритет, не жёсткий план.

См. также:
- `TODO.md` — мелкие технические долги.
- `structure.html` — карта понятий компилятора.
- `CLAUDE.md` — философия языка и режим работы.

---

## ✓ Что есть сейчас

**Память — безопасная сторона (`Handle[T]`):**
- Один линейный примитив — `Region`.
- Три способа создать: `region(N)` (heap), `stack_region(N)` (стек, N литерал), `aligned_region(N, A)` (posix_memalign, A литерал power-of-2).
- `Handle[T]` — копируемая ручка в Region. Gen-check + bounds-check на доступ. `Handle` длины 1 = коробка, длины N = буфер.
- `ref(r, v)` / `ref(r, n, init)` / `ref(r, [..])` — аллокация; `r[i]` чтение, `r[i] := v` запись (включая `r[i].field := v`); `len`, `slice(a, lo, hi)` (sub-handle без копирования), `try_at(r, i) -> Option[T]`.
- **`reset(r)`** — фундаментальная операция арены: массовый free с сохранением региона. Бумпает поколение (все живые `Handle` протухают → ловятся gen-check'ом), обновляет биндинг, откатывает bump-указатель. Это переиспользование per-frame / per-request. Операнд — переменная региона.
- Чтобы получить безопасный `Handle` над чужими байтами — копируй в регион (gen-free `Handle` в системе типов не существует). Безопасный `view[T]` (reinterpret внутри региона с авто-валидацией) — **планируется, НЕ реализован**; сейчас reinterpret есть только в сыром мире (`ptr_cast`, ниже).
- Slab allocator под капотом — никаких leak'ов, slots переиспользуются.
- Рекурсивные данные через `Handle`-поля: `enum Tree { Leaf, Node(int, Handle[Tree]) }`. Регион умер — всё дерево разом.

**Память — сырая сторона (`*T`, escape hatch, opt-in):**
- `*T` — голый C-указатель, **отдельный тип** от `Handle[T]`: безопасность видна в типе, не скрытое свойство. Нет gen/bounds-проверок.
- `c_alloc[T](n)` / `c_free(p)`, `*p` deref, `p[i]` / `p[i] := v`, `null_ptr[T]()`, `is_null(p)`, `as_ptr(ref) -> *T`.
- **`ptr_cast[T](e)`** — reinterpret сырого `*U` или int-адреса как `*T` (C-cast). Zero-copy overlay + фиксированные адреса (MMIO).
- **Арифметика указателей** (C-семантика): `p + n` / `p - n` сдвиг на элементы (→ `*T`), `p - q` разница в элементах (→ int), `to_int(p)` адрес как int.
- Граница безопасности: всё сырое и внешнее теряет gen ровно на FFI-границе, как и линейные ресурсы. Чужая память, требующая освобождения, — линейный тип с `drop`, зовущим их функцию; без обязательства — чистое заимствование (мы не освобождаем).

**Byte-codec (stdlib namespaces):**
- `std::bin` — read/write u16/u32/u64 LE+BE над `Handle[byte]` (безопасно). Эндианность host-независима по построению (byte-assembly).
- `std::raw` — то же над сырым `*byte` (foreign-буферы, mmap, пакеты) + `bswap16/32/64`. Зеркало `bin` для сырого мира.

**Идиомы памяти (выразимы как есть, проверено агентами):**
- Двухаренный паттерн (scratch + output) = два параметра `Region`, ноль церемоний.
- Per-frame / per-request = `arena` в scope + `reset` для переиспользования.
- Object pool / slotmap = **контейнер** поверх региона; per-slot generational safety — обычное целочисленное сравнение в библиотечном коде, не примитив языка.
- Фикс-размерные структуры (hash table, ring) = `ref(r, CAP, init)`; «bounded, don't grow» = `insert` возвращает `bool`.
- Идиома: функция берёт `Region` явным параметром только если аллоцирует; если читает/обходит — берёт `Handle` (хендл знает свой регион). (Автоматический ambient-регион / `box` / `in r` и escape-через-сигнатуру — **планируется, НЕ реализовано**; сейчас регион протаскивается явным параметром.)

**Линейные типы (resource ownership):**
- `linear struct Socket { fd: int }` / `linear enum ...` — типы помечены как resource.
- Обязательная `fn drop_<TypeName>(x: TypeName) -> int` в том же модуле. Компилятор требует.
- `let y = x` где x линейный — compile error. Aliasing запрещён.
- `let mut x = ...` линейного — compile error.
- Линейные нельзя класть в data position (поле обычной struct, generic param, элемент Ref).
- `drop(x)` — explicit consume, запускает destructor.
- Auto-drop в конце scope если не consumed.
- Branch divergence — нельзя забыть drop в ветке.
- Region — частный случай linear типа: автогенерируемый `drop_Region`.

**Pattern matching:**
- Match по int, bool, byte, Handle[byte], ADT.
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
- `()` — unit (пустой кортеж), значение «ничего». Эффекты (`:=`, `while`, `for`, `print`, `if` без `else`) возвращают `()`.
- `float` — IEEE 754 double (C `double`). NaN/Infinity по IEEE: `x != x` ловит NaN.
- Generics с параметрами `[T, U, ...]`.
- Параметрический полиморфизм, monomorphization.
- Запрет линейных типов в data position (поле, вариант, type-arg).
- Запрет линейных типов в позиции generic параметра (Region не пройдёт через `[T]`).
- `to_int(byte|float) -> int`, `to_byte(int) -> byte`, `to_float(int) -> float` — явные конверсии.
- `+ - * /` работают для int и float (оба операнда того же типа). `%` — только int.
- `match` на float запрещён (NaN/zero edge cases); используй `if` или bind+guard.

**Строки:**
- `Handle[byte]` — единственный тип строки. Никаких String/&str/CString/Cow.
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
- `as_ptr(a: Handle[T]) -> *T` — отдать байты Ref в libc/C-функцию.

**I/O — через io_uring:**
- Целевые ядра: Linux 5.6+. Не Windows, не macOS, не старые ядра.
- Все file/socket/timer/pipe I/O идут через `iouring.orto` ring API.
- Direct syscalls (`sys.orto`) остаются только для setup и того что в io_uring нет (process control, signals).
- Stage 1 (есть): synchronous façade — один SQE submit + один CQE wait per call. `examples/file_io_uring.orto` показывает.
- Stage 2 (есть): batched submit + multiple in-flight ops. `examples/iouring_batch.orto`.
- Stage 3 (в работе): ring-native completion-based concurrency. `await`, `await all { }`, `spawn`, `yield`, `Stream[T]`. Без `async`-раскраски, без `Future`/`Pin`. См. `STAGE3_ASYNC.md`.
  - Фаза 1 (есть): синтаксис — `await` / `await all` / `spawn` / `yield`. `async` модификатор для `extern fn`.
  - Фаза 2 (есть): типизация `Task[T]` и `Stream[T]` как builtin linear; induced linearity (Task[Region] валиден).
  - Фаза 3 (есть): `Handle[T]` где T линейный → линейный массив; cascade drop (drop_Array_T).
  - Фаза 4a (есть): async-детектор по AST.
  - Фаза 4b/5 MVP (есть): `yield`-only async `main` через io_uring nop. State-machine lowering, диспетчер, frame на стеке.
  - Фаза 4c (есть): `await` на `extern async fn` — реальный I/O через ring.
  - Фаза 4d (есть): `yield`/`await` внутри `if`/`while`/`break`/`continue`. State splits через `for(;;) switch`.
  - Фаза 4e (есть): `spawn` + slot pool (TigerBeetle стиль) + non-main async. Joinable + detached задачи. Chained awaits через dispatcher loop.
  - Фаза 4f (есть): не-int результаты `Task[T]` через long long pipeline в slot.
  - Фаза 4g (есть): auto-drop `Task[T]` → detached (worker self-frees slot, main блокируется на dispatcher).
  - Фаза 4h (есть): `spawn` из sync-контекста — sync функция может выдать `Task[T]` и async caller awaits.
  - Фаза 6 (есть): `Stream[T]` + `for x in stream` (multishot SQE). Inline-форма; bound-Stream и cancel — v2.
  - Фаза 7 (есть): `Result[T]` обёртка над `await` — Ok/Err per CQE.
  - Фаза 8 (есть): tuples (`(T1, T2, ...)`, `t.N`, `let (a, b, c) = ...`) + `await all { ... }` static + dynamic.
  - Фаза 8a (есть): fix frame-binder footgun для inline `let` в async.
  - v2 (после v1): многоядерность shared-nothing, bound Stream form через буфер/deferred-prep, `ASYNC_CANCEL` для drop Stream, match по tuple-паттернам, IOCP-бэкенд (Windows).

**Управление:**
- Всё — выражения. `if`/`match`/`let` возвращают значения.
- Dead-name tracking — use-after-move = compile error.
- Ветки `if`/`match` обязаны сходиться в одном live-set.
- Tail-position consume — bare имя в хвосте функции = move.
- `else if` цепочки без вложенных скобок.
- `if cond { ... }` без else — statement: выполняет тело ради эффекта, отбрасывает его значение, возвращает `()`.
- `while cond { body }` — циклы. `break` / `continue` внутри.
- `for i in lo..hi { ... }` — числовой диапазон (сахар над while).
- `for x in <ref> { ... }` — обход ячеек `Handle`/слайса по индексу (сахар над while, zero-cost). `for x in <stream>` — drain потока.
- `let mut x = ...; x := v;` — изменяемые биндинги. Запрещён `mut` для Region (избегаем утечек через reassign).
- Присваивание по пути: `x := v`, `a[i] := v`, `s.f := v`, `r[i].f := v`.
- Trailing `;` перед `}` отбрасывает значение выражения, блок возвращает int 0.

**Модули:**
- Один файл = один модуль, имя из имени файла (`str.orto` → `str`).
- `use foo::bar;` или `use foo::{a, b, c};` — selective import.
- Driver автоматически подгружает referenced модули из той же директории. Циклы — compile error.
- Mangling: `concat` в `str.orto` становится `str__concat`. References в `use'й`-щем модуле резолвятся прозрачно.
- Builtin names (`Handle`, `Region`, `Option`, `Some`, `None`, `byte`) и `main` не мангляются.
- Externs не мангляются (имя в C = имя в orto), дедуплицируются по имени.

---

## → Дорога вперёд

Базовая ось языка закрыта. Что осталось — это **stdlib** (это уже
orto-код, не compiler) и крупные архитектурные шаги, требующие
обсуждения с Francois.

### 1. Stdlib *(в работе)*

Сейчас в `examples/` есть прото-stdlib:
- `str.orto` — операции над `Handle[byte]`: eq, find, parse_int, concat, concat_all, split_byte, bytes_join, trim, to_lower/upper, c_str, etc.
- `io.orto` — print, println, putchar.
- `sys.orto` — `linear Fd` + syscall wrappers (open, read, write, close, socket, bind, sendto, recvfrom, etc.).
- `bin.orto` — binary read/write helpers (u16/u32 LE/BE) для netlink/network protocols.

Что нужно добавить (по приоритету из netlink анализа в `NETLINK_ANALYSIS.md`):
- **Hashmap** — линейный scan `Handle[(K, V)]` болезнен at scale. Hand-written without generics, или через monomorphization. Большая stdlib работа.
- `std::gen_arena` — generational arena для resource pools, evicting caches.
- `std::slab` — slab pool для homogeneous-size объектов.
- `std::ring` — ring buffer.
- `std::result` — convention для error handling (или сами enum'ы).

### 2. QBE-бэкенд *(M1–M5 готовы: вся RUN-регрессия проходит через QBE)*

Альтернатива связке «emit C → gcc»: **QBE** (крошечный SSA-бэкенд, ~10k строк C; amd64/arm64/riscv64). Зачем: быстрая компиляция (нет второго парса C), нормальный IR вместо C-сорс-импеданса, и **полный C ABI задаром** — вызовы C становятся нативными `call` к символу, переобъявления прототипа нет → **весь класс clash'ей растворяется** (`write`/`putchar`/`strlen` больше не конфликтуют).

**Сделано:** `lib/emit_qbe.ml` за флагом `--backend qbe` пишет `.ssa`; фронт (lexer→parser→resolve→lift→check→mono) **переиспользован целиком**; рантайм региона/gen-check/строки — `runtime_qbe.c` (линкуется), QBE-код зовёт его по C ABI.
- M1 функции/литералы/ret; M2 арифметика/let(стек-слоты)/if/while/сравнения/**short-circuit `&&`/`||`**; M3a вызовы/рекурсия/externs/числовые конверсии; M3b регионы/`Handle`/`reset`/срезы/сырые `*T`+арифметика/строки/tuples/try_at; M4 struct/enum/match(литералы/строки/or/guards/absurd) — агрегаты как указатели + натуральный layout + **value-копии (blit) при биндинге**; M5 замыкания (fat-pointer {env,code}, env в регионе, indirect call, env-адаптирующие wrappers).
- **Результат: `run_qbe.sh` → PASS=47, MISMATCH=0, TOOLFAIL=0** — вся RUN-регрессия даёт те же exit-коды, что C-бэкенд (включая дженерики, std map/set/vec/sort, замыкания, hello_putchar через C ABI без clash). C-бэкенд держим зелёным (59/59) как референс.

**Осталось:** async/io_uring узлы (TEAwait/TESpawn/TEForStream — в C-baseline они и так GCCFAIL в песочнице); глубокая оптимизация (LLVM-уровень) — позже, если бенч потребует; затем переключить дефолтный бэкенд на QBE.

### 3. C-интеграция: бесшовный импорт + безопасная обёртка *(планируется)*

Цель — **НЕ «как C++»** (он бесшовен ценой того, что *является* надмножеством C — анти-ортогональность + небезопасность). Цель — **импорт как у Zig + безопасная обёртка, которой нет ни у Zig, ни у C++**.

Догнать бесшовность (ортогонально):
- **Встроенный `use c "header.h"`** — интегрировать `tools/cimport.py` в компилятор как «модуль из C-заголовка» (аналог Zig `@cImport`). Парсинг делегируем clang JSON AST, свой парсер C НЕ пишем.
- **C-ABI указатели на функции** — передавать **capture-free** orto-`fn` в C как голый C-fn-ptr (qsort/GUI/сигналы/libuv). Падает из closure-сплита: не-захватывающая = можно как C-fn-ptr; замыкание = толстый указатель, нельзя (нужен трамплин + `void* context`). Новой концепции не требует.
- **Layout структур:** коммит на **C-совместимый layout по умолчанию** (поля в порядке объявления, натуральное выравнивание) → импортированные C-структуры работают по значению и по полям **без аннотации** `repr(C)`. Это НЕ `packed` (тот выкинут, byte-codec покрывал wire-форматы); C-layout нужен для передачи struct **по значению** в C, byte-codec'ом не обойти. Цена — отказ от reorder-оптимизации layout (мелкая).

Обойти Zig — **не импортом** (там Zig золотой стандарт, максимум паритет), а тем, чего у Zig нет: **безопасная линейная/gen-checked обёртка C-ресурсов**, бесплатно на уже существующих линейных типах. `fopen`/`fclose` → `linear struct File { h: *byte }` + `drop_File = fclose` → компилятором-гарантированный close-ровно-раз + ловля use-after-free над C-ресурсом. Zig (ручной, как C) так не может. Сырой C на входе → безопасный orto на выходе. Это и есть «бесшовно-и-безопасно», место, где ни C++, ни Zig не стоят.

---

## ◯ Открытые дизайн-вопросы

Эти не блокируют. Каждый — серьёзная архитектурная работа, требующая обсуждения.

- ~~**Closures / lambdas.**~~ **Сделано** (`examples/lambda.orto`, `closures.orto`, `closures_generic.orto`). Все function-значения — толстый указатель `{env_slot, env_offset, env_gen, code}`. Анонимные `fn(p: T) -> R { body }` без захвата → lambda-lifting в обычную функцию; с захватом — `closure(r, fn...)`, env в регионе, gen-checked. Полиморфные замыкания работают. Capture-by-reference и escape-анализ мы НЕ делали — env живёт в явном регионе, протухание ловит gen-проверка. Фича вписалась ортогонально, без магии, которой боялись.

- ~~**Рекурсивные данные (деревья/списки).**~~ **Сделано** (`examples/tree.orto`, `list.orto`). `enum Tree { Node(int, Handle[Tree]) }` или `enum List { Cons(int, Handle[List]) }` — поле за хендлом (`Handle`/`*T`) фиксированного размера разрывает цикл, узлы живут в регионе. Доступ к одной ячейке: `r[0]`.

- **Threading.** Уже expressible через linear типы — `linear struct Thread { id: int } drop_Thread = pthread_join`. Channels — `linear Sender`, `linear Receiver` с send/recv. Atomic primitives через `extern fn` (memory barriers от C). Не требует новой концепции — большая работа в stdlib + extern wrappers. См. `NETLINK_ANALYSIS.md`.

- **Variadic format `format(r, "...", a, b, c)`** — сильно болит в практике (netlink error messages, debug print). Без неё `concat_all + int_to_bytes` chains.

- **Generic Option/Result для linear types** — сейчас `Option[Fd]` запрещён (Fd linear, Option не linear). Workaround: возвращать raw int + wrap manually (`fd_wrap`). Чище — разрешить linear-aware generic instantiation: если T linear, container становится linear.

- **`format(r, "...", a, b, c)` variadic.** Текущее `concat_all(r, ref(r, [...]))` многословно. Variadic + типизированные args существенно улучшат, но variadic — серьёзная фича.

- **Nested patterns в match.** `Some(0) =>`, `Some(_) =>`. Сейчас `Some(x)` биндит x, литерал на месте не работает. Закроется guards (есть!) на 80%; nested cleaner но big refactor.

- **Guards в ADT match.** Сейчас запрещены — пользователь пишет `if` внутри arm body. Чтобы разрешить, нужен `goto`-based fallthrough в ADT switch emit.

- **Type-level alignment / sizes.** `Region[Page]` vs `Region[Default]` через тип. Сейчас alignment runtime через keyword.

- **Implicit allocator / `alloc fn`.** Обсуждалось — решено НЕ делать. Видимость аллокации (`r` параметр) — якорь философии orto.

---

### Оставшиеся НЕ-механистические решения (по убыванию глубины)

После категориального разбора структурное ядро отдумано — дальше в основном исполнение. По-настоящему «думать» (вкус/tradeoff/открытие, можно ошибиться) осталось здесь:

1. **Кросс-ядерная конкурентность — самое глубокое.** При thread-per-core по-ядерный аллокатор ⇒ владение памятью не пересекает ядро. Нерешено: как передавать большие данные между ядрами (копия / процессо-глобальный shared-аллокатор + free-back-message / seastar-стиль); как распределять соединения (`SO_REUSEPORT` per-core accept vs accept+handoff = снова пересечение границы); MPSC vs SPSC, backpressure, `select`. Send выводится структурно (нет локальных Handle/Region внутри), но точное правило — открыто. seastar-уровня.
2. **Escape-анализ** — чтобы placement на стек был молчаливой оптимизацией (и убрать `stack_region`, не теряя возможности). Реальный статический анализ: точность vs консервативность, цепляет замыкания и возврат Handle'ов. **Нельзя убирать `stack_region` до того, как анализ построен** — иначе регресс без замены.
3. **Ambient-регион / `box` / `in r`** — НЕ закрытый вкусовой звонок: неявная протяжка региона (эргономично, чуть «магия») vs явная (прозрачно, многословно). Склонились к «`Ref` в типе возврата сигналит ⇒ ок», но не зафиксировали и не построили. Прямо на законе явности.
4. **`?` для Result/Option** — ранний возврат = частично скрытый поток (виден в точке `?`). Вкусовой звонок: берём ли, в какой форме.
5. **`inout` (copy-in/copy-out)** — эргономика out-параметра без алиасинга; чистый desugar в возврат-перепривязку. Решение «добавлять ли» + как уживётся с линейными.

Механистическое (законы заданы, крутить ручку): HTTP-парсер/роутер/регион-на-запрос, ширина stdlib, доведение одноядерного async-lowering, чистка TODO-долгов.

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
