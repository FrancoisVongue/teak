# Stage 3: async I/O в orto — варианты для обсуждения

Stage 1 (sync façade) и Stage 2 (manual batched submit/wait) показали
что io_uring API работоспособен через stdlib. Stage 3 — это **языковая
поддержка async-style кода**, который под капотом эксплуатирует
io_uring батчинг и параллелизм.

Это **архитектурный** шаг. Документ не выбирает один путь — он
формулирует выбор и его последствия. Решение за тобой.

---

## Проблема

Сейчас в `iouring_batch.orto` чтобы запустить 3 параллельные
операции:

```orto
submit_read(ring, fd1, buf1, 0, 101);
submit_read(ring, fd2, buf2, 0, 102);
submit_read(ring, fd3, buf3, 0, 103);
flush(ring);
while completed < 3 {
    let c = wait_one(ring);
    ...
}
```

Программист видит SQE/CQE концепции, тэгает completion по id, сам
бойлерплейтит. Прямая инструкция компьютеру.

Хотим:

```orto
let (a, b, c) = await_all(
    read_file(ring, "/tmp/a"),
    read_file(ring, "/tmp/b"),
    read_file(ring, "/tmp/c"),
);
```

Или ещё проще:

```orto
async fn read_file(ring: Ring, path: Array[byte]) -> Array[byte] { ... }

let bytes = await read_file(ring, "/etc/hostname");
```

Программист пишет линейный код. Compiler/runtime управляет
parallelism. io_uring под капотом submit'ит параллельно.

---

## Пять путей (от наименее инвазивного к наиболее)

### Путь 1. Pure stdlib — без language changes (Go-libuv стиль)

Stage 2 уже это есть. Дальше — лучшие helpers, conventions, может
быть `for_each_completion(ring) { ... }`.

- **Pro**: ноль изменений компилятора.
- **Con**: программист пишет state machine руками. Composing two
  parallel ops в одну операцию требует boilerplate.
- **Linear types через await**: программист сам moves into frames.

В современных языках только C/Zig/Rust-без-tokio пишут так. Не
эргономично at scale.

### Путь 2. Stackful coroutines (Go-style green threads)

`spawn(f)` создаёт coroutine на своём стеке. `yield_on_io()` отдаёт
управление scheduler-у. Scheduler выбирает другой coroutine.

```orto
linear struct Task { handle: *byte }
fn drop_Task(t: Task) -> int { task_wait(t.handle); 0 }

fn spawn(f: fn() -> int) -> Task { ... }

fn read_file(ring: Ring, path: Array[byte]) -> Array[byte] {
    let fd = await uring_open(ring, path, ...);
    // ...
}
```

`await` обращается к scheduler-у; для io_uring — submit + park
coroutine; CQE wakes it.

- **Pro**: код выглядит как sync. Любая функция может await. Нет
  function coloring (нет split async/non-async).
- **Pro**: эргономично, привычно для пользователей Go.
- **Con**: каждая coroutine имеет свой стек (минимум 4-8 KB).
  Growth с many tasks. 10k coroutines = 40-80 MB.
- **Con**: implementation — scheduler в runtime, stack switching
  (ucontext или custom asm на x86-64).
- **Con**: stack growth — segmented stacks или contiguous reallocs.
  Сложно с linear типами на стеке.
- **Linear types**: живут в coroutine stack. Move across yield —
  работает естественно. Pin issue — value на стеке не должен
  перемещаться (linear types есть указатели в Region slab — не
  переезжают).

**Языки**: Go, Lua coroutines, Ruby fibers, Elixir/Erlang processes.

Размер работы в orto: scheduler ~500 строк C + stack switching
~100 строк ASM + `spawn`/`await` language primitives ~150 строк
compiler. Total ~750 строк.

### Путь 3. Stackless coroutines / state machines (Rust-style)

`async fn name(...) -> T` — компилятор раскрывает в struct + step
function. `await` это suspension point — функция возвращает
`Future` сейчас, продолжает позже.

```orto
async fn read_file(ring: Ring, path: Array[byte]) -> Array[byte] {
    let fd = await uring_open(ring, path, ...);
    let buf = array(...);
    let n = await uring_read(ring, fd, buf, 0);
    await uring_close(ring, fd);
    slice(buf, 0, n)
}
```

Compiler видит:
1. Frame struct с локальными между await:
   ```c
   struct read_file_frame {
       int state;
       Ring ring;
       Array_byte path;
       int fd;
       Array_byte buf;
       int n;
   };
   ```
2. `step` function — state machine, переключается по `state`.
3. Each await — set state, submit io_uring, return frame to runtime.
4. Runtime polls CQEs, resumes по completion.

- **Pro**: zero-cost runtime — нет stacks per task. Frame size
  known at compile time. Прямой mapping на io_uring SQE/CQE.
- **Pro**: каждая Future — linear handle, single owner, drop = cancel.
  Натуральный fit с нашими linear types.
- **Con**: **function coloring**. `async fn` отличается от `fn`.
  Non-async не могут await. Распространяется по call graph.
- **Con**: compiler сложность — transformation в state machine
  значительная. Tricky для линейных типов которые crossing await.
- **Con**: nested awaits в loops — frame size может быть большим.

**Языки**: Rust, JavaScript, C#, Python, Zig (был, removed in 0.11).

Размер в orto: state machine transformation ~400 строк compiler +
runtime scheduler ~300 строк C + Future type ~50 строк stdlib +
extension к check для async tracking ~150 строк. Total ~900 строк.

### Путь 4. Algebraic effects / continuations

`perform Read(fd, buf)` — это effect. Handler ловит и resumes.

```orto
handle Read with (fd, buf) {
    // ...submit to ring, wait, resume(result)...
} in {
    let n = perform Read(fd, buf);
}
```

- **Pro**: ортогональнее всего. Эффекты композируются. Нет coloring.
- **Pro**: один механизм для async, error handling, generators.
- **Con**: **серьёзный** language feature. First-class continuations.
  Нужны delimited control operators.
- **Con**: runtime cost — каждый perform allocates frame для
  resumption.
- **Con**: пользователи менее знакомы — это OCaml 5, Koka, Effekt.

**Языки**: OCaml 5, Koka, Effekt.

Размер: огромный. ~2000+ строк compiler + runtime.

### Путь 5. Hybrid — explicit Task without `async` syntax

`linear struct Task[T]` — opaque coroutine handle. Spawned с
function pointer + initial args. No state machine generation;
runtime provides cooperative scheduling.

```orto
linear struct Task { handle: *byte }
fn drop_Task(t: Task) -> int { ... }

fn spawn(f: fn(Ring) -> Array[byte], ring: Ring) -> Task { ... }
fn join(t: Task) -> Array[byte] { ... }

let t1 = spawn(read_a, ring);
let t2 = spawn(read_b, ring);
let a = join(t1);
let b = join(t2);
```

Под капотом — stackful coroutines, но без `await` syntax.
Программист передаёт function pointer (для функций без closures).

- **Pro**: меньше compiler change — нет state machine generation,
  нет `async` keyword.
- **Pro**: linear Task — handles natural fit.
- **Con**: function pointers только — closure-less. Капчуринг state
  через struct (программист пишет вручную).
- **Con**: всё ещё нужен scheduler + stack switching.

Размер: ~600 строк (без compiler transform).

---

## Сравнение

| Аспект | 1. Stdlib | 2. Stackful | 3. Stackless | 4. Effects | 5. Hybrid |
|---|---|---|---|---|---|
| Эргономика | Низкая | Высокая | Высокая | Очень высокая | Средняя |
| Function coloring | Нет | Нет | **Да** | Нет | Частично |
| Memory per task | 0 | 4-8 KB stack | Frame size | Frame per perform | 4-8 KB stack |
| Linear через suspend | Ручной | Авто (stack) | Compiler сохраняет в frame | Через frame | Ручной |
| Compiler сложность | 0 | ~150 строк | ~550 строк | ~1500+ строк | ~50 строк |
| Runtime сложность | 0 | ~600 строк C | ~300 строк C | ~500 строк C | ~550 строк C |
| Знакомость пользователю | Низкая | Высокая (Go) | Высокая (Rust/JS) | Низкая | Средняя |
| Cancellation | Ручной | Через scheduler | Drop Future | Через handler | Ручной |
| Тысячи tasks | Дёшево | Дорого (memory) | Дёшево | Зависит | Дорого |

---

## Что меняется в orto при каждом

### Linear types через suspend

Самый тонкий момент. Линейный value на стеке между двух await — что
с ним?

- **Stackful**: live на coroutine stack. Pin natural если slab-based
  resources (Region, Fd) — указатели не меняются.
- **Stackless**: компилятор спиллит в Future frame. Linear move в
  field, restore из field на resume. Compiler tracks ownership через
  suspend points.
- **Effects**: похоже на stackless — frame stores linear.

В **stackless** случае compiler должен ввести правило: "linear value
live across await — moves в frame, restoring on resume". Это
расширение move analysis.

### Function coloring (path 3)

`fn` нельзя вызывать `async fn` потому что `async fn` не
синхронный return. Это распространяется через call graph — любая
функция использующая I/O становится async.

Mitigation: автоматический lift. Если функция `f` вызывает только
`fn`, она `fn`. Если хотя бы одна `async fn` — `f` становится `async fn`
автоматически. Но это требует whole-program analysis или
function-level inference (не локально).

В Rust coloring явный и пользователь раздражается. В Zig был
неявный, но потом удалили.

### Region и await

Region — slab handle. Может жить через suspend без issues
(handle stable). Linear rule auto_drop в creator scope = creator
scope продолжается after await. Семантически чисто.

### Drop при await cancel

Future cancelled до завершения — нужно drop'нуть все linear values
в frame. Stackless подход: cancellation function знает frame layout,
drops fields. Compiler genererates this.

---

## Сравнение с известными системами

**Go**: stackful, scheduler в runtime. Каждая goroutine ≈ 4 KB. 10k
goroutines = 40 MB. Pragmatic but memory-hungry.

**Rust + tokio**: stackless. `async fn` + Pin + lifetime gymnastics.
Compose works perfectly. Coloring — известный pain point.

**JavaScript + Node**: stackless single-threaded event loop. Простой.
Function coloring явный.

**C# / Python**: stackless. Похоже на JS.

**Zig**: попробовал stackless, удалили в 0.11. Сейчас просто
explicit `async`-libraries.

**Lua coroutines / Ruby fibers**: stackful, легковесные. Но один-в-один
с ОС-потоком (cooperative).

**Erlang/BEAM**: stackful + own scheduler + immutable data + actor
model. Очень специфичный — preempt safe потому что нет shared state.

---

## Моя рекомендация (НЕ окончательная, обсуждение)

**Путь 2 (Stackful) лучше всего матчится с философией orto:**

1. **No function coloring** — это `catalog of rules`. Любая функция
   I/O или CPU — одинаковые. Орт.
2. **Linear types через stack** — натурально. Не требует compiler
   spill into frame.
3. **Эргономика** — sync-style writing, async behavior. Низкий barrier.
4. **Compiler сложность мала** — `spawn`, `await`, `yield` это
   stdlib primitives + минимальная language поддержка.
5. **Известность Go-стиля** — пользователи понимают.

Минусы:
- Memory per task — но для типичных workloads (1000 in-flight ops
  максимум) 4 KB * 1000 = 4 MB. Acceptable.
- Stack switching — есть `ucontext_t` в glibc, можно использовать
  для начала. Custom asm позже для perf.

**Путь 3 (Stackless / state machines):**

Лучшая perf и memory characteristics. Но **function coloring** —
это catalog of rules. Любая функция должна быть помечена `async`
если когда-либо вызывает I/O. Это distorts code organization
вокруг I/O — что противоречит orto принципу "никаких ad-hoc
distinctions".

Также — compiler transformation в state machine значительно
усложнит emit.

**Путь 4 (Effects):**

Орто-философски лучше всего. Но размер работы — нереалистичен сейчас.
Потенциальная цель на 2-3 года когда базовый язык стабилен.

**Путь 1 (только stdlib):**

Уже есть. Не закрывает эргономику.

**Путь 5 (Hybrid):**

Компромисс stackful без `async`. OK как proof of concept но
теряет эргономику линейного кода.

---

## Open questions для тебя

1. **Function coloring приемлемо?** Если да — Stackless рассматриваем.
   Если нет — Stackful.

2. **Memory per task — критично?** Если 10k+ concurrent I/O ops
   — stackful дорог. Если 100-1000 — пофиг.

3. **Cancellation через `drop(task)` или через explicit `cancel()`?**
   Linear style → drop.

4. **Кто owns Region через task boundaries?** Spawn task с Region
   borrow → parent owns. Spawn задача создает свой Region → task
   owns, drops at completion. Обе варианты.

5. **Когда нужно?** Если Piano (твой проект) сейчас не требует —
   можно отложить и продолжать сейчас другие фичи.

Скажи, обсудим конкретно. Не пишу код Stage 3 до решения по
направлению.
