# Ортогональность orto

Архив дизайн-решений. Каждая строка таблицы — место где мог быть
каталог правил, и явный выбор почему он там не вырос.

## Ядро

Не выводится из остального:

| Категория | Primitive'ы |
|---|---|
| Данные | `int`/`bool`/`byte`/`float` литералы, `struct`, `enum`, `fn` type, generics `[T]` |
| Вычисление | `let` / `if` / `match` / `while` / call / binop / unop |
| Память | `Region`, `Array[T]`, `linear struct/enum`, `drop_<T>`, `*T` (FFI escape) |
| Конкурентность | `await`, `spawn`, `Task[T]`, `Stream[T]`, `Result[T]` |
| FFI | `extern fn` |
| Модули | `use foo::{bar};` — манглинг в `resolve.ml` |

## Sugar (parser/check переписывает в ядро)

| Sugar | Раскрывается в |
|---|---|
| `yield` | `await orto_nop()` |
| `for i in lo..hi { … }` | `let _hi=hi; let mut i=lo; while i<_hi { …; i := i+1 }` |
| `x \|> f(a)` | `f(x, a)` |
| `let (a, b) = t` | `let _t = t; let a = _t.0; let b = _t.1` |
| `"abc"` | `Array[byte]` handle в slot 0 |
| `0xFF` / `0b1010` | `EInt` |
| trailing `;` перед `}` | `let _ = body; 0` |
| pattern guards `pat if c => body` | match arm + проверка |

## Дизайн-решения (закрыто)

| Что | Решение |
|---|---|
| `Result` / `Option` как builtin ADT | Оставлено — третий такой тип реально не нужен; Either пользователь объявит сам |
| `Task[T]` и `Stream[T]` параллельно | Оставлено — разная семантика завершения (один CQE vs многие) |
| `await all { … }` static + dynamic | Оставлено — гетерогенный fan-out требует tuples, гомогенный — массив. Парсер дизамбигирует по `{` |
| `yield` как keyword | ✅ Pure sugar над `await orto_nop()` (фаза 11) |
| `t.0` numeric field | Оставлено — стандартная нотация (Rust/Swift/Scala) |
| `extern fn` модификаторы `async`/`stream` | ✅ Убраны — return type определяет calling convention (фаза 9) |
| Три drop path (Region runtime / user / induced) | Оставлено — разные по природе; vtable стоила бы dispatch |
| `Task[T]` для T > 8 байт | ✅ slot.result `uint8_t[16]`, memcpy uniform (фаза 10) |
| `for i in lo..hi` vs `for x in stream` | Оставлено — `..` локально дизамбигирует |
| `all` контекстный keyword после `await` | Оставлено — строго одна позиция AST |
| `fr->` префикс в emit | Оставлено — implementation detail, не language surface |
| `spawn` только на ECall | Оставлено — семантическая необходимость |
| `Region` спец-правила в `let mut` | Оставлено — обобщено через `is_linear_ty(t)`, не hardcoded имя |
| `main` и externs не мангляются | Оставлено — вынужденно C-линкером |
| Move analysis (~200 строк) | Удалено при унификации Region/linear |
| `match` только на ADT | Расширено — scrutinee_kind dispatch покрывает int/byte/bool/bytes |

## Конфигурация рантайма (CLI флаги, не часть языка)

| Флаг | Что | Default |
|---|---|---|
| `--slots N` | размер slot pool на поток | 1024 |
| `--cores N` | количество pthread-воркеров (shared-nothing) | 1 |
| `--ring-entries N` | размер io_uring SQ ring | 64 |

`orto main.orto --cores 4 --slots 8192 --ring-entries 256` — 4 потока, 8192 слота, 256 SQE на ring каждый. Glue компилируется с тем же `-DORTO_CORES=N`. Auto-flush в emit срабатывает когда ring почти полон, так что `--ring-entries` в основном можно не трогать.

## Что осталось как debt v2

1. **Bound Stream form** — `let s = stream(); for x in s {}`. Требует CQE-буфер или deferred prep.
2. **`ASYNC_CANCEL` в `drop_Stream`** — пока DETACHED-fallback.
3. **Match по tuple-паттернам** — есть destructuring let, но не match.
4. **Dynamic slot pool** — slab list для роста без бэлк-аллокации.
5. **Cross-core communication** через `IORING_OP_MSG_RING`.
6. **Windows / IOCP backend**.

Открытых дизайн-вопросов из ядра — нет.

## Безопасность памяти: что закрыто, что осталось

| Класс багов | Закрыто? | Как |
|---|---|---|
| Use-after-free | ✅ | Region drop при выходе scope; gen counter (64-bit) на handle'ах |
| Double-free | ✅ | Linear `drop` в типе; повторный drop = compile error |
| Buffer overflow на Array | ✅ | bounds check на каждое `a[i]` (abort при выходе) |
| Buffer overflow на `*T` | ⚠️ | FFI escape, ответственность пользователя |
| Null deref | ✅ | нет null в языке; `Option[T]` явный |
| Uninit read | ✅ | компилятор требует init |
| Data race | ✅ | shared-nothing per core; нет shared mut между ядрами |
| Memory leak | ✅ | Region drop, linear drop — компилятор требует |
| Dangling stack ref | ✅ | нет `&local` оператора синтаксически |
| Gen counter wrap | ✅ | 64-bit, не достижимо за разумное время |
| Integer overflow | ⚠️ | wrap (как C); опционально `--check-overflow` в будущем |
| Stack overflow | ⚠️ | cc флаг (`cc -fstack-check`), вне нашего компилятора |
| FFI escape (`*T`, `extern fn`) | ⛔ | by design — наша единственная дверь в C |

**Итог**: ~99% memory safety. Остаток — FFI (by design) и cc-уровневые проверки (`-fstack-check`, `-fsanitize=undefined`).

**Рекомендуемые `cc` флаги для production-сборки**:
```
cc -O2 -fstack-check -fno-strict-aliasing -D_FORTIFY_SOURCE=2 out.c -luring
```
- `-fstack-check`: catch stack overflow до SIGSEGV.
- `-fno-strict-aliasing`: наш emit использует memcpy для cross-type blob; strict aliasing мог бы их optimise out.
- `-D_FORTIFY_SOURCE=2`: glibc-level bounds check на string/mem functions.

В чём это отличие от Rust:
- Rust 99% memory safety **внутри safe** + 1% `unsafe` в каждой core lib.
- orto 99% memory safety **везде** + `*T`/`extern fn` явный FFI escape.

## Permanent decisions — не делаем никогда

Это не «может быть в v2». Это **никогда**, потому что эти фичи **не вписываются в нашу модель явности и Region-based памяти**.

| Что | Почему не делаем |
|---|---|
| **Closures с capture by reference** (`\|x\| { use(captured_var) }`) | Capture скрывает состояние, эквивалентно OOP с `this`. Captured переменные либо с stack (dangling после возврата функции), либо требуют hoisting на heap (escape analysis = магия за спиной). Альтернатива: явная struct + function pointer, на 2 строки больше, всё видно. |
| **`&local_var` оператор** (ссылка на стек-слот) | Создаёт dangling pointer если ссылка переживёт scope. C делает молча, Rust ловит через lifetimes (расползающаяся машинерия), GC языки прячут escape analysis'ом (магия + GC). У нас нет оператора → проблема не возникает. Хочешь долгоживущую ссылку — клади в `Region`, получай gen-проверяемый handle. |
| **Lambdas / anonymous functions** | Тот же случай — захватывают окружение неявно. Лямбда без capture эквивалентна named top-level fn, но без преимуществ читаемости. |
| **Method syntax `x.method()`** | Сахар поверх `method(x)`. Открывает дверь для `impl` блоков, traits, virtual dispatch — каскад OOP-машинерии. Отказ один раз — закрывает каскад. |
| **Operator overloading** | `a + b` должно делать одно. Перегрузка = invisible different behavior по типу аргумента. |
| **Implicit conversions кроме int→float** | Та же причина. |
| **Exceptions** | `Result[T]` достаточно. Исключения — control flow за пределами сигнатуры. |
| **GC** | `Region` + linear types достаточно. GC = неявное освобождение, GC паузы, write barriers, write barriers overhead. |
| **Type classes / traits / interfaces** | Каталог правил полиморфизма. Параметрический полиморфизм через generics — достаточно. |
| **Inheritance** | Тот же случай. |
| **Macros / template metaprogramming** | Сильно осложняет инструменты (LSP, рефакторинг). У нас sugar в parser хватает. |
| **Variadic args / default args / named args** | Каждая — отдельное правило с edge case'ами. Builder pattern или явный массив дают то же без магии. |
| **Reflection** | Нет use case'а который не закрыт codegen'ом или явной структурой. |

**Главный принцип каждой строки**: эти фичи **скрывают что-то** что должно быть видимо в коде — captured state, lifetime, type-dispatched behavior, control flow. У нас всё видно. Это не лимит — это **defining choice**.
