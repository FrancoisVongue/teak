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

`orto main.orto --cores 4 --slots 8192` — 4 потока, по 8192 слота на каждый. Glue компилируется с тем же `-DORTO_CORES=N`.

## Что осталось как debt v2

1. **Bound Stream form** — `let s = stream(); for x in s {}`. Требует CQE-буфер или deferred prep.
2. **`ASYNC_CANCEL` в `drop_Stream`** — пока DETACHED-fallback.
3. **Match по tuple-паттернам** — есть destructuring let, но не match.
4. **Dynamic slot pool** — slab list для роста без бэлк-аллокации.
5. **Cross-core communication** через `IORING_OP_MSG_RING`.
6. **Windows / IOCP backend**.

Открытых дизайн-вопросов из ядра — нет.
