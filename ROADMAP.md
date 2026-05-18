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
- `int`, `bool`, `byte`, `struct`, `enum`, `fn(...) -> ...`.
- Generics с параметрами `[T, U, ...]`.
- Параметрический полиморфизм, monomorphization.
- Запрет линейных типов в data position (поле, вариант, type-arg).
- Запрет линейных типов в позиции generic параметра (Region не пройдёт через `[T]`).

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

### 1. Stdlib *(следующее)*

Когда понадобится:
- `std::gen_arena` — generational arena поверх `Array[Slot[T]]`. Handle tables, resource pools, evicting caches (где Region — bump-only).
- `std::slab` — slab pool для homogeneous-size объектов.
- `std::ring` — ring buffer / circular array для стримов.
- `std::str` — расширить: `bytes_copy(r, s)`, `bytes_find`, `starts_with`, `ends_with`, `split`, etc.
- `std::int` — `min`, `max`, `abs`.
- `std::option` — `unwrap_or`, и (когда будут closures) `map`, `and_then`.

Все — orto code, не compiler features. Сейчас `str.orto`, `io.orto`, `db.orto` в `examples/` — это прото-stdlib.

---

## ◯ Открытые дизайн-вопросы

Эти не блокируют. Каждый — серьёзная архитектурная работа, требующая обсуждения.

- **Closures / lambdas.** Сейчас только именованные функции. Closures открывают callbacks, higher-order patterns (`map`, `fold`, `filter`). Усложнение — capture analysis, runtime representation (fat pointer), interaction с linear типами.

- **Threading.** Region линейный = одно владение. Передача через channel-like API. Atomic gen-counter для копий handle'ов. Базовая модель не меняется, но дизайн сборки требует разговора.

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
