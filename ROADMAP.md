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

**Управление:**
- Всё — выражения. `if`/`match`/`let` возвращают значения.
- Dead-name tracking — use-after-move = compile error.
- Ветки `if`/`match` обязаны сходиться в одном live-set.
- Tail-position consume — bare имя в хвосте функции = move.

---

## → Дорога вперёд

### 1. Raw pointers `*T` *(следующее — для C interop)*

Escape hatch для интеграции с C-библиотеками:
- `c_alloc(N)` → `*T`, выделяет через `malloc`.
- `c_free(p)` — программист сам.
- `*p` разыменование, `p[i]` индексирование.
- Никаких проверок, никакого gen.

Это **параллельный** путь к Region. Используется когда C-API требует владения указателем (или возвращает указатель который должен освободиться через `free`). Программист сознательно сходит с safe пути.

Тип явно `Unsafe` или `Raw` чтобы было видно в сигнатурах: `fn glfw_init() -> *Window` бьёт сразу — это C boundary.

### 2. Модули

`module foo; use foo::bar;` — разделение программ на файлы. Открывает дорогу к stdlib.

Минимально:
- Один файл = один модуль.
- `use path::item;` импортирует.
- Не плодим cyclic-зависимости.
- Маньглинг имён включает имя модуля.

### 3. Stdlib

Когда модули появятся:
- `std::gen_arena` — generational arena поверх `Array[Slot[T]]`. Game-style handle tables, resource pools.
- `std::slab` — slab pool для homogeneous-size объектов.
- `std::ring` — ring buffer / circular array для стримов.
- `std::str` — операции над строками. Канонические: `bytes_copy(r, s)` (копирует Array[byte] в другой Region), `bytes_concat(r, [s...])` (склейка), `bytes_eq`, `bytes_find`, `parse_int`, `int_to_bytes(r, n)`, `starts_with`, `split`, etc.

Все — orto code, не compiler features.

### 4. `try_at(a, i)` для defensive чтения

Дефолтный `a[i]` остаётся abort-on-dangling (быстрый, для обычных случаев где регион гарантированно жив). Добавим safe-вариант:
- `try_at(a, i)` → `Option[T]`.
- `try_set(a, i, v)` → `Option[int]`.

Программист выбирает по контексту. Hot loop — `a[i]`. Defensive код где Array мог пережить регион — `try_at`.

---

## ◯ Открытые дизайн-вопросы

Эти не блокируют, но рано или поздно вылезут:

- **Mutable bindings.** Сейчас `let x = ...` immutable. `let mut x = ...; x := ...` ввести? Влияет на:
  - Циклы (нужны mut counter).
  - Builder patterns.
  - Performance-sensitive код.
  - Возможно нужны вместе с loops.

- **Loops (`while`, `for`).** Сейчас только рекурсия. Хвостовая рекурсия в C от gcc оптимизируется в jump (TCO), но не везде. Loops были бы прямолинейнее.

- **Closures / lambdas.** Сейчас только именованные функции верхнего уровня. Closures открывают callbacks, higher-order patterns. Усложнение — capture analysis.

- **Pipeline `|>`.** Sugar для chains: `x |> f |> g` ≡ `g(f(x))`. Удобно для DSL-стиля. Не сложно реализовать.

- **Pattern or-arms.** `match x { 1 | 2 | 3 => ... }`. Token `TPipe` зарезервирован под это.

- **Type aliases.** `type Bytes = Array[byte]`. `type` keyword уже зарезервирован. ~20 строк.

- **Error handling beyond Option/Result.** Effects? Try/catch? Скорее всего — нет, остаёмся на ADT.

- **Threading.** Region линейный = одно владение. Передача через channel-like API. Atomic gen-counter для Ref. Базовая модель не меняется, но дизайн сборки требует обсуждения.

- **Type-level alignment / sizes.** `Region[Page]` vs `Region[Default]` через тип. Сейчас alignment runtime через keyword. Если ошибки с alignment станут проблемой — добавим.

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
