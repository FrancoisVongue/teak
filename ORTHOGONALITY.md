# Ортогональность orto: что есть, что не есть, и где врёт интуиция

Документ для разбора того, насколько чисто построен язык на сегодня
и где надо было бы сделать иначе. Не план действий — описание состояния.

Закон N+M из CLAUDE.md: добавление фичи должно стоить столько строк
сколько занимает сама фича, без каскада в другие места. Цель — оценить
каждую фичу по этому критерию.

---

## Базовые оси (ядро языка)

Это то, что не строится ни на чём другом и определяет всю остальную
модель:

1. **Память: `Region`** (линейный примитив) и slab-аллокатор под капотом.
2. **`Array[T]`** — копируемая ручка в Region (slot + offset + len + gen).
3. **`*T`** — сырой указатель для FFI, без проверок.
4. **`struct` / `enum`** — продукт и сумма данных.
5. **`fn`** — функции верхнего уровня с параметрами и возвратом.
6. **Generics `[T]`** — параметрический полиморфизм через monomorphization.
7. **Modules** — один файл = один модуль, mangling `mod__name`.
8. **Expressions as units** — всё возвращает значение, нет statements.
9. **Примитивные типы `int`, `bool`, `byte`**.

Каждая ось добавляет одну концепцию и одну C-конструкцию на эмиссии.
Они **минимально пересекаются между собой**: Region не знает про
struct, struct не знает про generics в смысле своего кода (только
через mono), модули не знают про типы. Хорошо.

---

## Что построено поверх ядра

Список с честной оценкой "стоимости" каждой фичи — сколько мест в
компиляторе она тронула. Чем меньше — тем ортогональнее.

### Категория А: чисто-sugar (только parser)

| Фича | Lexer | Parser | AST | Resolve | Check | Mono | Emit |
|---|---|---|---|---|---|---|---|
| `\|>` pipeline | 1 token | 30 строк | 0 | 0 | 0 | 0 | 0 |
| `for i in lo..hi` | 2 keywords | 25 строк desugar | 0 | 0 | 0 | 0 | 0 |
| `else if` цепочки | 0 | 5 строк | 0 | 0 | 0 | 0 | 0 |
| `if` без else | 0 | 3 строки | 0 | 0 | 0 | 0 | 0 |
| Trailing `;` в блоке | 0 | 5 строк | 0 | 0 | 0 | 0 | 0 |

Эти **по-настоящему ортогональны**. Удалить — снести парсер-код, и
всё. Никаких ссылок из других стадий. По закону N+M идеальные.

### Категория B: новый узел AST, но без архитектурных эффектов

| Фича | Lexer | Parser | AST | Resolve | Check | Mono | Emit |
|---|---|---|---|---|---|---|---|
| `let mut`+`:=` | 1 keyword | 10 строк | 1 bit | 1 case | ~50 строк | 1 case | 1 case |
| `while`+`break`+`continue` | 3 keywords | 15 строк | 3 узла | 3 cases | ~40 строк | 3 cases | ~30 строк |
| `return` | 1 keyword | 5 строк | 1 узел | 1 case | ~15 строк | 1 case | 1 case |
| or-patterns | 0 | 15 строк | 1 узел | 1 case | ~20 строк | 1 case | ~10 строк |
| `try_at` | 1 keyword | 8 строк | 1 узел | 1 case | ~20 строк | 1 case | ~30 строк |
| `slice` | 1 keyword | 10 строк | 1 узел | 1 case | ~30 строк | 1 case | ~30 строк |
| type aliases | 0 | 10 строк | 1 узел | ~80 строк (expansion) | 0 | 0 | 0 |

Каждая стоит **примерно как сама фича** — 50-150 строк всего на все
стадии. Это закон N+M в работе. Они дополняют существующие
механизмы (match, индексирование, expressions), не переписывают их.

### Категория C: новый примитив

| Фича | Lexer | Parser | AST | Resolve | Check | Mono | Emit |
|---|---|---|---|---|---|---|---|
| `byte` тип | 1 keyword | 3 строки | 0 (TyApp) | 0 | ~10 строк | 5 строк | ~5 строк |
| String литералы `"..."` | 1 token | 30 строк (escape) | 1 узел | 1 case | ~5 строк | 1 case | ~70 строк (static region) |
| `to_int`/`to_byte` | 2 keywords | 15 строк | 2 узла | 2 cases | ~20 строк | 2 cases | ~10 строк |
| Raw pointers `*T` | 1 token | 20 строк | 1 ty, 6 узлов | 6 cases | ~100 строк | 6 cases + 1 ty | ~100 строк |
| Modules | 1 keyword, 1 token | 30 строк | 1 узел | **новый файл, 250 строк** | 0 | 0 | 0 |

Здесь добавление шире, но **каждый примитив изолирован в своём
блоке кода**. Удалить byte — список изменений known. Удалить raw
pointers — known. Удалить модули — снести `resolve.ml`, отменить
mangling в эмиссии (которой не делается, externs не мангляются).

Модули заслуживают отдельной точки: они не трогают check/mono/emit
вообще. Весь mangling резолвится в resolve.ml до того, как программа
попадает в check. Это **очень чистая** изоляция.

---

## Скрытые сопряжения (где врёт интуиция)

Это места где фичи выглядят независимыми, но связаны.

### 1. Region и `let mut`

`let mut x = ...` запрещён для Region. Это **специальный case** в
check.ml. Если завтра добавим другой линейный тип (скажем,
`FileHandle`), нужно либо повторить этот special case, либо
обобщить. Закон N+M слегка нарушен — добавление линейного типа
теперь требует точечной правки в let-обработке.

**Правильнее было бы:** ввести функцию `is_drop_owner(ty) -> bool` и
все правила формулировать через неё. Сейчас "Region" hardcoded в
нескольких местах.

### 2. Region и аллокация Array

`array(r, ...)` явно принимает Region. То же `slice(a, lo, hi)` —
не нужен r, но возвращаемый Array неявно ссылается на slot region'а.
То же `array_data(a)` — точно так же.

Это **правильное** сопряжение (Array физически в Region), но **синтаксически
оно asymmetric**: `array` пишет r, `slice` нет. Программист должен
держать в голове "что аллоцирует, а что — нет".

**Это нельзя устранить без implicit Region context.** Мы решили его
не делать — значит живём с этим.

### 3. `if` без else + EBreak/EContinue/EReturn

EBreak имеет тип `TyMeta(fresh)` — unifies с чем угодно. Это нужно
чтобы `if cond { break } else { 5 }` имело тип int (then = meta
unifies с int = 5).

Но в `let _ = return_expr;` — meta остаётся unresolved, zonk
ломается. Я добавил **специальный hack**: если `let _ = expr` и тип
expr — meta, пришпилить к int.

Это **coupling между divergent expressions и let-семантикой**. В
"чистом" дизайне следовало бы ввести явный тип "никогда не
возвращается" (`Never` в Rust), и unification с ним работает иначе.
Сейчас meta + ad-hoc trickery.

### 4. Builtin Option как ADT

`Option[T]` встроен в check.ml как `builtin_option_decl`. Если
пользователь определит свой `Option` — error. Если убрать встроенный
Option — `try_at` сломается (он возвращает `Option[T]`).

`Some` и `None` не мангляются (они в `builtin_names` resolve.ml). Это
**специальная привилегия имён**, как у `main`.

**Чище было бы:** Option определяется в стандартной библиотеке как
обычный enum, и интрисик `try_at` ссылается на него через явный путь.
Тогда built-in список пустой, всё через обычные модули.

### 5. `main` и externs не мангляются

Оба — special cases в `mangle_for_module`. Если будут другие имена
со специальной судьбой (скажем, weak symbols), потребует расширения.

**Это вынужденное:** main должен быть `main` для C-линкера, putchar
должен быть `putchar`. Других вариантов на этом уровне нет. Но это
"законное" special-casing — оно вынуждено наружным миром, не
дизайном языка.

### 6. Match — только на ADT, не на int/byte/Array[byte]

В address_book.orto:
```orto
if bytes_eq(cmd, "count")       { ... }
else if bytes_eq(cmd, "find")   { ... }
else if bytes_eq(cmd, "search") { ... }
```

Хотелось бы:
```orto
match cmd {
    "count"  => ...,
    "find"   => ...,
    "search" => ...,
    _        => ...,
}
```

Не работает, потому что match scrutinee должен быть ADT. **Это
архитектурное ограничение**, не баг. Расширение требует:
- pattern-match на int (со встроенным `==`)
- pattern-match на bytes (со встроенным `bytes_eq`)
- exhaustivity check для не-ADT (только default)
- emit как if-else chain

Это **новая ось** (literal patterns), которую мы не добавили. И тут
видно: текущий `match` строится на ADT через monomorphization. Эта
основа не покрывает не-ADT, и нужна **отдельная** реализация.

Сейчас match **не до конца ортогонален** с типами — он привилегирует
ADT. В будущем, если добавим literal patterns, это будет три
параллельных пути (ADT match, int match, bytes match) — и тогда уже
точно стоит абстрагировать.

### 7. Move analysis — мёртвый код

`check_moves_expr` в check.ml — около 200 строк. Когда Region был
линейным, он ловил use-after-move. Сейчас Region копируемый, все
типы копируемые, **move analysis ничего полезного не делает**.

Это **технический долг** от старой модели. Он не вредит, но
загромождает check.ml. Если убрать — компилятор ужмётся на ~200
строк OCaml и логика станет проще для понимания.

Это **нарушение N+M в обратную сторону**: фича "линейный Region"
ушла, но её compiler-инфраструктура осталась.

### 8. `array(r, [...])` требует Region даже для read-only пользования

Inline литерал массива `[1, 2, 3]` нельзя написать как expression —
только `array(r, [1, 2, 3])`. Это значит:

```orto
concat_all(r, array(r, [a, b, c]))
```

— два r. Хотелось бы `concat_all(r, [a, b, c])`, где `[a, b, c]`
аллоцируется в каком-то temp месте.

**Но это противоречит "explicit allocation"** — все аллокации видны.
Так что осознанная цена. Не баг.

---

## Что построено на чём (диаграмма зависимостей)

```
                  +-- Region (linear primitive)
                  |
   Array[T] ------+-- slab allocator + gen counters
        |
        +-- slice (sub-handle)
        +-- try_at (Option[T] view)
        +-- array_data (-> *T)
        +-- string literals (static region slot 0)
                                  |
        +-- byte (primitive) -----+
                  |               |
                  +-- to_int / to_byte
                  
                  
   Generics [T] --+-- monomorphization
                  |
                  +-- check_instantiation (rejects linear-in-T)
                  

   Modules -------+-- use foo::bar
                  +-- resolve.ml (mangling pass)
                  +-- type aliases (resolved-away)
                  +-- selective imports


   Expressions ---+-- if/else (block expr)
                  +-- match (ADT only)
                  +-- let / let mut
                  +-- while + break/continue
                  +-- return
                  +-- for (sugar -> while+let mut)
                  +-- |> (sugar -> call)


   FFI escape ----+-- *T raw pointers
                  +-- c_alloc / c_free
                  +-- null_ptr / is_null
                  +-- array_data (Array bytes -> *T)
                  +-- extern fn (C symbols)
```

Главные **узлы** — Region, Array[T], expressions. Большинство фич
висит на них как листья.

---

## Оценка по фичам (sortировано худшее→лучшее)

### Худшее: где coupling сильный

1. **`Option` встроен** — special-cased в check, в emit, в resolve.
   Чистка: definite-bonus после stdlib появится возможность
   определить Option в стандартной библиотеке.

2. **Move analysis** — оставлен от прошлой модели, не делает работы.
   Чистка: удалить ~200 строк, упростить infer.

3. **`Region`-specific правила в let** — `mut` запрет, alias запрет,
   auto_drop логика. Это пять разных мест. Чистка: ввести
   `drop_owner(ty) -> bool` и формулировать через неё.

### Среднее: вынужденное coupling

4. **String литералы и Array[byte]** — литералы знают про slot 0,
   gen=1, статическую таблицу. Это вынужденная связь "literal vs
   handle" но реализована чисто.

5. **Modules и externs** — `extern` имена не мангляются. Вынуждено
   тем что они уходят в C linker. Чистка: explicit `c_name` поле в
   extern_decl, формально отделить orto-namespace от C-namespace.

6. **`main` не манглируется** — то же что и externs.

7. **TyMeta для break/continue/return + ad-hoc fixup в let** —
   технический долг. Чистка: ввести `TyNever`.

### Лучшее: чисто ортогональные

8. **Pipeline `|>`** — sugar в парсере, 30 строк, всё.

9. **`for` цикл** — sugar в парсере, 25 строк, всё.

10. **Type aliases** — отдельный pass в resolve, 80 строк, не
    трогает check/mono/emit.

11. **try_at** — самостоятельный intrinsic, нет coupling.

12. **slice** — то же.

13. **Generics + monomorphization** — большая фича, но изолирована
    в mono.ml, не утекает.

---

## Если бы делать заново — что иначе

Не "переделывать сейчас", а как оценочное замечание для будущих
больших фич.

1. **Линейность как property, не как тип.** Region "линейный" сейчас
   потому что hardcoded по имени. Лучше: каждый тип знает свою
   "linearity flag", и все правила формулируются над флагом.

2. **Builtin типы как стандартная библиотека.** Option, possibly
   Result в будущем — определяются в `std::core.orto`, компилятор
   только знает что они есть. Сейчас Option захардкоден в check.ml.

3. **TyNever для divergent expressions.** Унифицируется в любую
   сторону. Убирает hack для `let _ = return ...`.

4. **Pattern matching не привязан к ADT.** Расширить на int/byte/
   bytes — это новая ось, но если не сделать сейчас, появится "три
   разных матча" в будущем.

5. **Externs с отдельным `c_name`.** Формально отделить от orto-имени.

6. **Drop discipline через trait/interface.** Сейчас "drop при выходе
   из scope" hardcoded для Region. Если будут другие resource-owning
   типы (FileHandle, Socket, GPU buffer) — нужна общая модель.

7. **Удалить move analysis.** Не делает работы при copyable Region.

---

## Итог

**Что получилось хорошо:**

- Модули — образцовая изоляция, отдельный pass.
- Sugar фичи (`|>`, `for`, `else if`, trailing `;`) — чисто парсер.
- Generics — изолированы в mono.
- Raw pointers — параллельный путь, не пересекается с Region.
- Type aliases — отдельный pass, никаких лазеек в другие стадии.

**Где врёт интуиция:**

- "Region копируемый" звучит как обычный тип, но имеет 5+ специальных
  правил в let.
- "Option just an enum" — на деле special-cased.
- `match` ощущается универсальным, на деле — только ADT.
- Move analysis — выглядит как live machinery, на деле dead code.

**Что добавили без сожаления:**

Все категории A и B (~12 фич) добавлены практически даром. Цена
была близка к их собственному размеру.

**Где будет больно при росте:**

- Добавление любого нового линейного/resource-owning типа.
- Добавление literal-patterns в match.
- Добавление еще одного интегрального типа (u16, u32, i64) — каждый
  будет повторять путь byte.

Эти три места — кандидаты на абстракцию, когда боль появится в практике.
