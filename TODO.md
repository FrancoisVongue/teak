# orto compiler — technical debt

Долги, обнаруженные по ходу разработки. Каждый — кандидат на чистку,
но не блокер.

---

## Открытые

### 1. Неиспользуемые spread temp-биндинги
**Файл:** lib/check.ml (генерация TELet для spread base'ов)
**Суть:** когда после `..base` все поля переопределяются явно, временная
переменная для base'а становится мёртвой. GCC не варнит (struct copy),
но это лишняя работа.

### 2. ctor_map использует имя конструктора как ключ — коллизия после mono
**Файл:** lib/emit.ml, build_ctor_map
**Суть:** после мономорфизации Option_int и Option_bool оба имеют ctor
"Some". `Hashtbl.add` ставит обе записи, `Hashtbl.find` возвращает
случайную (последнюю). Для tag это OK (tag всегда тот же для одного
ctor name), но arg_tys могут оказаться от не-той инстанции.
Фикс: ключ в ctor_map должен включать имя owner'а mangled, не
только ctor name. ~20 строк.

### 3. `drop_fn_name_for` через string-manipulation
**Файл:** lib/check.ml
**Суть:** convention-by-name `mod__name` → `mod__drop_name` через
поиск первого `__`. Fragile если кто-то использует `__` в имени.
Чище: хранить base_name отдельным полем в decl. ~30 строк рефакторинга.

### 4. `_drop_N` имя протекает в C-output
`let _ = linear_value` → переименовывается в `_drop_N` и попадает в C
как имя переменной. Программист видит. Не баг, но косметика. Можно
ввести специальный AST-узел `TEDiscard`.

### 5. `linear` поле дублируется (`is_linear` для type_decl, `rec_is_linear` для record_decl)
**Файл:** lib/ast.ml
**Суть:** Унаследовано от исторического разделения `struct` vs `enum`.
Один флаг с разными именами в двух типах данных. Если унифицировать
record_decl/type_decl — упростится.

---

## Закрыто

- ~~Дублирование validate_ty / validate_ty_for_ascription~~ — унифицировано.
- ~~Имя ty_contains_own устарело~~ — переименовано в ty_contains_linear.
- ~~Параллельные Array/Buf таблицы в emit~~ — объединены.
- ~~Own/Ref/take/unwrap/look/legacy ref/deref/:=/??/panic~~ — удалено.
- ~~Cascade destructors~~ — невозможны по построению (linear только в top-level position).
- ~~Strings~~ — `Array[byte]` + статический region для литералов + `slice` + `to_int`/`to_byte`.
- ~~Raw pointers `*T`~~ — `c_alloc`/`c_free`/`*p`/`null_ptr`/`is_null`/`array_data` для FFI.
- ~~Модули~~ — `use foo::bar;` selective import, auto-loading, mangling `mod__name`.
- ~~mut + loops~~ — `let mut x = ...; x := v;` + `while` + `break`/`continue`. `if` без else. Trailing `;`.
- ~~Pipeline `|>`~~ — sugar в parser.
- ~~for loop~~ — sugar в parser.
- ~~return keyword~~ — early exit.
- ~~or-patterns~~ — `1 | 2 | 3 =>`, `Red | Green | Blue =>`.
- ~~type aliases~~ — `type Bytes = Array[byte];`, resolved-away.
- ~~try_at~~ — `Option[T]` defensive read.
- ~~Pattern matching v1~~ — литералы/bind/exhaustivity по типу скрутини.
- ~~Pattern guards~~ — `pat if cond => body` для non-ADT match.
- ~~Linear types~~ — `linear struct/enum` + `drop_T` + alias/mut/data-position checks. Region унифицирован как первый-классный linear.
- ~~Dead move analysis (`takes_consume`/`tail_consume`/`is_consumed`/`is_copyable`/`consume_arg`)~~ — удалено ~195 строк после унификации Region.
- ~~T.func.param_drops field~~ — всегда [], удалено.
- ~~PWild как отдельный variant~~ — унифицирован с PBind "_".

---

## Не-долги, открытые дизайн-вопросы

См. `ROADMAP.md` секцию "◯ Открытые дизайн-вопросы" — closures,
threading, format() variadic, nested patterns, ADT guards, type-level
alignment. Каждый — серьёзная архитектурная работа, обсуждается
отдельно.
