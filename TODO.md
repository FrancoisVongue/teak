# orto compiler — technical debt

Долги, обнаруженные по ходу разработки. Каждый — кандидат на чистку,
но не блокер.

---

## Открытые

### 0. Closures — РАБОТАЮТ end-to-end (захват + first-class)
**Файлы:** ast.ml (`EFun`, `EClosure`), token.ml/lexer.ml (`closure`),
parser.ml, resolve.ml, lift.ml, check.ml (`TEMakeClosure`, поле
`func.captures`, infer EClosure, финализация lifted-лямбд), mono.ml,
emit.ml (fat-pointer представление, env-структуры, env-распаковка),
examples/lambda.orto, examples/closures.orto.

**Сделано:**
- `fn(p: T, ...) -> R { body }` без захвата → lambda-lifting в обычную
  top-level функцию (lift.ml).
- `closure(r, fn(...))` с захватом → env-struct в регионе `r`, толстый
  указатель `{env_slot, env_offset, env_gen, code}`.
- **Все function-значения унифицированы как толстые указатели.** Обычная
  функция = env_slot -1 (wrapper `__fnval_`). Замыкание = env в регионе.
  Один тип `fn(A)->B`, взаимозаменяемы везде: HOF-аргумент, возврат,
  поле структуры, элемент массива (всё проверено).
- **Безопасность едет на существующей gen-машинерии регионов:** вызов
  протухшего замыкания (регион сброшен) → abort, ровно как OOB массива.
  Новой оси памяти НЕ добавлено.
- Захват по копии. Захват линейного значения отвергается — следует из
  «у линейных типов нет операции копирования», не спецправило.

**Ортогональность проверена:** замыкания не потребовали спецслучаев в
массивах/структурах/возвратах — они значение существующего типа.
Заодно закрыт латентный баг ctor_map (TODO #2, см. ниже) в части
match-биндеров.

**Осознанно НЕ сделано (ради ортогональности):** стекового
non-escaping яруса нет. Он потребовал бы escape/taint-анализа (новая
ось, N²-взаимодействия). Вместо него один механизм: замыкание =
региональный агрегат. `map/filter/fold` используют тот же регион, что
и массивы — регион нужен ровно тогда, когда нужен массиву.

**Остаётся:**
1. Вывод типов параметров/возврата лямбды (сейчас обязательны).
2. Полиморфные замыкания (захват/сигнатура с type-var отвергается;
   lifted-лямбды мономорфны, type_params=[]).
3. async внутри тела замыкания (await/yield отвергается; замыкание,
   СОЗДАННОЕ в async-функции и вызванное синхронно, работает — env в
   регионе переживает frame hoisting).
4. Lazy[T] — теперь тривиально поверх `closure(r, ...)`.

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

### 6. for-in по связанной Stream-переменной не поддерживается
**Файл:** lib/emit.ml, async_split_segments TEForStream case
**Суть:** v1 поддерживает только инлайн-форму `for x in stream_extern(args) { ... }`.
Bound-форма `let s = stream_extern(...); for x in s { ... }` требует
вытянуть SQE prep в момент let'a и вернуть Stream[T] значение
{slot, gen} как в Task. Сейчас падает на emit. См. STAGE3_ASYNC.md §13
phase 6 (handed off as follow-up).

### 7. drop_Stream через ASYNC_CANCEL
**Файл:** lib/emit.ml, emit_stream_drop_defs
**Суть:** Сейчас drop_Stream помечает слот как DETACHED и multishot SQE
продолжает гореть до самозакрытия источника. По спеке §16 нужен
io_uring_prep_cancel перед освобождением. ~20 строк.

### 8a. `let x = v;` внутри async-функции (внутри while без suspension)
**Файл:** lib/emit.ml, async_rewrite_to_frame + sync emit_expr TELet.
**Суть:** Когда `let x = v` живёт ВНУТРИ async-функции, но окружающее
выражение (например `while` без suspension) делегирует генерацию sync
emit'у, sync TELet эмитит `int x = v;` как C-локал. Но
async_rewrite_to_frame переименовал все использования `x` в теле в
`fr->x`. Поэтому декларация неиспользуется, а тело читает
неинициализированное `fr->x`. Исправляется добавлением
`fr->x = v;` после let'a в async-контексте (или переписыванием TELet
в TEAssign внутри переписчика). Не задевает тесты без `let`-внутри-
неприостанавливающегося-`while`, но любой такой случай будет
давать мусор.

### 8b. Pattern match on tuples — `match t { (a, b, c) => ... }`
**Файл:** lib/parser.ml + check.ml — v1 поддерживает только
`let (a, b, c) = t;` и `t.0`. Расширение match-pattern'а: новый
вариант `PTuple of pat list`, проверка совместимости с TyTuple,
выполнение из `t.f0/f1/...`. Не блокирует — простые случаи
выражаются let+if.

### 8c. 1-tuple `(e,)`/`(T,)`
Сейчас отвергается. Не нужно для текущих задач, но если кто-нибудь
зацепится — добавить можно как обёртку над одним типом, mangling
`Tuple1_T`. Дешёво.

### 8d. Wide tuples — производительность
Кортежи передаются по значению (memcpy всех полей). Для кортежей в
сотни байт это становится заметным. Не актуально пока, но имеет
смысл когда такие кортежи появятся.

### 8. `for x in stream` — обработка multishot EOF без значения
**Файл:** lib/emit.ml, TEForStream lowering
**Суть:** Сейчас тело прогоняется ровно для каждого CQE; финальный CQE
с `more==0` тоже считается событием. Для accept_multishot это правильно
(последний CQE — закрытие источника, не accepted fd). Для recv_multishot
аналогично. Если в будущем понадобится автоматическая фильтрация EOF,
нужен явный SQE-shape-aware path. Пока — на программисте проверять
`if conn < 0 { break }`.

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
