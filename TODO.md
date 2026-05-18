# orto compiler — technical debt

Долги, обнаруженные по ходу разработки. Каждый — кандидат на чистку,
но не блокер.

---

## Открытые

### 1. Неиспользуемые spread temp-биндинги
**Файл:** lib/check.ml (генерация TELet для spread base'ов)
**Суть:** когда после `..base` все поля переопределяются явно, временная
переменная для base'а становится мёртвой. Пример: `Stack { ..s, top: 1,
mid: 2, bot: 3, depth: 4 }` — `s` копируется в `_spread_1` и не
используется. GCC не варнит (struct copy), но это лишняя работа.
Фикс: пост-обработка после построения field_map — если для каждого
поля `_spread_N.field` НЕ финальное значение, дроп этот binding.
~20 строк.

### 2. TPipe токен висит без употребления
**Файл:** lib/lexer.ml, lib/token.ml
**Суть:** `|` теперь нигде не используется в грамматике. Зарезервирован
под возможные or-patterns (`match x { 1 | 2 => ... }`). Если or-patterns
не будем делать — удалить.

### 3. `type` keyword занят, но только ошибка
**Файл:** lib/parser.ml
**Суть:** `type` зарезервирован под будущие aliases (`type Bytes = ...`).
Сейчас выдаёт ошибку с подсказкой "используй struct/enum".

### 4. ctor_map использует имя конструктора как ключ — коллизия после mono
**Файл:** lib/emit.ml, build_ctor_map
**Суть:** после мономорфизации Option_int и Option_bool оба имеют ctor
"Some". `Hashtbl.add` ставит обе записи, `Hashtbl.find` возвращает
случайную (последнюю). Для tag это OK (tag всегда тот же для одного
ctor name), но arg_tys могут оказаться от не-той инстанции.
Фикс: ключ в ctor_map должен включать имя owner'а mangled, не
только ctor name. ~20 строк.

### 5. Region_header утечка
В v1 заголовки регионов накапливаются — buffer освобождается, header
остаётся (нужен для gen-check на dangling Array). Для long-running
программ это рост. Фикс: pool allocator слотов под headers с
переиспользованием по gen.

### 6. Generic-T copyability дыра
`is_copyable (TyVar _) = true`. Region теоретически может пройти через
generic функцию, и внутри функции линейность не отслеживается. Лечится
запретом linear в TyVar position на этапе монолорфизации, или
проверкой после mono. На практике редко происходит.

### 7. `_drop_N` имя протекает в C-output
`let _ = linear_value` → переименовывается в `_drop_N` и попадает в C
как имя переменной. Программист видит. Не баг, но косметика. Можно
ввести специальный AST-узел `TEDiscard`.

---

## Закрыто

- ~~Дублирование validate_ty / validate_ty_for_ascription~~ — унифицировано.
- ~~Имя ty_contains_own устарело~~ — переименовано в ty_contains_linear.
- ~~Параллельные Array/Buf таблицы в emit~~ — объединены через handle_kind.
- ~~Fake `let p = p; body` wrap для param-drop~~ — заменено на T.func.param_drops.
- ~~Дубликат диспатча в TEIndex/TEAssignIdx~~ — вынесен в index_setup helper.
- ~~Own/Ref/take/unwrap/look/legacy ref/deref/:=/??/panic~~ — удалено.
- ~~Cascade destructors~~ — невозможны по построению (linear только Region).
- ~~Strings~~ — `Array[byte]` + статический region для литералов + `slice` + `to_int`/`to_byte`.
- ~~Raw pointers `*T`~~ — `c_alloc`/`c_free`/`*p`/`null_ptr`/`is_null`/`array_data` для FFI.
- ~~Модули~~ — `use foo::bar;` selective import, auto-loading из той же директории, mangling `mod__name`.
- ~~mut + loops~~ — `let mut x = ...; x := v;` + `while cond { body }` + `break`/`continue`. `if` без else. Trailing `;` отбрасывает значение.

---

## Не-долги, открытые дизайн-вопросы

- **Option как privileged ADT.** Имена Some/None зарезервированы.
  Используется только пользователем — компилятор больше Option не
  генерирует. Можно сделать user-defined.
- **`type` keyword под type aliases.** Не реализовано.
- **Lambda / closures.** Пока только именованные функции верхнего уровня.
- **Pipeline `|>` оператор.** Обсуждался как сахар. Не делаем.
- **Сырые указатели `*T`** для системного программирования. Параллельный
  путь к региону + array. Не реализовано.
