(* Tokens. *)

type token =
  (* literals *)
  | TInt of int64
  | TFloat of float
  | TIdent of string
  | TCtorIdent of string
  | TStringLit of string   (* "..." — byte string literal *)
  (* keywords *)
  | TFn
  | TLet
  | TMut          (* mut — `let mut x = ...;` for reassignable bindings *)
  | TIf
  | TElse
  | TWhile        (* while cond { body } *)
  | TBreak        (* break; — early exit from while *)
  | TContinue     (* continue; — skip to next iteration *)
  | TFor          (* for i in lo..hi { body } — sugar for while+mut *)
  | TIn           (* in — used in `for i in lo..hi` *)
  | TReturn       (* return expr; — early exit from a function *)
  | TTrue
  | TFalse
  | TIntTy
  | TBoolTy
  | TByteTy       (* byte — 1-byte unsigned primitive *)
  | TU16Ty        (* u16 — 2-byte unsigned, for binary protocols *)
  | TU32Ty        (* u32 — 4-byte unsigned *)
  | TU64Ty        (* u64 — 8-byte unsigned *)
  | TFloatTy      (* float — IEEE 754 double-precision *)
  | TToFloat      (* to_float(n) — int → float *)
  | TType
  | TMatch
  | TExtern
  | TStruct
  | TEnum
  | TLinear        (* linear — marks a struct/enum as move-only with user drop *)
  | TDrop          (* drop(x) — consume and run the type's destructor *)
  | TUse           (* use mod::item; — selective import *)
  | TConst         (* const NAME: T = expr; — named compile-time value *)
  | TLen          (* len — array length *)
  | TSlice        (* slice(a, lo, hi) — sub-handle into the same region *)
  | TToInt        (* to_int(b) — widen byte to int *)
  | TToByte       (* to_byte(n) — truncate int to byte *)
  | TToU16        (* to_u16(n) — truncate int to u16 *)
  | TToU32        (* to_u32(n) — truncate int to u32 *)
  | TToU64        (* to_u64(n) — truncate int to u64 *)
  | TCAlloc       (* c_alloc[T](n) — malloc n*sizeof(T), returns *T *)
  | TCFree        (* c_free(p) — free a raw pointer *)
  | TNullPtr      (* null_ptr[T]() — typed NULL *)
  | TIsNull       (* is_null(p) — NULL check *)
  | TArrayData    (* array_data(a) — *T pointing at the bytes of Array[T] *)
  | TPtrCast      (* ptr_cast[T](e) — reinterpret a raw pointer/address as *T *)
  | TTryAt        (* try_at(a, i) — defensive read, returns Option[T] *)
  | TRegion       (* region(N) — heap arena, malloc'd block *)
  | TStackRegion  (* stack_region(N) — N literal, block on stack *)
  | TAlignedRegion (* aligned_region(N, A) — heap, A-byte aligned *)
  | TAwait        (* await — Stage 3 async suspension point *)
  | TSpawn        (* spawn f(...) — Stage 3 fire-and-forget task *)
  | TYield        (* yield — Stage 3 cooperative scheduling point *)
  | TUnderscore   (* `_` as a standalone token (wildcard pattern) *)
  | TQuestion     (* postfix `?` — Result early-return *)
  | TPub          (* `pub use foo::{a};` — re-export *)
  | TTest         (* `test "name" { body }` — test block, --test mode *)
  | TNamespace    (* `namespace foo::bar { decls }` — nested namespace block *)
  | TPrint        (* print(expr)    — intrinsic: writev a tuple or scalar *)
  | TPrintln      (* println(expr)  — same, plus trailing '\n' *)
  | TArena        (* arena r = region(N); — scope-bound region binding *)
  | TClosure      (* closure(r, fn...) — capturing lambda, env in region r *)
  | TRef          (* ref(r, v) — allocate cell(s) in region r, return Ref[T] *)
  (* punctuation *)
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TLBracket     (* [ *)
  | TRBracket     (* ] *)
  | TComma
  | TColon
  | TColonCol     (* :: — module path separator *)
  | TSemi
  | TEq
  | TArrow        (* -> *)
  | TFatArrow     (* => *)
  | TPipe         (* |  *)
  | TPipeArrow    (* |> — pipeline: x |> f means f(x) *)
  | TAmp          (* &  bitwise AND *)
  | TCaret        (* ^  bitwise XOR *)
  | TTilde        (* ~  bitwise NOT *)
  | TShl          (* << *)
  | TShr          (* >> *)
  | TPlus         (* +  *)
  | TMinus        (* -  *)
  | TStar         (* *  *)
  | TSlash        (* /  *)
  | TPercent      (* %  *)
  | TEqEq         (* == *)
  | TNeq          (* != *)
  | TLt           (* <  *)
  | TGt           (* >  *)
  | TLe           (* <= *)
  | TGe           (* >= *)
  | TAndAnd       (* && *)
  | TOrOr         (* || *)
  | TBang         (* !  *)
  | TDot          (* .  *)
  | TDotDot       (* .. *)
  | TColonEq      (* := — only for array index assignment a[i] := v *)
  | TEOF

let show = function
  | TInt n        -> Printf.sprintf "INT(%Ld)" n
  | TFloat f      -> Printf.sprintf "FLOAT(%g)" f
  | TIdent s      -> Printf.sprintf "IDENT(%s)" s
  | TCtorIdent s  -> Printf.sprintf "CTOR(%s)" s
  | TStringLit s  -> Printf.sprintf "STR(%S)" s
  | TFn           -> "FN"
  | TLet          -> "LET"
  | TMut          -> "MUT"
  | TIf           -> "IF"
  | TElse         -> "ELSE"
  | TWhile        -> "WHILE"
  | TBreak        -> "BREAK"
  | TContinue     -> "CONTINUE"
  | TFor          -> "FOR"
  | TIn           -> "IN"
  | TReturn       -> "RETURN"
  | TTrue         -> "TRUE"
  | TFalse        -> "FALSE"
  | TIntTy        -> "INT_TY"
  | TBoolTy       -> "BOOL_TY"
  | TByteTy       -> "BYTE_TY"
  | TU16Ty        -> "U16_TY"
  | TU32Ty        -> "U32_TY"
  | TU64Ty        -> "U64_TY"
  | TFloatTy      -> "FLOAT_TY"
  | TToFloat      -> "TO_FLOAT"
  | TType         -> "TYPE"
  | TMatch        -> "MATCH"
  | TExtern       -> "EXTERN"
  | TStruct       -> "STRUCT"
  | TEnum         -> "ENUM"
  | TLinear       -> "LINEAR"
  | TDrop         -> "DROP"
  | TUse          -> "USE"
  | TConst        -> "CONST"
  | TLen          -> "LEN"
  | TSlice        -> "SLICE"
  | TToInt        -> "TO_INT"
  | TToByte       -> "TO_BYTE"
  | TToU16        -> "TO_U16"
  | TToU32        -> "TO_U32"
  | TToU64        -> "TO_U64"
  | TCAlloc       -> "C_ALLOC"
  | TCFree        -> "C_FREE"
  | TNullPtr      -> "NULL_PTR"
  | TIsNull       -> "IS_NULL"
  | TArrayData    -> "ARRAY_DATA"
  | TPtrCast      -> "PTR_CAST"
  | TTryAt        -> "TRY_AT"
  | TRegion       -> "REGION"
  | TStackRegion  -> "STACK_REGION"
  | TAlignedRegion -> "ALIGNED_REGION"
  | TAwait        -> "AWAIT"
  | TSpawn        -> "SPAWN"
  | TYield        -> "YIELD"
  | TUnderscore   -> "_"
  | TQuestion     -> "?"
  | TPub          -> "PUB"
  | TTest         -> "TEST"
  | TNamespace    -> "NAMESPACE"
  | TPrint        -> "PRINT"
  | TPrintln      -> "PRINTLN"
  | TArena        -> "ARENA"
  | TClosure      -> "CLOSURE"
  | TRef          -> "REF"
  | TLParen       -> "("
  | TRParen       -> ")"
  | TLBrace       -> "{"
  | TRBrace       -> "}"
  | TLBracket     -> "["
  | TRBracket     -> "]"
  | TComma        -> ","
  | TColon        -> ":"
  | TColonCol     -> "::"
  | TSemi         -> ";"
  | TEq           -> "="
  | TArrow        -> "->"
  | TFatArrow     -> "=>"
  | TPipe         -> "|"
  | TPipeArrow    -> "|>"
  | TAmp          -> "&"
  | TCaret        -> "^"
  | TTilde        -> "~"
  | TShl          -> "<<"
  | TShr          -> ">>"
  | TPlus         -> "+"
  | TMinus        -> "-"
  | TStar         -> "*"
  | TSlash        -> "/"
  | TPercent      -> "%"
  | TEqEq         -> "=="
  | TNeq          -> "!="
  | TLt           -> "<"
  | TGt           -> ">"
  | TLe           -> "<="
  | TGe           -> ">="
  | TAndAnd       -> "&&"
  | TOrOr         -> "||"
  | TBang         -> "!"
  | TDot          -> "."
  | TDotDot       -> ".."
  | TColonEq      -> ":="
  | TEOF          -> "EOF"
