(* Tokens. *)

type token =
  (* literals *)
  | TInt of int
  | TIdent of string
  | TCtorIdent of string
  | TStringLit of string   (* "..." — byte string literal *)
  (* keywords *)
  | TFn
  | TLet
  | TIf
  | TElse
  | TTrue
  | TFalse
  | TIntTy
  | TBoolTy
  | TByteTy       (* byte — 1-byte unsigned primitive *)
  | TType
  | TMatch
  | TExtern
  | TStruct
  | TEnum
  | TUse           (* use mod::item; — selective import *)
  | TArray        (* array — sized buffer allocated in a region *)
  | TLen          (* len — array length *)
  | TSlice        (* slice(a, lo, hi) — sub-handle into the same region *)
  | TToInt        (* to_int(b) — widen byte to int *)
  | TToByte       (* to_byte(n) — truncate int to byte *)
  | TCAlloc       (* c_alloc[T](n) — malloc n*sizeof(T), returns *T *)
  | TCFree        (* c_free(p) — free a raw pointer *)
  | TNullPtr      (* null_ptr[T]() — typed NULL *)
  | TIsNull       (* is_null(p) — NULL check *)
  | TArrayData    (* array_data(a) — *T pointing at the bytes of Array[T] *)
  | TRegion       (* region(N) — heap arena, malloc'd block *)
  | TStackRegion  (* stack_region(N) — N literal, block on stack *)
  | TAlignedRegion (* aligned_region(N, A) — heap, A-byte aligned *)
  | TUnderscore   (* `_` as a standalone token (wildcard pattern) *)
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
  | TInt n        -> Printf.sprintf "INT(%d)" n
  | TIdent s      -> Printf.sprintf "IDENT(%s)" s
  | TCtorIdent s  -> Printf.sprintf "CTOR(%s)" s
  | TStringLit s  -> Printf.sprintf "STR(%S)" s
  | TFn           -> "FN"
  | TLet          -> "LET"
  | TIf           -> "IF"
  | TElse         -> "ELSE"
  | TTrue         -> "TRUE"
  | TFalse        -> "FALSE"
  | TIntTy        -> "INT_TY"
  | TBoolTy       -> "BOOL_TY"
  | TByteTy       -> "BYTE_TY"
  | TType         -> "TYPE"
  | TMatch        -> "MATCH"
  | TExtern       -> "EXTERN"
  | TStruct       -> "STRUCT"
  | TEnum         -> "ENUM"
  | TUse          -> "USE"
  | TArray        -> "ARRAY"
  | TLen          -> "LEN"
  | TSlice        -> "SLICE"
  | TToInt        -> "TO_INT"
  | TToByte       -> "TO_BYTE"
  | TCAlloc       -> "C_ALLOC"
  | TCFree        -> "C_FREE"
  | TNullPtr      -> "NULL_PTR"
  | TIsNull       -> "IS_NULL"
  | TArrayData    -> "ARRAY_DATA"
  | TRegion       -> "REGION"
  | TStackRegion  -> "STACK_REGION"
  | TAlignedRegion -> "ALIGNED_REGION"
  | TUnderscore   -> "_"
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
