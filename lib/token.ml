(* Tokens. *)

type token =
  (* literals *)
  | TInt of int
  | TIdent of string
  | TCtorIdent of string
  (* keywords *)
  | TFn
  | TLet
  | TIf
  | TElse
  | TTrue
  | TFalse
  | TIntTy
  | TBoolTy
  | TType
  | TMatch
  | TExtern
  | TStruct
  | TEnum
  | TArray        (* array — sized buffer allocated in a region *)
  | TBuf          (* buf — raw stack-allocated array, compile-time size *)
  | TLen          (* len — array length *)
  | TRegion       (* region — owned arena, frees its buffer at scope-exit *)
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
  | TFn           -> "FN"
  | TLet          -> "LET"
  | TIf           -> "IF"
  | TElse         -> "ELSE"
  | TTrue         -> "TRUE"
  | TFalse        -> "FALSE"
  | TIntTy        -> "INT_TY"
  | TBoolTy       -> "BOOL_TY"
  | TType         -> "TYPE"
  | TMatch        -> "MATCH"
  | TExtern       -> "EXTERN"
  | TStruct       -> "STRUCT"
  | TEnum         -> "ENUM"
  | TArray        -> "ARRAY"
  | TBuf          -> "BUF"
  | TLen          -> "LEN"
  | TRegion       -> "REGION"
  | TUnderscore   -> "_"
  | TLParen       -> "("
  | TRParen       -> ")"
  | TLBrace       -> "{"
  | TRBrace       -> "}"
  | TLBracket     -> "["
  | TRBracket     -> "]"
  | TComma        -> ","
  | TColon        -> ":"
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
