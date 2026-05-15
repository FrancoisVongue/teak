(* Tokens for stage B.

   New since stage 0:
   - TCtorIdent: identifier starting with uppercase. Used for both
     type names (`Shape`) and constructor names (`Circle`). Lexer
     decides by the first character, parser uses position to decide
     which it is.
   - TType, TMatch, TUnderscore: new keywords / token.
   - TFatArrow (=>): only used in match arms.

   No precedence levels and no operator tokens. *)

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
  | TRef
  | TDeref
  | TPanic
  | TOwn          (* own — heap allocation, exclusive ownership *)
  | TTake         (* take — explicit consume of an Own *)
  | TUnwrap       (* unwrap — copy value out of Own (only for copyable T) *)
  | TLook         (* look — read through Ref, returns Option (only for copyable T) *)
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
  | TColonEq      (* := *)
  | TQQ           (* ?? *)
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
  | TRef          -> "REF"
  | TDeref        -> "DEREF"
  | TPanic        -> "PANIC"
  | TOwn          -> "OWN"
  | TTake         -> "TAKE"
  | TUnwrap       -> "UNWRAP"
  | TLook         -> "LOOK"
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
  | TQQ           -> "??"
  | TEOF          -> "EOF"
