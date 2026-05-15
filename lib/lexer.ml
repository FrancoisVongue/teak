(* Lexer: char stream -> token stream.

   Rule: the first character of an identifier determines its class.
   - [a-z_] starts a regular identifier (variables, functions).
   - [A-Z]  starts a constructor identifier (types, constructors).
   This is the only place we look at character case. *)

open Token

exception Lex_error of string * int

let is_digit c = c >= '0' && c <= '9'
let is_lower c = c >= 'a' && c <= 'z'
let is_upper c = c >= 'A' && c <= 'Z'
let is_alnum c = is_lower c || is_upper c || is_digit c || c = '_'
let is_lower_ident_start c = is_lower c || c = '_'

let lower_ident_or_keyword s =
  match s with
  | "fn"    -> TFn
  | "let"   -> TLet
  | "if"    -> TIf
  | "else"  -> TElse
  | "true"  -> TTrue
  | "false" -> TFalse
  | "int"   -> TIntTy
  | "bool"  -> TBoolTy
  | "type"  -> TType
  | "struct" -> TStruct
  | "enum"  -> TEnum
  | "match" -> TMatch
  | "extern" -> TExtern
  | "ref"   -> TRef
  | "deref" -> TDeref
  | "panic" -> TPanic
  | "own"   -> TOwn
  | "take"  -> TTake
  | "unwrap" -> TUnwrap
  | "look"  -> TLook
  | "_"     -> TUnderscore
  | _       -> TIdent s

let lex (src : string) : token list =
  let n = String.length src in
  let buf = Buffer.create 64 in
  let tokens = ref [] in
  let push t = tokens := t :: !tokens in
  let i = ref 0 in

  while !i < n do
    let c = src.[!i] in
    match c with
    | ' ' | '\t' | '\n' | '\r' -> incr i

    | '/' when !i + 1 < n && src.[!i + 1] = '/' ->
        i := !i + 2;
        while !i < n && src.[!i] <> '\n' do incr i done

    | '(' -> push TLParen;   incr i
    | ')' -> push TRParen;   incr i
    | '{' -> push TLBrace;   incr i
    | '}' -> push TRBrace;   incr i
    | '[' -> push TLBracket; incr i
    | ']' -> push TRBracket; incr i
    | ',' -> push TComma;    incr i
    | ':' ->
        if !i + 1 < n && src.[!i + 1] = '=' then begin
          push TColonEq; i := !i + 2
        end else begin
          push TColon; incr i
        end
    | '?' ->
        if !i + 1 < n && src.[!i + 1] = '?' then begin
          push TQQ; i := !i + 2
        end else
          raise (Lex_error
            ("unexpected `?` (only `??` is recognized)", !i))
    | ';' -> push TSemi;     incr i
    | '=' ->
        if !i + 1 < n && src.[!i + 1] = '>' then begin
          push TFatArrow; i := !i + 2
        end else if !i + 1 < n && src.[!i + 1] = '=' then begin
          push TEqEq; i := !i + 2
        end else begin
          push TEq; incr i
        end

    | '-' ->
        if !i + 1 < n && src.[!i + 1] = '>' then begin
          push TArrow; i := !i + 2
        end else begin
          push TMinus; incr i
        end

    | '+' -> push TPlus;    incr i
    | '*' -> push TStar;    incr i
    | '%' -> push TPercent; incr i

    | '/' -> push TSlash; incr i

    | '<' ->
        if !i + 1 < n && src.[!i + 1] = '=' then begin
          push TLe; i := !i + 2
        end else begin push TLt; incr i end

    | '>' ->
        if !i + 1 < n && src.[!i + 1] = '=' then begin
          push TGe; i := !i + 2
        end else begin push TGt; incr i end

    | '!' ->
        if !i + 1 < n && src.[!i + 1] = '=' then begin
          push TNeq; i := !i + 2
        end else begin push TBang; incr i end

    | '&' when !i + 1 < n && src.[!i + 1] = '&' ->
        push TAndAnd; i := !i + 2

    | '|' when !i + 1 < n && src.[!i + 1] = '|' ->
        push TOrOr; i := !i + 2

    | '|' -> push TPipe; incr i

    | '.' ->
        if !i + 1 < n && src.[!i + 1] = '.' then begin
          push TDotDot; i := !i + 2
        end else begin
          push TDot; incr i
        end

    | c when is_digit c ->
        Buffer.clear buf;
        while !i < n && is_digit src.[!i] do
          Buffer.add_char buf src.[!i];
          incr i
        done;
        push (TInt (int_of_string (Buffer.contents buf)))

    | c when is_lower_ident_start c ->
        Buffer.clear buf;
        while !i < n && is_alnum src.[!i] do
          Buffer.add_char buf src.[!i];
          incr i
        done;
        push (lower_ident_or_keyword (Buffer.contents buf))

    | c when is_upper c ->
        Buffer.clear buf;
        while !i < n && is_alnum src.[!i] do
          Buffer.add_char buf src.[!i];
          incr i
        done;
        push (TCtorIdent (Buffer.contents buf))

    | c ->
        raise (Lex_error
          (Printf.sprintf "unexpected character %C" c, !i))
  done;
  push TEOF;
  List.rev !tokens
