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

(* hex digit value, or -1 if not a hex digit *)
let hex_val (c : char) : int =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
  else if c >= 'A' && c <= 'F' then Char.code c - Char.code 'A' + 10
  else -1

(* Append the UTF-8 encoding of a Unicode codepoint (assumed 0..0x10FFFF). This
   is where literals become real UTF-8 text: `"\u{e9}"` emits the bytes for é. *)
let utf8_add (buf : Buffer.t) (cp : int) : unit =
  if cp < 0x80 then
    Buffer.add_char buf (Char.chr cp)
  else if cp < 0x800 then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end else if cp < 0x10000 then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end

let lower_ident_or_keyword s =
  match s with
  | "fn"    -> TFn
  | "let"   -> TLet
  | "mut"   -> TMut
  | "if"    -> TIf
  | "else"  -> TElse
  | "while" -> TWhile
  | "break" -> TBreak
  | "continue" -> TContinue
  | "for"   -> TFor
  | "in"    -> TIn
  | "return" -> TReturn
  | "true"  -> TTrue
  | "false" -> TFalse
  | "int"   -> TIntTy
  | "bool"  -> TBoolTy
  | "byte"  -> TByteTy
  | "u16"   -> TU16Ty
  | "u32"   -> TU32Ty
  | "u64"   -> TU64Ty
  | "float" -> TFloatTy
  | "to_float" -> TToFloat
  | "type"  -> TType
  | "struct" -> TStruct
  | "enum"  -> TEnum
  | "resource" -> TResource
  | "unsafe" -> TUnsafe
  (* `drop` is NOT a language concept — a consumer is just a named by-value
     function the type's author writes. "drop" lexes as an ordinary ident. *)
  | "match" -> TMatch
  | "extern" -> TExtern
  | "use"   -> TUse
  | "const" -> TConst
  | "len"   -> TLen
  | "slice" -> TSlice
  | "to_int" -> TToInt
  | "to_byte" -> TToByte
  | "c_alloc" -> TCAlloc
  | "c_free"  -> TCFree
  | "null_ptr" -> TNullPtr
  | "is_null" -> TIsNull
  | "as_ptr" -> TAsPtr
  | "ptr_cast" -> TPtrCast
  | "reset" -> TReset
  | "try_at" -> TTryAt
  | "region" -> TRegion
  | "stack_region" -> TStackRegion
  | "aligned_region" -> TAlignedRegion
  | "await" -> TAwait
  | "spawn" -> TSpawn
  | "yield" -> TYield
  | "test"  -> TTest
  | "pub"   -> TPub
  | "namespace" -> TNamespace
  | "print"   -> TPrint
  | "println" -> TPrintln
  | "arena"   -> TArena
  | "closure" -> TClosure
  | "ref"     -> TRef
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
        end else if !i + 1 < n && src.[!i + 1] = ':' then begin
          push TColonCol; i := !i + 2
        end else begin
          push TColon; incr i
        end
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
    | '?' -> push TQuestion; incr i
    | '*' -> push TStar;    incr i
    | '%' -> push TPercent; incr i

    | '/' -> push TSlash; incr i

    | '<' ->
        if !i + 1 < n && src.[!i + 1] = '=' then begin
          push TLe; i := !i + 2
        end else if !i + 1 < n && src.[!i + 1] = '<' then begin
          push TShl; i := !i + 2
        end else begin push TLt; incr i end

    | '>' ->
        if !i + 1 < n && src.[!i + 1] = '=' then begin
          push TGe; i := !i + 2
        end else if !i + 1 < n && src.[!i + 1] = '>' then begin
          push TShr; i := !i + 2
        end else begin push TGt; incr i end

    | '!' ->
        if !i + 1 < n && src.[!i + 1] = '=' then begin
          push TNeq; i := !i + 2
        end else begin push TBang; incr i end

    | '&' when !i + 1 < n && src.[!i + 1] = '&' ->
        push TAndAnd; i := !i + 2

    | '&' -> push TAmp; incr i

    | '^' -> push TCaret; incr i

    | '~' -> push TTilde; incr i

    | '|' when !i + 1 < n && src.[!i + 1] = '|' ->
        push TOrOr; i := !i + 2

    | '|' when !i + 1 < n && src.[!i + 1] = '>' ->
        push TPipeArrow; i := !i + 2

    | '|' -> push TPipe; incr i

    | '.' ->
        if !i + 1 < n && src.[!i + 1] = '.' then begin
          push TDotDot; i := !i + 2
        end else begin
          push TDot; incr i
        end

    | '"' ->
        incr i;
        Buffer.clear buf;
        let closed = ref false in
        while !i < n && not !closed do
          let ch = src.[!i] in
          if ch = '"' then begin
            incr i; closed := true
          end else if ch = '\\' then begin
            if !i + 1 >= n then
              raise (Lex_error ("unterminated escape in string literal", !i));
            (match src.[!i + 1] with
             | 'n'  -> Buffer.add_char buf '\n'; i := !i + 2
             | 't'  -> Buffer.add_char buf '\t'; i := !i + 2
             | 'r'  -> Buffer.add_char buf '\r'; i := !i + 2
             | '0'  -> Buffer.add_char buf '\000'; i := !i + 2
             | '\\' -> Buffer.add_char buf '\\'; i := !i + 2
             | '"'  -> Buffer.add_char buf '"'; i := !i + 2
             | '\'' -> Buffer.add_char buf '\''; i := !i + 2
             | 'x'  ->
                 (* \xNN — exactly two hex digits → one raw byte *)
                 if !i + 3 >= n
                    || hex_val src.[!i + 2] < 0 || hex_val src.[!i + 3] < 0 then
                   raise (Lex_error ("\\x needs two hex digits", !i));
                 Buffer.add_char buf
                   (Char.chr (hex_val src.[!i + 2] * 16 + hex_val src.[!i + 3]));
                 i := !i + 4
             | 'u'  ->
                 (* \u{HEX} — a Unicode codepoint, emitted as UTF-8 bytes *)
                 if !i + 2 >= n || src.[!i + 2] <> '{' then
                   raise (Lex_error ("\\u must be followed by {HEX}", !i));
                 let j = ref (!i + 3) and cp = ref 0 and digits = ref 0 in
                 while !j < n && src.[!j] <> '}' do
                   let h = hex_val src.[!j] in
                   if h < 0 then
                     raise (Lex_error ("\\u{...} expects hex digits", !j));
                   cp := !cp * 16 + h; incr digits; incr j
                 done;
                 if !j >= n then
                   raise (Lex_error ("unterminated \\u{...}", !i));
                 if !digits = 0 || !cp > 0x10FFFF then
                   raise (Lex_error ("\\u{...} is not a valid codepoint", !i));
                 utf8_add buf !cp;
                 i := !j + 1                    (* past the closing '}' *)
             | c    -> raise (Lex_error
                 (Printf.sprintf "unknown escape \\%c in string literal" c, !i)))
          end else if ch = '\n' then
            raise (Lex_error ("newline in string literal — use \\n", !i))
          else begin
            Buffer.add_char buf ch;
            incr i
          end
        done;
        if not !closed then
          raise (Lex_error ("unterminated string literal", !i));
        push (TStringLit (Buffer.contents buf))

    | '\'' ->
        (* 'a' — a single byte (0..255). For multibyte text use a string with
           \u{...}; for a Unicode rune use the decode helpers in stdlib. *)
        incr i;                                       (* past opening quote *)
        if !i >= n then raise (Lex_error ("unterminated char literal", !i));
        let bval =
          if src.[!i] = '\\' then begin
            if !i + 1 >= n then
              raise (Lex_error ("unterminated escape in char literal", !i));
            (match src.[!i + 1] with
             | 'n'  -> i := !i + 2; 10
             | 't'  -> i := !i + 2; 9
             | 'r'  -> i := !i + 2; 13
             | '0'  -> i := !i + 2; 0
             | '\\' -> i := !i + 2; 92
             | '\'' -> i := !i + 2; 39
             | '"'  -> i := !i + 2; 34
             | 'x'  ->
                 if !i + 3 >= n
                    || hex_val src.[!i + 2] < 0 || hex_val src.[!i + 3] < 0 then
                   raise (Lex_error ("\\x needs two hex digits", !i));
                 let v = hex_val src.[!i + 2] * 16 + hex_val src.[!i + 3] in
                 i := !i + 4; v
             | c -> raise (Lex_error
                 (Printf.sprintf "unknown escape \\%c in char literal" c, !i)))
          end else begin
            let v = Char.code src.[!i] in incr i; v
          end
        in
        if !i >= n || src.[!i] <> '\'' then
          raise (Lex_error
            ("char literal must be a single byte (closing ' expected)", !i));
        incr i;                                       (* past closing quote *)
        push (TCharLit bval)

    | c when is_digit c ->
        (* Detect 0x.../0b... prefixes BEFORE consuming digits. *)
        if c = '0' && !i + 1 < n
           && (src.[!i + 1] = 'x' || src.[!i + 1] = 'X') then begin
          i := !i + 2;
          Buffer.clear buf;
          let is_hex ch =
            is_digit ch || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
          in
          while !i < n && is_hex src.[!i] do
            Buffer.add_char buf src.[!i]; incr i
          done;
          if Buffer.length buf = 0 then
            raise (Lex_error ("hex literal needs at least one digit", !i));
          push (TInt (Int64.of_string ("0x" ^ Buffer.contents buf)))
        end
        else if c = '0' && !i + 1 < n
                && (src.[!i + 1] = 'b' || src.[!i + 1] = 'B') then begin
          i := !i + 2;
          Buffer.clear buf;
          while !i < n && (src.[!i] = '0' || src.[!i] = '1') do
            Buffer.add_char buf src.[!i]; incr i
          done;
          if Buffer.length buf = 0 then
            raise (Lex_error ("binary literal needs at least one digit", !i));
          push (TInt (Int64.of_string ("0b" ^ Buffer.contents buf)))
        end
        else begin
          Buffer.clear buf;
          while !i < n && is_digit src.[!i] do
            Buffer.add_char buf src.[!i];
            incr i
          done;
          (* Float literal if the integer part is followed by '.' + digit
             or by 'e'/'E' (scientific). Plain `3.` is rejected — require
             an explicit `3.0` to avoid ambiguity with method calls later. *)
          let is_float =
            (!i + 1 < n && src.[!i] = '.' && is_digit src.[!i + 1])
            || (!i < n && (src.[!i] = 'e' || src.[!i] = 'E'))
          in
          if is_float then begin
            if !i < n && src.[!i] = '.' then begin
              Buffer.add_char buf '.';
              incr i;
              while !i < n && is_digit src.[!i] do
                Buffer.add_char buf src.[!i];
                incr i
              done
            end;
            if !i < n && (src.[!i] = 'e' || src.[!i] = 'E') then begin
              Buffer.add_char buf src.[!i];
              incr i;
              if !i < n && (src.[!i] = '+' || src.[!i] = '-') then begin
                Buffer.add_char buf src.[!i];
                incr i
              end;
              if not (!i < n && is_digit src.[!i]) then
                raise (Lex_error
                  ("malformed float exponent — digits required after `e`", !i));
              while !i < n && is_digit src.[!i] do
                Buffer.add_char buf src.[!i];
                incr i
              done
            end;
            push (TFloat (float_of_string (Buffer.contents buf)))
          end else
            push (TInt (Int64.of_string (Buffer.contents buf)))
        end

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
