#!/usr/bin/env python3
# cimport — generate orto `extern fn` declarations from a C header.
#
# Like Zig's @cImport / Rust's bindgen: we do NOT parse C ourselves —
# we let clang parse the header into its JSON AST, then translate the
# FunctionDecl nodes into orto extern declarations. MVP: functions only
# (structs/enums/macros are phase 2). Type map targets Linux x86-64 LP64.
#
#   python3 tools/cimport.py <header.h> [symbol ...]   > ffi.orto
import json, subprocess, sys, re

# C type (clang qualType string) -> orto type. ABI-equivalent on LP64.
SCALAR = {
    "int":"int", "signed int":"int",            # bridge: orto int -> C int
    "unsigned int":"u32", "unsigned":"u32",
    "short":"i16", "short int":"i16", "unsigned short":"u16",
    "long":"i64", "long int":"i64", "long long":"i64", "long long int":"i64",
    "unsigned long":"u64", "unsigned long int":"u64",
    "unsigned long long":"u64", "size_t":"u64", "ssize_t":"i64",
    "intptr_t":"i64", "uintptr_t":"u64", "ptrdiff_t":"i64",
    "char":"i8", "signed char":"i8", "unsigned char":"byte",
    "double":"f64", "float":"f32",
    "_Bool":"bool",
    "void":"()",                                  # only valid as return
}

def map_type(c):
    c = c.strip()
    # pointers: any `... *` -> opaque *byte (MVP); char* / const char* too
    if c.endswith("*"):
        inner = c[:-1].strip().removeprefix("const ").strip()
        if inner in ("char","const char","signed char","unsigned char","void"):
            return "*byte"
        # known scalar pointee -> *scalar, else opaque *byte
        return "*" + SCALAR.get(inner, "byte")
    c2 = c.removeprefix("const ").strip()
    return SCALAR.get(c2, None)   # None = unknown, skip the function

def ret_of(qual):                  # "RET (PARAMS)" -> RET
    i = qual.find("(")
    return qual[:i].strip() if i >= 0 else qual.strip()

def main():
    hdr = sys.argv[1]
    wanted = set(sys.argv[2:])
    js = subprocess.run(["clang","-Xclang","-ast-dump=json","-fsyntax-only",hdr],
                        capture_output=True, text=True).stdout
    ast = json.loads(js)
    seen = set()
    out = []
    for n in ast.get("inner", []):
        if n.get("kind") != "FunctionDecl" or "name" not in n: continue
        name = n["name"]
        if name in seen: continue
        if wanted and name not in wanted: continue
        seen.add(name)
        qual = n.get("type",{}).get("qualType","")
        params = [c.get("type",{}).get("qualType","")
                  for c in n.get("inner",[]) if c.get("kind")=="ParmVarDecl"]
        rty = map_type(ret_of(qual))
        ptys = [map_type(p) for p in params]
        if rty is None or any(p is None for p in ptys):
            out.append(f"// SKIP {name}: unmapped type in `{qual}`")
            continue
        args = ", ".join(f"a{i}: {t}" for i,t in enumerate(ptys))
        out.append(f"extern fn {name}({args}) -> {rty}")
    print("\n".join(out))

main()
