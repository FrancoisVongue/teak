#!/bin/sh
# Compare the Cranelift backend against the C-backend baseline (exit codes).
# Uses the orto-clif driver to turn emitted CLIF into a linkable object.
cd "$(dirname "$0")" || exit 1
dune build 2>&1 || exit 1
ORTO=_build/default/bin/main.exe
DRV="${DRV:-cranelift_driver/target/release/orto-clif}"
[ -x "$DRV" ] || { echo "driver not built: $DRV"; exit 1; }
pass=0; unsup=0; mismatch=0; toolfail=0
for f in examples/*.orto std/tests/*.orto; do
  name=$(basename "$f" .orto)
  case "$f" in std/tests/*) name="stdtest_$name";; esac
  exp=$(grep -E "^$name RUN " run_baseline.txt | awk '{print $3}')
  [ -z "$exp" ] && continue
  err=$("$ORTO" "$f" --backend clif -o /tmp/c.clif 2>&1)
  echo "$err" | grep -q "wrote" || { unsup=$((unsup+1)); echo "UNSUP $name"; continue; }
  if ! "$DRV" /tmp/c.clif /tmp/c.o >/dev/null 2>/tmp/c.err; then toolfail=$((toolfail+1)); echo "CLIFFAIL $name"; continue; fi
  if ! cc /tmp/c.o runtime_qbe.c -lm -o /tmp/c.bin 2>/dev/null; then toolfail=$((toolfail+1)); echo "CCFAIL $name"; continue; fi
  timeout 10 /tmp/c.bin >/dev/null 2>&1
  got=$?
  if [ "$got" = "$exp" ]; then pass=$((pass+1)); else mismatch=$((mismatch+1)); echo "MISMATCH $name got=$got exp=$exp"; fi
done
echo "CLIF vs C: PASS=$pass UNSUPPORTED=$unsup MISMATCH=$mismatch TOOLFAIL=$toolfail"
[ "$mismatch" = 0 ] && [ "$toolfail" = 0 ] && [ "$unsup" = 0 ]
