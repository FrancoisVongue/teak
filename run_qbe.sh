#!/bin/sh
# Compare the QBE backend against the C-backend baseline (exit codes).
# Requires a built `qbe` (set QBE=path, default qbe in PATH).
cd "$(dirname "$0")" || exit 1
dune build 2>&1 || exit 1
ORTO=_build/default/bin/main.exe
QBE="${QBE:-qbe}"
pass=0; unsup=0; mismatch=0; toolfail=0
for f in examples/*.orto std/tests/*.orto; do
  name=$(basename "$f" .orto)
  case "$f" in std/tests/*) name="stdtest_$name";; esac
  exp=$(grep -E "^$name RUN " run_baseline.txt | awk '{print $3}')
  [ -z "$exp" ] && continue
  err=$("$ORTO" "$f" --backend qbe -o /tmp/q.ssa 2>&1)
  echo "$err" | grep -q "wrote" || { unsup=$((unsup+1)); echo "UNSUP $name"; continue; }
  if ! "$QBE" /tmp/q.ssa > /tmp/q.s 2>/dev/null; then toolfail=$((toolfail+1)); echo "QBEFAIL $name"; continue; fi
  if ! cc /tmp/q.s runtime_qbe.c -lm -o /tmp/q 2>/dev/null; then toolfail=$((toolfail+1)); echo "CCFAIL $name"; continue; fi
  timeout 10 /tmp/q >/dev/null 2>&1
  got=$?
  if [ "$got" = "$exp" ]; then pass=$((pass+1)); else mismatch=$((mismatch+1)); echo "MISMATCH $name got=$got exp=$exp"; fi
done
echo "QBE vs C: PASS=$pass UNSUPPORTED=$unsup MISMATCH=$mismatch TOOLFAIL=$toolfail"
[ "$mismatch" = 0 ] && [ "$toolfail" = 0 ] && [ "$unsup" = 0 ]
