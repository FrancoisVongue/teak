#!/bin/sh
# Regression runner: compile every examples/*.orto through orto + gcc,
# run it, and compare the exit code against a saved baseline.
#
#   ./run_examples.sh            # run, compare against baseline.txt
#   ./run_examples.sh --record   # (re)write baseline.txt
#
# GCCFAIL / NOMAIN_OR_ERR lines are recorded as-is; async/io_uring and
# library-only modules are expected to land there in this sandbox.
set -e
cd "$(dirname "$0")"
dune build 2>&1
ORTO=_build/default/bin/main.exe
OUT=/tmp/orto_run
mkdir -p "$OUT"
BASE=run_baseline.txt
RESULT="$OUT/result.txt"
: > "$RESULT"
for f in examples/*.orto; do
  name=$(basename "$f" .orto)
  if ! "$ORTO" "$f" -o "$OUT/$name.c" >/dev/null 2>&1; then
    echo "$name NOMAIN_OR_ERR" >> "$RESULT"; continue
  fi
  if gcc -O0 -w -o "$OUT/$name" "$OUT/$name.c" >/dev/null 2>&1; then
    set +e; timeout 5 "$OUT/$name" >/dev/null 2>&1; code=$?; set -e
    echo "$name RUN $code" >> "$RESULT"
  else
    echo "$name GCCFAIL" >> "$RESULT"
  fi
done
if [ "$1" = "--record" ]; then
  cp "$RESULT" "$BASE"
  echo "recorded baseline ($(wc -l < "$BASE") entries)"
else
  if diff -u "$BASE" "$RESULT"; then
    echo "OK — no regressions ($(wc -l < "$RESULT") entries)"
  else
    echo "REGRESSION — see diff above"; exit 1
  fi
fi
