default:
    @just --list

alias b := build
alias t := test

build:
    dune build

test:
    ./run_examples.sh

record:
    ./run_examples.sh --record

qbe:
    ./run_qbe.sh

context output="-":
    @./scripts/language_context.sh "{{output}}"

emit file:
    dune exec bin/main.exe -- {{file}}

emit-qbe file:
    dune exec bin/main.exe -- {{file}} --backend qbe -o /tmp/q.ssa

clean:
    dune clean
