.PHONY: build test fmt run clean

build:
	dune build

test:
	dune runtest

fmt:
	dune fmt

run:
	dune exec oschaemll

clean:
	dune clean
