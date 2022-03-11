##
# Omd
#
# @file

.PHONY: test build fmt deps

build: deps
	dune build

deps:
	opam install . --deps-only --yes

test:
	-dune build @gen --auto-promote
	dune test

fmt:
	-dune build @fmt --auto-promote
# end
