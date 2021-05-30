##
# Omd
#
# @file

.PHONY: test build fmt

build:
	dune build

test:
	-dune build @gen --auto-promote
	dune test

fmt:
	-dune build @fmt --auto-promote
# end
