##
# Omd
#
# @file

.PHONY: test build

build:
	dune build

test:
	-dune build @gen --auto-promote
	dune test

# end
