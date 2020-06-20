`omd`: Markdown library and tool in OCaml
=======================================

Omd is an OCaml library designed to parse and print Markdown into different
formats. In addition to the library, a command-line tool `omd` is included to
easily convert markdown into HTML.

Omd aims at implementing the [Commonmark](https://commonmark.org/) standard. The
version currently targeted is [0.29](https://spec.commonmark.org/0.29/).

Omd is developed on GitHub. If you need to report an issue, please do so at
https://github.com/ocaml/omd/issues.

Dependencies
------------

The minimum version of OCaml required is 4.09. Omd does not currently have any
dependencies apart from the standard library.

Installation
------------

The recommended way to install `omd` is via the [opam package manager][opam]:

```sh
$ opam install omd
```

You can also build it manually with:

```sh
$ dune build
```

You can run the testsuite by doing:

```sh
$ dune runtest
```
