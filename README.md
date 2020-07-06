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

The minimum version of OCaml required is 4.04.2 Omd does not currently have any
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

History
-------

OMD has been developed by [Philippe Wang](https://github.com/pw374/) at [OCaml
Labs](http://ocaml.io/) in [Cambridge](http://www.cl.cam.ac.uk).

Its development was motivated by at least these facts:

- We wanted an OCaml implementation of Markdown; some OCaml parsers of Markdown
  existed before but they were incomplete. It's easier for an OCaml project to
  depend on an pure-OCaml implementation of Markdown than to depend some
  interface to a library implemented using another language, and this is ever
  more important since [Opam](https://opam.ocaml.org) exists.

- We wanted to provide a way to make the contents of the
  [OCaml.org](http://ocaml.org/) website be essentially in Markdown instead of
  HTML. And we wanted to this website to be implemented in OCaml.

- Having an OCaml implementation of Markdown is virtually mandatory for those
  who want to use a Markdown parser in a [Mirage](http://www.openmirage.org)
  application.  Note that OMD has replaced the previous Markdown parser of
  [COW](https://github.com/mirage/ocaml-cow), which has been developed as part
  of the Mirage project.

Thanks
------

Thank you to [Christophe Troestler](https://github.com/Chris00), [Ashish
Argawal](https://github.com/agarwal), [Sebastien
Mondet](https://github.com/smondet), [Thomas
Gazagnaire](https://github.com/samoht), [Daniel
Bünzli](https://github.com/dbuenzli), [Amir Chaudry](https://github.com/amirmc),
[Anil Madhavapeddy](https://github.com/avsm/), [David
Sheets](https://github.com/dsheets/), [Jeremy
Yallop](https://github.com/yallop/), and \<please insert your name here if you
believe you've been forgotten\> for their feedbacks and contributions to this
project.
