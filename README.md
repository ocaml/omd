OMD: extensible Markdown library and tool in OCaml
==================================================

OMD provides two things:

1. the command-line tool `omd`, which takes some Markdown and 
   converts it to HTML or Markdown.

   Use `omd -help` for more information on how to use it.

2. the library for OCaml contains several modules:
   - the module `Omd` contains most functions a user will need for basic
     Markdown manipulation.
   - the modules `Omd_parser`, `Omd_lexer`, `Omd_backend`, `Omd_representation` and `Omd_utils` basically implement what their names say:
     * `Omd_parser` implements the parser (the most complex part).
     * `Omd_lexer` implements a (basic) lexer.
     * `Omd_backend` implements 3 backends:
        1. HTML: default backend.
        2. Markdown: sometimes it's useful to show that 
           the fix-point is easily reachable.
        3. S-expression: it's mainly used for debugging.
     * `Omd_representation` declares the datatypes used in `Omd`. 
       It also provides some functions to work on those datatypes.
     * `Omd_utils` provides some useful tools that are not very specific
       to the OMD-specific datatypes.


OMD aims at implementing the ["original Markdown 
specs"](http://daringfireball.net/projects/markdown/syntax) with a few
Github Flavour Markdown characteristics. OMD is also meant to be more
"sane" than other Markdown parsers from the semantics point of view: if 
something bothers you from the semantics point of view, please [open an
issue on Github](https://github.com/pw374/omd/issues).


Encoding
--------

**OMD assumes its input is US-ASCII or UTF-8 encoded.**

Dependencies
------------

OMD is implemented in OCaml, therefore it needs it to be compiled.
OCaml 4.00.1 and then 4.01.0 have been used. OMD should be compatible
with 3.12.0 as well, if it's not then please [open an
issue](https://github.com/pw374/omd/issues).


The opam package for OMD depends on ocamlfind, which is only
used to compile and install OMD.

The root Makefile uses oasis, ocamlbuild and oasis2opam.
The Makefile in src/ only use the compilers from the standard
distribution of OCaml.

OMD, compiled as a library and/or a tool, doesn't depend on
anything other than the OCaml standard library and runtime.

----------------

Usage
-----

- to install `omd` using opam (recommended)

   `opam install omd`

- to get the development version of omd

  `git clone git://github.com/pw374/omd.git`

- to compile `omd`
  - without `oasis` nor `ocamlbuild`

      `cd omd/src && make`

  - using `oasis` and `ocamlbuild`

      `cd omd && make`


----------------

Log
---

The recommended version numbers are typefaced in **bold**.
As new releases come out and bugs are discovered, a version can stop
being recommended.

- 1.0.0: this release won't be/isn't compatible with previous versions.
  Hopefully it won't break too many things when it gets released.

- tags 1.0.0.x precede 1.0.0.
Also, tags 1.0.0.x will not be released in OPAM, next release will be 1.0.0.
And 1.0.0.x may not be compatible with each other.

- tag 1.0.0.g: accept HTML blocks which directly follow each other

- tag 1.0.0.f: fix: accept all XML-compatible attribute names for HTML attributes

- tag 1.0.0.e: fix backslash-escaping for hash-ending ATX-titles + fix Markdown output for Html_block

- tag 1.0.0.d: fix (HTML parsing) bugs introduced in 1.0.0.b and 1.0.0.c

- tag 1.0.0.c: rewrite parser of block HTML to use the updated Omd.t

- tag 1.0.0.b: rewrite parser of inline HTML to use the updated Omd.t

- tag 1.0.0.a: upgrade Omd.t for HTML representation

- **0.9.7**: introduction of media:end + bug fixes

- 0.9.6: fix a bug (concerning extensions) introduced by 0.9.4.

  This version is recommended. If you need to have a version that still has
  `Tag of extension` instead of `Tag of name * extension`, you should use 0.9.3

- 0.9.5: bug fix + `Tag of extension` changed to `Tag of name * extension`

- 0.9.4: fixes a bug for the new feature

- **0.9.3**: new feature `media:type="text/omd"`. 

  This version is recommended if you do not use that new feature. 
  Otherwise, it's strongly recommended that you use 0.9.6 instead.

- 0.9.2: not released...

- older versions: cf. [commit log](https://github.com/pw374/omd/commits/master)
