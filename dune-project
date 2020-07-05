(lang dune 2.0)

(name omd)

(version 2.0.0)

(generate_opam_files)

(license ISC)

(authors "Philippe Wang <philippe.wang@gmail.com>"
  "Nicol\195\161s Ojeda B\195\164r <n.oje.bar@gmail.com>")

(source
 (github ocaml/omd))

(package
 (name omd)
 (synopsis "A Markdown frontend in pure OCaml")
 (description
   "This Markdown library is implemented using only pure OCaml (including\nI/O operations provided by the standard OCaml compiler distribution).\nOMD is meant to be as faithful as possible to the original Markdown.\nAdditionally, OMD implements a few Github markdown features, an\nextension mechanism, and some other features. Note that the opam\npackage installs both the OMD library and the command line tool `omd`.")
 (tags
  (org:ocamllabs org:mirage)))
