#!/bin/bash
# $Id: sh-insert.sh,v 1.1 2006/08/26 01:08:18 philippej Exp $

version=0.1.0beta
cat > descr <<EOF
Markdown to HTML tool and extensible Markdown library.
OMD is a Markdown to HTML tool based on its own extensible Markdown library. OMD is implemented in OCaml and has no dependency other than the standard OCaml distribution.
EOF

cat > opam <<EOF
opam-version: "$version"
maintainer: "philippe.wang@gmail.com"
build: [
  [make "build"]
  [make "install"]
]
remove: [
  ["ocamlfind" "remove" "omd"]
]
depends : ["ocamlfind"]
EOF
rm -fr "omd.$version"
mkdir "omd.$version"
cp -r src tests setup.{data,ml} _tags _oasis README.md Makefile "omd.$version"
tar cvzf "omd.$version.tar.gz" "omd.$version"
cat > url <<EOF
archive: "http://pw374.github.io/software/omd.$version.tar.gz"
checksum: "$( (md5sum < omd.$version.tar.gz | sed 's/ *//g' ) || (md5 < omd.$version.tar.gz | sed 's/MD5 (.*) = \([a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]\)/\1/g') )"
EOF

oasis2opam http://pw374.github.io/software/omd.0.1.0beta.tar.gz

