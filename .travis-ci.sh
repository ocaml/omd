#!/bin/bash -xue

PATH=~/ocaml/bin:$PATH; export PATH
OPAMYES="true"; export OPAMYES

TARGET="$1"; shift

case "$TARGET" in
  prepare)
    echo -en "travis_fold:start:ocaml\r"
    if [ ! -e ~/ocaml/cached-version -o "$(cat ~/ocaml/cached-version)" != "$OCAML_VERSION.$OCAML_RELEASE" ] ; then
      rm -rf ~/ocaml
      mkdir -p ~/ocaml/src
      cd ~/ocaml/src
      wget http://caml.inria.fr/pub/distrib/ocaml-$OCAML_VERSION/ocaml-$OCAML_VERSION.$OCAML_RELEASE.tar.gz
      tar -xzf ocaml-$OCAML_VERSION.$OCAML_RELEASE.tar.gz
      cd ocaml-$OCAML_VERSION.$OCAML_RELEASE
      ./configure -prefix ~/ocaml
      make world.opt
      make install
      cd ../..
      rm -rf src
      echo "$OCAML_VERSION.$OCAML_RELEASE" > ~/ocaml/cached-version
    fi
    echo -en "travis_fold:end:ocaml\r"

    echo -en "travis_fold:start:opam.init\r"
    PREFIX=/home/travis
    if [ ! -e ~/ocaml/bin/opam -o "$OPAM_RESET" = "1" ] ; then
      mkdir ~/ocaml/src
      cd ~/ocaml/src
      wget https://github.com/ocaml/opam/releases/download/2.0.0-rc4/opam-full-2.0.0.tar.gz
      tar -xzf opam-full-2.0.0.tar.gz
      cd opam-full-2.0.0
      ./configure --prefix=$PREFIX/ocaml
      make lib-ext
      make all
      make install
      cd ../..
      rm -rf src
      rm -rf ~/.opam
      opam init --disable-sandboxing
      eval $(opam config env)
      opam install dune uchar
      rm -rf ~/.opam-start
      mv ~/.opam ~/.opam-start
    fi
    cp -a ~/.opam-start ~/.opam
    echo -en "travis_fold:end:opam.init\r"
  ;;
  build)
    eval $(opam config env)
    dune build --display=short
    dune clean
    dune runtest
    opam pin add omd .
  ;;
  *)
    echo "bad command $TARGET">&2; exit 1
  ;;
esac
