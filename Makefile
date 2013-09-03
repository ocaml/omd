PKGNAME = $(shell oasis query name)
PKGVERSION = $(shell oasis query version)
PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

DISTFILES = README.md _oasis setup.ml Makefile \
  $(wildcard $(addprefix src/, *.ml *.mli *.mllib *.mlpack *.ab))

PREFIX = $(shell opam config var prefix)
ifneq ($(PREFIX),)
PREFIX_FLAG = --prefix $(PREFIX)
endif

.PHONY: all byte native doc install uninstall reinstall test

all byte native setup.log: configure
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml $< -configure --enable-tests $(PREFIX_FLAG)

setup.ml: _oasis
	oasis setup -setup-update dynamic
	touch $@

doc install uninstall reinstall test: setup.log
	ocaml setup.ml -$@


# Make a tarball
.PHONY: dist tar
dist tar: $(DISTFILES)
	mkdir $(PKGNAME)-$(PKGVERSION)
	cp --parents -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
#	setup.ml independent of oasis:
	cd $(PKGNAME)-$(PKGVERSION) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	$(RM) -rf $(PKGNAME)-$(PKGVERSION)

.PHONY: clean distclean dist-clean
clean:
	ocaml setup.ml -clean
	$(RM) $(PKG_TARBALL)

distclean dist-clean:: clean
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
