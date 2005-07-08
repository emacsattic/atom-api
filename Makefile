#
# atom-api Makefile
#

## Copyright (C) 2005  Masayuki Ataka  <masayuki.ataka@gmail.com>

PACKAGE = atom-api
VERSION = 2005-06-27

prefix = /usr/local
datadir = ${prefix}/share
lispdir = ${datadir}/emacs/site-lisp/${PACKAGE}

DOC = ChangeLog Makefile
MISC =
EL = atom-api.el # atom-api-muse.el muse-atom.el
ELC = $(EL:.el=.elc)
LPATH = lpath.el

CP = cp -p
RM = rm -f
TAR = tar czvf
MAKE = make
MKDIR = mkdir
EMACS = emacs
INSTALL = install -m 644

ELCC    = $(EMACS) -batch -q -no-site-file -l lpath.el
DISTRIB = $(PACKAGE)-$(VERSION)

.PHONY : all compile-elc html
all: compile-elc

compile-elc: $(ELC)

%.elc: %.el
	$(ELCC) -f batch-byte-compile $<

#
# clean target
#
.PHONY : clean distclean
clean:
	$(RM) *~ *.orig *.rej
	$(RM) *.elc

distclean: clean
	$(RM) *.html
	$(RM) $(DISTRIB).tar.gz
	$(RM) -r $(DISTRIB)

#
# distribution target
#
.PHONY : dist floppy
dist: distclean info html INSTALL.ja
	$(MKDIR) $(DISTRIB)
	$(CP) $(DOC) $(EL) $(LPATH) $(DISTRIB)
	$(CP) -r doc $(DISTRIB)
	$(TAR) $(DISTRIB).tar.gz $(DISTRIB)

#
# install target
#
.PHONY : install
install:
	if [ ! -d $(lispdir) ]; then \
	  $(MKDIR) -p $(lispdir); \
	fi
	$(INSTALL) $(EL) $(ELC) $(lispdir)