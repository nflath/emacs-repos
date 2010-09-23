EMACS=emacs
VERSION=`sed -ne 's/^(defconst c-version "\(.*\)"/\1/p' < cc-defs.el`

ELISPFILES=\
 cc-align.el \
 cc-awk.el \
 cc-bytecomp.el \
 cc-cmds.el \
 cc-compat.el \
 cc-defs.el \
 cc-engine.el \
 cc-fonts.el \
 cc-langs.el \
 cc-menus.el \
 cc-mode.el \
 cc-styles.el \
 cc-subword.el \
 cc-vars.el

COREFILES=\
 NEWS \
 cc-mode.texi \
 $(ELISPFILES)

EXTRAFILES=\
 MANIFEST \
 README \
 COPYING \
 cc-guess.el \
 cc-lobotomy.el \
 cc-fix.el

.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) -batch -q -no-site-file -f batch-byte-compile $<

all: bytecomp

force:

bytecomp: $(subst .el,.elc,$(ELISPFILES))

derived-mode-ex.elc: force
	test -f derived-mode-ex.el || ln -s admin/derived-mode-ex.el .
	EMACSLOADPATH=".${EMACSLOADPATH:+:}${EMACSLOADPATH}" $(EMACS) -batch -q -no-site-file -f batch-byte-compile derived-mode-ex.el

release: docs dist

distdir:
	@test -d dist || mkdir dist

docs: distdir info html dvi ps
	tar cf - cc-mode.info* | gzip -c > dist/cc-mode.info.tar.gz
	tar cf - cc-mode.XEMACS.info* | gzip -c > dist/cc-mode.XEMACS.info.tar.gz
	version=$(VERSION) && \
	cd html && \
	$(RM) -r cc-mode-$$version && \
	mkdir cc-mode-$$version && \
	chmod 755 cc-mode-$$version && \
	cp *.html cc-mode-$$version && \
	chmod 644 cc-mode-$$version/* && \
	tar cf - cc-mode-$$version | gzip -c > ../dist/cc-mode.html.tar.gz
	gzip -c cc-mode.dvi > dist/cc-mode.dvi.gz
	gzip -c cc-mode.ps > dist/cc-mode.ps.gz
	gzip -c cc-mode.rev.ps > dist/cc-mode.rev.ps.gz

info:
	makeinfo cc-mode.texi
	makeinfo -DXEMACS -o cc-mode.XEMACS.info cc-mode.texi

# "html:" fixes broken links to the Emacs/Elisp manual in "html-raw:" by
# pointing them into http://www.gnu.org/.
html: html-raw
	./2www.gnu.org.sh html
html-raw:
	makeinfo --html -o html cc-mode.texi

dvi:
	test -d texi || mkdir texi
	cd texi && texi2dvi ../cc-mode.texi
	mv texi/cc-mode.dvi .

ps: dvi
	dvips -o cc-mode.ps cc-mode.dvi
	dvips -r -o cc-mode.rev.ps cc-mode.dvi

# The standalone release.
dist: force
	$(MAKE) "FILES=$(EXTRAFILES) $(COREFILES)" \
	        TARGZFILE=cc-mode-$(VERSION).tar.gz distcommon

# Used internally with FILES and TARGZFILE.
distcommon: distdir
	version=$(VERSION) && \
	$(RM) -r cc-mode-$$version && \
	mkdir cc-mode-$$version && \
	chmod 755 cc-mode-$$version && \
	cp $(FILES) cc-mode-$$version && \
	chmod 644 cc-mode-$$version/* && \
	( tar cf - cc-mode-$$version | gzip -c -9 > dist/$(TARGZFILE) )

test:
	$(EMACS) -q -batch -no-site-file -l tests/000tests.el -f do-all-tests

clean:
	$(RM) *.elc
	$(RM) ChangeLog.gz
	for d in cc-mode-* "texi" html/cc-mode-*; do \
	  if test -d $$d; then $(RM) -r $$d; else :; fi; \
	done

spotless: clean
	$(RM) cc-mode.info*
	$(RM) -r html
	$(RM) cc-mode.dvi cc-mode*.ps
	$(RM) -r dist

rcs2log:
	@rcs2log -u "mast	Martin Stjernholm	bug-cc-mode@gnu.org" \
		-u "acmacm	Alan Mackenzie	bug-cc-mode@gnu.org" \
		-l 74 cc-*.el
