LIBRARY = cpphs
VERSION = 1.20.8

DIRS	= Language/Preprocessor/Cpphs \
	  Text/ParserCombinators

SRCS	= Language/Preprocessor/Cpphs.hs \
          Language/Preprocessor/Cpphs/CppIfdef.hs \
          Language/Preprocessor/Cpphs/HashDefine.hs \
          Language/Preprocessor/Cpphs/MacroPass.hs \
          Language/Preprocessor/Cpphs/Options.hs \
          Language/Preprocessor/Cpphs/Position.hs \
          Language/Preprocessor/Cpphs/ReadFirst.hs \
          Language/Preprocessor/Cpphs/RunCpphs.hs \
          Language/Preprocessor/Cpphs/SymTab.hs \
          Language/Preprocessor/Cpphs/Tokenise.hs \
          Language/Preprocessor/Unlit.hs \
          cpphs.hs

AUX	= README LICENCE* CHANGELOG $(LIBRARY).cabal Setup.hs Makefile \
	  cpphs.hugs cpphs.compat \
	  tests/[A-BD-Z]* tests/[a-np-z]* \
	  docs/[a-z]*

HC	= ghc
HFLAGS	=
HEAP	=
HOSTSTRIP = strip

all: $(LIBRARY)
package:
	tar cf tmp.tar $(SRCS) $(AUX)
	mkdir $(LIBRARY)-$(VERSION)
	cd $(LIBRARY)-$(VERSION); tar xf ../tmp.tar
	tar --format=ustar -zcf $(LIBRARY)-$(VERSION).tar.gz $(LIBRARY)-$(VERSION)
	zip -r $(LIBRARY)-$(VERSION).zip $(LIBRARY)-$(VERSION)
	rm -r tmp.tar $(LIBRARY)-$(VERSION)
haddock: $(SRCS)
	mkdir -p docs/$(LIBRARY)
	for dir in $(DIRS); do mkdir -p docs/$(LIBRARY)/$$dir; done
	for file in $(SRCS); \
	    do HsColour -anchor -html $$file \
	          >docs/$(LIBRARY)/`dirname $$file`/`basename $$file .hs`.html;\
	    done
	haddock --html --title=$(LIBRARY) \
	    --odir=docs/$(LIBRARY) --package=$(LIBRARY) \
	    --source-module="%{MODULE/.//}.html" \
	    --source-entity="%{MODULE/.//}.html#%{NAME}" \
	    $(SRCS)



$(LIBRARY): $(SRCS)
	$(HC) $(HFLAGS) $(HEAP) -o $@  $(SRCS)
	$(HOSTSTRIP) $@
