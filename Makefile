BINDIR   ?= bin
PACKAGE  := quickdep

SCRIPTSDIR := scripts
SCRIPTS    := $(wildcard $(SCRIPTSDIR)/*.sh)
SCRIPTSBIN := $(SCRIPTS:$(SCRIPTSDIR)/%.sh=$(BINDIR)/%)

# generate path of cabal build artifact
ARCH        := $(shell uname -i)-linux
GHC         := ghc-$(shell ghc --version | awk '{print $$NF}')
PACKAGE_VER := $(shell sed -n 's/version\s*:\s*\(.*\)/\1/p' package.yaml)
CABAL_BUILD := dist-newstyle/build/$(ARCH)/$(GHC)/$(PACKAGE)-$(PACKAGE_VER)
CABAL_BUILD := $(CABAL_BUILD)/x/$(PACKAGE)/build/$(PACKAGE)

all: $(BINDIR)/quickdep-internal $(SCRIPTSBIN)

# we leave determining whether a new build is necessary to cabal
$(BINDIR)/quickdep-internal: $(BINDIR) force
	hpack
	cabal build
	cp $(CABAL_BUILD)/quickdep $@

$(BINDIR)/%: $(SCRIPTSDIR)/%.sh
	cp $< $@

$(BINDIR):
	mkdir -p $@

clean:
	[ -d $(BINDIR) ] && rm $(BINDIR) -rf
	cabal clean

.PHONY: force all clean
