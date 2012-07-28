GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.1

.PHONY: all clean doc install

all: report.html doc dist/build/bitbrawl/bitbrawl dist/bitbrawl-$(VERSION).tar.gz

install: dist/build/bitbrawl/bitbrawl
	cabal install

report.html: bitbrawl.hs
	-hlint $(HLINTFLAGS) --report .

doc: dist/doc/html/bitbrawl/index.html README

dist/doc/html/bitbrawl/index.html: dist/setup-config bitbrawl.hs BitBrawl/Util.hs BitBrawl/SDLgfx.hs BitBrawl/Animation.hs BitBrawl/Colour.hs
	cabal haddock --hyperlink-source

dist/setup-config: bitbrawl.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc

dist/build/bitbrawl/bitbrawl: bitbrawl.cabal dist/setup-config bitbrawl.hs BitBrawl/Util.hs BitBrawl/SDLgfx.hs BitBrawl/Animation.hs BitBrawl/Colour.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/bitbrawl-$(VERSION).tar.gz: bitbrawl.cabal dist/setup-config README bitbrawl.hs BitBrawl/Util.hs BitBrawl/SDLgfx.hs BitBrawl/Animation.hs BitBrawl/Colour.hs
	cabal check
	cabal sdist
