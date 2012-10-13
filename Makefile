GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2 -threaded
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.1

.PHONY: all clean doc install

all: report.html doc dist/build/bitbrawl/bitbrawl dist/bitbrawl-$(VERSION).tar.gz

install: dist/build/bitbrawl/bitbrawl
	cabal install

report.html: BitBrawl/Main.hs BitBrawl/Util.hs BitBrawl/SDLgfx.hs BitBrawl/Animation.hs BitBrawl/Colour.hs BitBrawl/SDL.hs BitBrawl/Types.hs BitBrawl/Derive.hs
	-hlint $(HLINTFLAGS) --report BitBrawl

doc: dist/doc/html/bitbrawl/index.html README

dist/doc/html/bitbrawl/index.html: dist/setup-config BitBrawl/Main.hs BitBrawl/Util.hs BitBrawl/SDLgfx.hs BitBrawl/Animation.hs BitBrawl/Colour.hs BitBrawl/SDL.hs BitBrawl/Types.hs BitBrawl/Derive.hs
	-cabal haddock --hyperlink-source --executables

dist/setup-config: bitbrawl.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc BitBrawl/Derive.hs

BitBrawl/Derive.hs: BitBrawl/Types.hs
	derive -m BitBrawl.Derive \
		-iBitBrawl.Types \
		-iData.Lens.Common \
		-i'qualified Graphics.UI.SDL as SDL' \
		-i'qualified Graphics.UI.SDL.TTF as SDL.TTF' \
		-i'qualified Graphics.UI.SDL.Mixer as SDL.Mixer' \
		-i'qualified Physics.Hipmunk as H' \
		-d Lens BitBrawl/Types.hs > $@

dist/build/bitbrawl/bitbrawl: bitbrawl.cabal dist/setup-config BitBrawl/Main.hs BitBrawl/Util.hs BitBrawl/SDLgfx.hs BitBrawl/Animation.hs BitBrawl/Colour.hs BitBrawl/SDL.hs BitBrawl/Types.hs BitBrawl/Derive.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/bitbrawl-$(VERSION).tar.gz: bitbrawl.cabal dist/setup-config README BitBrawl/Main.hs BitBrawl/Util.hs BitBrawl/SDLgfx.hs BitBrawl/Animation.hs BitBrawl/Colour.hs BitBrawl/SDL.hs BitBrawl/Types.hs BitBrawl/Derive.hs
	cabal check
	cabal sdist
