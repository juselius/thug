.PHONEY: all thug thug.hs

all: thug thug.js

thug:
	cabal build

thug.js:
	$(MAKE) -C src/Client
