all: thug.js

thug.js: Thug.hs
	fay -o $@ $<

install: thug.js
	install -m 644 $< ../../static/js
