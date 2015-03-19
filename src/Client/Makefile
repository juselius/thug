all: thug.js

thug.js: Thug.hs
	fay -o $@ --library --strict Thug $<

install: thug.js
	install -m 644 $< ../../static/js
