clean:
	rm -fr *.o *.js*

combine: mod.js
	cat test.jsexe/lib.js test.jsexe/rts.js test.jsexe/lib1.js mod.js > all.js

mod.js: mod.hs
	ghcjs mod.hs
	ghcjs --print-obj mod.js_o > mod.js
