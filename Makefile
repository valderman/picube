RELEASEOPTS=--opt-all -Wall
DEBUGOPTS=--debug -Wall
HSFILES=app.hs config.hs Server/*.hs Client/*.hs

debug: $(HSFILES)
	hastec app.hs -fforce-recomp $(DEBUGOPTS)
	chmod 644 app.js
	ghc --make app.hs
	./app --embed app.js app.html

release: $(HSFILES)
	hastec app.hs -fforce-recomp $(RELEASEOPTS)
	chmod 644 app.js
	ghc --make -O2 app.hs
	./app --embed app.js app.html

clean:
	find . -iname '*.hi' -exec rm \{\} \;
	find . -iname '*.o' -exec rm \{\} \;
	find . -iname '*.jsmod' -exec rm \{\} \;
	find . -iname '*~' -exec rm \{\} \;

distclean: clean
	rm -f index.html app.html app config.txt booru.jpg
