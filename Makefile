RELEASEOPTS=--opt-all
DEBUGOPTS=--debug
HSFILES=app.hs config.hs Server/*.hs Client/*.hs

debug: $(HSFILES)
	hastec app.hs -fforce-recomp $(DEBUGOPTS) --output-html
	hastec config.hs -o index.html -fforce-recomp $(DEBUGOPTS) --output-html
	chmod 644 app.html
	chmod 644 index.html
	ghc --make -O2 app.hs

release: $(HSFILES)
	hastec app.hs -fforce-recomp $(RELEASEOPTS) --output-html
	hastec config.hs -o index.html -fforce-recomp $(RELEASEOPTS) --output-html
	chmod 644 app.html
	chmod 644 index.html
	ghc --make -O2 app.hs

clean:
	find . -iname '*.hi' -exec rm \{\} \;
	find . -iname '*.o' -exec rm \{\} \;
	find . -iname '*.jsmod' -exec rm \{\} \;
	find . -iname '*~' -exec rm \{\} \;
	rm -f booru.jpg
	rm -f config.txt

distclean: clean
	rm -f index.html app.html app
