RELEASEOPTS=--opt-all -Wall
DEBUGOPTS=--debug -Wall
ARMOPTS= -opta-mcpu=cortex-a7 -opta-mfpu=neon -opta-mtune=native -opta-mfloat-abi=hard
HSFILES=app.hs Config.hs Server/*.hs Client/*.hs

debug: debug-js debug-bin

debug-js: $(HSFILES)
	hastec app.hs -fforce-recomp $(DEBUGOPTS)
	chmod 644 app.js

debug-bin: debug-js app.html
	ghc --make app.hs
	./app --embed app.js app.html

release: release-js release-bin

release-bin: release-js app.html
	ghc --make -O2 app.hs
	./app --embed app.js app.html

release-js: $(HSFILES)
	hastec app.hs -fforce-recomp $(RELEASEOPTS)
	chmod 644 app.js

# Finish build process with precompiled JS
finish-arm: app.js app.html $(HSFILES)
	ghc --make -O2 app.hs
	./app --embed app.js app.html

clean:
	find . -iname '*.hi' -exec rm \{\} \;
	find . -iname '*.o' -exec rm \{\} \;
	find . -iname '*.jsmod' -exec rm \{\} \;
	find . -iname '*~' -exec rm \{\} \;

distclean: clean
	rm -f app.js app config.txt booru.jpg
