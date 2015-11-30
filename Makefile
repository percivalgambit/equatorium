all: equatorium

.PHONY: equatorium
equatorium: Main.elm Disk.elm Equatorium.elm
	elm-make $< --output=html/index.html

.PHONY: clean
clean:
	rm -rf elm-stuff
