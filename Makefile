all: equitorium

.PHONY: equitorium
equitorium: Main.elm Disk.elm Equitorium.elm | html
	elm-make $< --output=html/index.html

html:
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf elm-stuff
