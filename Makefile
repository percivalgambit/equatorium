all: equitorium

.PHONY: equitorium
equitorium: Main.elm Disk.elm Equitorium.elm
	elm-make $< --output=html/index.html

.PHONY: clean
clean:
	rm -rf elm-stuff