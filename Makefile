all: equitorium

.PHONY: equitorium
equitorium: Main.elm Disk.elm
	elm-make $< --output=html/equitorium.html

.PHONY: clean
clean:
	$(RM) elm-stuff