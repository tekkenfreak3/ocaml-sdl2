all: build

build: clean
	corebuild -cflag -g -pkg ctypes.foreign sdl2.inferred.mli && cp _build/*inferred* .
	corebuild -cflag -g -pkg ctypes.foreign -lflags -cclib,-lSDL2,-cclib,-lSDL2_image game.native

debug: clean
	corebuild -cflag -g -pkg ctypes.foreign sdl2.inferred.mli && cp _build/*inferred* .
	corebuild -cflag -g -pkg ctypes.foreign -lflags -cclib,-lSDL2,-cclib,-lSDL2_image,-custom game.byte
clean:
	find . -name "*~" -exec rm {} \;
	rm -rf _build
	find . -regextype posix-basic -regex ".*/game\.\(native\|byte\)" -exec rm {} \;
	find . -name  "*inferred*" -exec rm {} \;
