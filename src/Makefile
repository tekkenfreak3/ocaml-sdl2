all: build

PKGFLAGS=-pkg ctypes.foreign -pkg ctypes
build:
	corebuild -cflag -g $(PKGFLAGS) sdl2.inferred.mli && cp _build/*inferred* .
	corebuild -cflag -g $(PKGFLAGS) -lflags -cclib,-lSDL2,-cclib,-lSDL2_image,-cclib,-lSDL2_ttf game.native

debug:
	corebuild -cflag -g $(PKGFLAGS) sdl2.inferred.mli && cp _build/*inferred* .
	corebuild -cflag -g $(PKGFLAGS) -lflags -cclib,-lSDL2,-cclib,-lSDL2_image,-cclib,-lSDL2_ttf -custom game.byte

