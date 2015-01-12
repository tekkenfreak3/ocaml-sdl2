all: build

build:
	cd src && make
	cp src/game.native .

debug:
	cd src && make
	cp src/game.byte .

clean:
	cd src && make clean
