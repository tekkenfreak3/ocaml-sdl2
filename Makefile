all: build


all: build

build:
	cd src && make build
	cp src/game.native .

debug:
	cd src && make debug
	cp src/game.byte .

clean:
	cd src && make clean
