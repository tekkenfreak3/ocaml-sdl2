all: build


all: build

build:
	cd src && make build
	cp src/game.native .

debug:
	cd src && make debug
	cp src/game.byte .

clean:
	find . -name "*~" -exec rm {} \;
	rm -rf src/_build
	find . -regextype posix-basic -regex ".*/game\.\(native\|byte\)" -exec rm {} \;
	find . -name  "*inferred*" -exec rm {} \;
