.PHONY: all bindir build clean

all: bindir build

build:
	cabal install --bindir=bin

bindir:
	mkdir -p bin

clean:
	rm -rf dist bin *~ 
