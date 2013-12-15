CFLAGS	= -Wall -Wextra -I../include -std=c99

.PHONY: all

all:
	ocamlbuild -use-ocamlfind -classic-display -package ctypes.foreign demo.native

lcd: lcd.c
	gcc $(CFLAGS) -o $@ $<

test: all
	./demo.native

clean:
	rm -f demo demo.native
	ocamlbuild -clean
