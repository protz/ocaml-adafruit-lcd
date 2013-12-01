CFLAGS	= -Wall -Wextra -I../include -std=c99

.PHONY: all

all:
	ocamlbuild -use-ocamlfind -classic-display -package ctypes.foreign lcd.native

lcd: lcd.c
	gcc $(CFLAGS) -o $@ $<

test: all
	./lcd.native

clean:
	rm -f lcd.native lcd
	ocamlbuild -clean
