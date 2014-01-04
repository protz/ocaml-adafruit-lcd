.PHONY: all

TARGETS	:= demo.native lcd.cma lcd.cmxa lcd.cmxs

all:
	ocamlbuild -use-ocamlfind -classic-display $(TARGETS)

test: all
	./demo.native

clean:
	ocamlbuild -clean

install:
	ocamlfind install adafruit-lcd META \
	  _build/lcd.a _build/lcd.cmi _build/lcd.cmx _build/lcd.cmxs \
	  _build/lcd.cmxa _build/lcd.cma _build/lcd.mli \

uninstall:
	ocamlfind remove adafruit-lcd
