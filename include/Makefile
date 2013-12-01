all: i2c-dev.so
	

i2c-dev.so: i2c-dev.c
	gcc -fPIC -shared -o $@ $<

clean:
	rm -f i2c-dev.so
