all: i2c-dev.so
	

i2c-dev.so: i2c-dev.c
	gcc -g -fPIC -shared -o $@ $<

clean:
	rm -f i2c-dev.so
