all: m

clean: clean_m

clean_m:
	rm -f a.out m3_.c m3.c m2.c

.PHONY: m
m:
	gsc -c m2.scm
	gsc -c m3.scm
	gsc -link m2.c m3.c
	# mingw-get install libtool. still cannot find -ldl and -lutil
	gcc -o a.out m1.c m2.c m3.c m3_.c -lgambc -lm -ldl -lutil
	a.out

