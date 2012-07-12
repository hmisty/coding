clean: clean_square clean_m

clean_square:
	rm -f a.out square_.c square.c

square:
	gsc -c square.scm
	gsc -link square.c
	gcc square.c square_.c -lgambc -lm -ldl -lutil

clean_m:
	cd m
	rm -f a.out m3_.c m3.c m2.c

m:
	cd m
	gsc -c m2.scm
	gsc -c m3.scm
	gsc -link m2.c m3.c
	gcc m1.c m2.c m3.c m3_.c -lgambc -lm -ldl -lutil
