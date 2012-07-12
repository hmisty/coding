all: square m

clean: clean_square clean_m

clean_square:
	rm -f square square_.c square.c

square:
	gsc -c square.scm
	gsc -link square.c
	gcc -o square square.c square_.c -lgambc -lm -ldl -lutil
	./square 30

clean_m:
	rm -f m/a.out m/m3_.c m/m3.c m/m2.c

.PHONY: m
m:
	gsc -c m/m2.scm
	gsc -c m/m3.scm
	gsc -link m/m2.c m/m3.c
	gcc -o m/a.out m/m1.c m/m2.c m/m3.c m/m3_.c -lgambc -lm -ldl -lutil
	m/a.out

