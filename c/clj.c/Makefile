CC=gcc
INC=/System/Library/Frameworks/JavaVM.framework/Headers
LIB=/System/Library/Frameworks/JavaVM.framework/Libraries
PAR=-framework JavaVM
OPT=-g

all: clj

clj: clj.c
	$(CC) $(OPT) -I$(INC) -L$(LIB) $(PAR) -o $@ $<

.PHONY: clean
clean:
	rm -rf clj clj.dSYM
