SDL_CFLAGS = -I/c/MinGW/include/SDL -IC:\\MinGW\\include\\SDL -IC:/MinGW/include/SDL -Dmain=SDL_main -D_GNU_SOURCE=1 -D_REENTRANT
#SDL_LIBS = -L/c/MinGW/lib -LC:\\MinGW\\lib -LC:/MinGW/lib -lSDL_draw -lSDLmain -lSDL -lSDL.dll -L/usr/lib -lmingw32
SDL_LIBS = -L/c/MinGW/lib -LC:\\MinGW\\lib -LC:/MinGW/lib -lSDL_draw -lSDLmain -lSDL.dll -L/usr/lib -lmingw32

#all: square m
all: sdldraw2

clean: clean_square clean_m

sdldraw2: libsdldraw
	gsc -exe 

libsdldraw:
	gsc -link libsdldraw.scm
	gsc -obj -cc-options "$(SDL_CFLAGS)" libsdldraw.c libsdldraw_.c
	ld -shared -static $(SDL_LIBS) libsdldraw_.o libsdldraw.o -o libsdldraw.dll

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

