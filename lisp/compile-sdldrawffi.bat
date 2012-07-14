rem gsc  -cc-options "-I/c/MinGW/include/SDL -IC:\\MinGW\\include\\SDL -IC:/MinGW/include/SDL -Dmain=SDL_main -D_GNU_SOURCE=1 -D_REENTRANT" -ld-options "-L/c/MinGW/lib -LC:\\MinGW\\lib -LC:/MinGW/lib -lmingw32 -mwindows -lSDLmain -lSDL -lSDL_draw"  -exe sdldrawffi.scm
rem !!! -lSDL_draw MUST PRECEDE -lSDL !!!!! it sucks!
gsc  -cc-options "-I/c/MinGW/include/SDL -IC:\\MinGW\\include\\SDL -IC:/MinGW/include/SDL -Dmain=SDL_main -D_GNU_SOURCE=1 -D_REENTRANT" -ld-options "-L/c/MinGW/lib -LC:\\MinGW\\lib -LC:/MinGW/lib -lSDL_draw -lSDLmain -lSDL"  -exe sdldrawffi.scm
