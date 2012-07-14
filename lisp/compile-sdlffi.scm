;;; Compile sdlffi

(define (compile-sdl-file file)
  (let ( (file-name (string-append file ".scm")) )
     (newline)
     (display "COMPILING: ") (display file-name)
     (newline)
     (compile-file file-name
                   options: '(exe) ;;'(report debug)
                   cc-options: "-IC:/MinGW/include/ -D_GNU_SOURCE=1 -D_REENTRANT"
                   ld-options: "-LC:/MinGW/lib -lSDL")
) )

(define (load-file file)
  (display "LOADING:   ") (display file)
  (newline)
  (load file))

(begin
  ;(compile-sdl-file "sdl-interface")
  ;(load-file "sdl-interface")
  (compile-sdl-file "sdlffi")
)

(exit)

;;		--- E O F ---		;;
