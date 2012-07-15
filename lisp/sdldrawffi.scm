;;; by http://www.animal-machine.com/blog/2010/07/brief-ffi-tutorial-for-gambit-scheme-and-sdl/
(c-declare "#include \"SDL.h\"")
(c-declare "#include \"SDL_draw.h\"") ;;; MUST explicitly include!!! otherwise FFI won't work
(c-declare "#include \"SDL_events.h\"")
(c-declare "#include \"SDL_timer.h\"")

;;; SDL Constants ;;;
(define sdl-init-timer       #x00000001)
(define sdl-init-audio       #x00000010)
(define sdl-init-video       #x00000020)
(define sdl-init-cdrom       #x00000100)
(define sdl-init-joystick    #x00000200)
(define sdl-init-noparachute #x00100000)
(define sdl-init-eventthread #x01000000)
(define sdl-init-everything  #x0000FFFF)

;;; SDL_events.h
(define SDL_KEYDOWN 2)
(define SDL_QUIT 12)

;;; SDL Functions ;;;
(define sdl-quit (c-lambda () void "SDL_Quit"))
(define sdl-init (c-lambda (unsigned-int32) int "SDL_Init"))
(define sdl-set-video-mode (c-lambda (int int int unsigned-int32) (pointer "SDL_Surface") "SDL_SetVideoMode"))
(define sdl-wm-set-caption (c-lambda (char-string char-string) void "SDL_WM_SetCaption"))
(define sdl-update-rect (c-lambda ((pointer "SDL_Surface") int int unsigned-int32 unsigned-int32) void "SDL_UpdateRect"))
;;; $ grep SDL_GetTicks *
;;; SDL_timer.h:extern DECLSPEC Uint32 SDLCALL SDL_GetTicks(void);
(define sdl-get-ticks (c-lambda () unsigned-int32 "SDL_GetTicks"))

(c-declare #<<EOL
/* exit -> 0 else 1 */
int sdl_event_loop()
{
    // Poll for events, and handle Quit.
    SDL_Event event;
    while (SDL_PollEvent(&event))
    {
        switch (event.type)
        {
            case SDL_KEYDOWN:
                break;
            case SDL_KEYUP:
                // If escape is pressed, return (and thus, quit)
                if (event.key.keysym.sym == SDLK_ESCAPE)
                    return(0);
                break;
            case SDL_QUIT:
                return(0);
            default:
                break;
        } /* end switch */
    } /* end while */

    return(1);
}
EOL
) ;;; DONOT write EOL) !!!
(define sdl-event-loop? (c-lambda () bool "sdl_event_loop"))

;;; SDL_draw Functions ;;;
(define sdl-draw-line (c-lambda ((pointer "SDL_Surface") int int int int unsigned-int32) void "Draw_Line"))

;;; Colors ;;;
(define c-white #x00FFFFFF) ;; ARGB

;;; simple program to create a window and display it for five seconds ;;;

(sdl-init sdl-init-everything)
(define screen (sdl-set-video-mode 640 480 32 0))
(sdl-wm-set-caption "Test" "Test Window!")
(sdl-draw-line screen 100 100 30 0 c-white)
(sdl-update-rect screen 0 0 0 0)
(let loop () 
  (if (sdl-event-loop?) (loop)))
(sdl-quit)

