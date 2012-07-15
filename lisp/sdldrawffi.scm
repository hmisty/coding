;;; by http://www.animal-machine.com/blog/2010/07/brief-ffi-tutorial-for-gambit-scheme-and-sdl/
(c-declare "#include \"SDL.h\"")
(c-declare "#include \"SDL_draw.h\"") ;;; MUST explicitly include!!! otherwise FFI won't work
(c-declare "#include \"SDL_video.h\"")
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
(define sdl-map-rgb (c-lambda ((pointer "SDL_PixelFormat") int int int) unsigned-int32 "SDL_MapRGB"))

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
(define sdl-draw-pixel (c-lambda ((pointer "SDL_Surface") int int unsigned-int32) void "Draw_Pixel"))
(define sdl-draw-line (c-lambda ((pointer "SDL_Surface") int int int int unsigned-int32) void "Draw_Line"))
(define sdl-draw-circle (c-lambda ((pointer "SDL_Surface") int int int unsigned-int32) void "Draw_Circle"))
(define sdl-draw-fillcircle (c-lambda ((pointer "SDL_Surface") int int int unsigned-int32) void "Draw_FillCircle"))
(define sdl-draw-hline (c-lambda ((pointer "SDL_Surface") int int int unsigned-int32) void "Draw_HLine"))
(define sdl-draw-vline (c-lambda ((pointer "SDL_Surface") int int int unsigned-int32) void "Draw_VLine"))
(define sdl-draw-rect (c-lambda ((pointer "SDL_Surface") int int int int unsigned-int32) void "Draw_Rect"))
(define sdl-draw-fillrect (c-lambda ((pointer "SDL_Surface") int int int int unsigned-int32) void "Draw_FillRect"))
(define sdl-draw-ellipse (c-lambda ((pointer "SDL_Surface") int int int int unsigned-int32) void "Draw_Ellipse"))
(define sdl-draw-fillellipse (c-lambda ((pointer "SDL_Surface") int int int int unsigned-int32) void "Draw_FillEllipse"))
(define sdl-draw-round (c-lambda ((pointer "SDL_Surface") int int int int int unsigned-int32) void "Draw_Round"))
(define sdl-draw-fillround (c-lambda ((pointer "SDL_Surface") int int int int int unsigned-int32) void "Draw_FillRound"))

;;; simple program to create a window and display it for five seconds ;;;
(sdl-init sdl-init-everything)
(define screen (sdl-set-video-mode 640 480 32 0))
(pp screen)
(sdl-wm-set-caption "Test" "Test Window!")
;;; Colors ;;;
(define c-white #x00FFFFFF) ;; ARGB
(define c-gray  #x00C8C8C8)
(define c-dgray #x00404040)
(define c-cyan  #x0020FFFF)

(sdl-draw-line screen 100 100 30 0 c-white)
(sdl-draw-line screen 30 0 100 100 c-white)
(sdl-draw-line screen 100 100 30 0 c-white)
(sdl-draw-line screen 30 0 100 100 c-white)
(sdl-draw-line screen 0 0 100 100 c-white)
(sdl-draw-line screen 100 100 300 200 c-white)
;(sdl-draw-line screen 200 300 250 400 (sdl-map-rgb screen-format 128 128 255))
;(sdl-draw-line screen 500 50 600 70 (sdl-map-rgb screen-format 128 255 128))
(sdl-draw-circle screen 150 150 5 c-white)
;(sdl-draw-circle screen 150 150 4 (sdl-map-rgb screen-format 64 64 64))
;(sdl-draw-circle screen 150 150 3 (sdl-map-rgb screen-format 255 0 0))
;(sdl-draw-circle screen 150 150 2 (sdl-map-rgb screen-format 0 255 0))
;(sdl-draw-fillcircle screen 150 150 1 (sdl-map-rgb screen-format 0 0 255))
;(sdl-draw-line screen 500 100 600 120 (sdl-map-rgb screen-format 128 255 128))
;;; omit some more
(sdl-draw-rect screen 398 298 4 4 c-cyan)
;(sdl-draw-fillrect screen 500 200 50 70 (sdl-map-rgb screen-format 64 200 64))
;;; omit some
(sdl-draw-hline screen 10 400 50 c-white)
(sdl-draw-vline screen 60 400 360 c-white)
(sdl-draw-rect screen 500 400 50 50 c-white)
(sdl-draw-pixel screen 510 410 c-white)
;(sdl-draw-pixel screen 520 420 (sdl-map-rgb screen-format 255 0 0))
;(sdl-draw-pixel screen 530 430 (sdl-map-rgb screen-format  0 255 0))
;(sdl-draw-pixel screen 540 440 (sdl-map-rgb screen-format  0 0 255))
(sdl-draw-ellipse screen 100 300 60 30 c-white)
;(sdl-draw-fillellipse screen 300 300 30 60 (sdl-map-rgb screen-format 64 64 200))
;;; omit some
(sdl-draw-round screen 200 20 70 50 10 c-white)
(sdl-draw-fillround screen 390 20 70 50 20 c-cyan)
;(sdl-draw-rect screen 499 199 52 72 (sdl-map-rgb screen-format 255 255 0))
;(sdl-draw-fillrect screen 500 200 50 70 (sdl-map-rgb screen-format 64 200 64))
(sdl-draw-fillcircle screen 500 330 30 c-cyan)

(sdl-update-rect screen 0 0 0 0)
(let loop () 
  (if (sdl-event-loop?) (loop)))
(sdl-quit)

