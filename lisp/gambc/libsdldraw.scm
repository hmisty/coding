;;; by http://www.animal-machine.com/blog/2010/07/brief-ffi-tutorial-for-gambit-scheme-and-sdl/
(c-declare #<<EOL
#include "SDL.h"
#include "SDL_draw.h"
#include "SDL_video.h"
#include "SDL_events.h"
#include "SDL_timer.h"

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

;;; SDL types
(c-define-type sdl-surface (pointer "SDL_Surface"))
(c-define-type sdl-pixel-format (pointer "SDL_PixelFormat"))

;;; SDL Functions ;;;
(define sdl-quit (c-lambda () void "SDL_Quit"))
(define sdl-init (c-lambda (unsigned-int32) int "SDL_Init"))
(define sdl-set-video-mode (c-lambda (int int int unsigned-int32) sdl-surface "SDL_SetVideoMode"))
(define sdl-wm-set-caption (c-lambda (char-string char-string) void "SDL_WM_SetCaption"))
(define sdl-update-rect (c-lambda (sdl-surface int int unsigned-int32 unsigned-int32) void "SDL_UpdateRect"))
(define sdl-map-rgb (c-lambda ((pointer "SDL_PixelFormat") int int int) unsigned-int32 "SDL_MapRGB"))

;;; $ grep SDL_GetTicks *
;;; SDL_timer.h:extern DECLSPEC Uint32 SDLCALL SDL_GetTicks(void);
(define sdl-get-ticks (c-lambda () unsigned-int32 "SDL_GetTicks"))

(define sdl-event-loop? (c-lambda () bool "sdl_event_loop"))
(define sdl-surface->format (c-lambda (sdl-surface) sdl-pixel-format "___result = ___arg1->format;"))

;;; SDL_draw Functions ;;;
(define sdl-draw-pixel (c-lambda (sdl-surface int int unsigned-int32) void "Draw_Pixel"))
(define sdl-draw-line (c-lambda (sdl-surface int int int int unsigned-int32) void "Draw_Line"))
(define sdl-draw-circle (c-lambda (sdl-surface int int int unsigned-int32) void "Draw_Circle"))
(define sdl-draw-fillcircle (c-lambda (sdl-surface int int int unsigned-int32) void "Draw_FillCircle"))
(define sdl-draw-hline (c-lambda (sdl-surface int int int unsigned-int32) void "Draw_HLine"))
(define sdl-draw-vline (c-lambda (sdl-surface int int int unsigned-int32) void "Draw_VLine"))
(define sdl-draw-rect (c-lambda (sdl-surface int int int int unsigned-int32) void "Draw_Rect"))
(define sdl-draw-fillrect (c-lambda (sdl-surface int int int int unsigned-int32) void "Draw_FillRect"))
(define sdl-draw-ellipse (c-lambda (sdl-surface int int int int unsigned-int32) void "Draw_Ellipse"))
(define sdl-draw-fillellipse (c-lambda (sdl-surface int int int int unsigned-int32) void "Draw_FillEllipse"))
(define sdl-draw-round (c-lambda (sdl-surface int int int int int unsigned-int32) void "Draw_Round"))
(define sdl-draw-fillround (c-lambda (sdl-surface int int int int int unsigned-int32) void "Draw_FillRound"))

