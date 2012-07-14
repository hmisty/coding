(c-declare #<<end-of-c-declare

#include "SDL.h"
#include "SDL_opengl.h"
#include "SDL_image.h"

end-of-c-declare
)

;;; SDL Constants ;;;
(define SDL-INIT-TIMER       #x00000001)
(define SDL-INIT-AUDIO       #x00000010)
(define SDL-INIT-VIDEO       #x00000020)
(define SDL-INIT-CDROM       #x00000100)
(define SDL-INIT-JOYSTICK    #x00000200)
(define SDL-INIT-NOPARACHUTE #x00100000)
(define SDL-INIT-EVENTTHREAD #x01000000)
(define SDL-INIT-EVERYTHING  #x0000FFFF)

(define SDL-SWSURFACE        #x00000000)
(define SDL-HWSURFACE        #x00000001)
(define SDL-ASYNCBLIT        #x00000004)
(define SDL-ANYFORMAT        #x10000000)
(define SDL-HWPALETTE        #x20000000)
(define SDL-DOUBLEBUF        #x40000000)
(define SDL-FULLSCREEN       #x80000000)
(define SDL-OPENGL           #x00000002)
(define SDL-OPENGLBLIT       #x0000000A)
(define SDL-RESIZABLE        #x00000010)
(define SDL-NOFRAME          #x00000020)


; SDL_Surface accessors
(define sdl-surface->a-mask (c-lambda ((pointer "SDL_Surface")) unsigned-int32 "___result = ___arg1->format->Amask;"))
(define sdl-surface->bpp (c-lambda ((pointer "SDL_Surface")) unsigned-int8 "___result = ___arg1->format->BytesPerPixel;"))
(define sdl-surface->w (c-lambda ((pointer "SDL_Surface")) int32 "___result = ___arg1->w;"))
(define sdl-surface->h (c-lambda ((pointer "SDL_Surface")) int32 "___result = ___arg1->h;"))
(define sdl-surface->pixels (c-lambda ((pointer "SDL_Surface")) (pointer void) "___result_voidstar = ___arg1->pixels;"))



;;; SDL Functions ;;;
(define sdl-quit (c-lambda () void "SDL_Quit"))
(define sdl-init (c-lambda (unsigned-int32) int "SDL_Init"))
(define sdl-set-video-mode (c-lambda (int int int unsigned-int32) (pointer "SDL_Surface") "SDL_SetVideoMode"))
(define sdl-wm-set-caption (c-lambda (char-string char-string) void "SDL_WM_SetCaption"))
(define sdl-delay (c-lambda (unsigned-int32) void "SDL_Delay"))
(define sdl-gl-swap-buffers (c-lambda () void "SDL_GL_SwapBuffers"))
(define sdl-free-surface (c-lambda ((pointer "SDL_Surface")) void "SDL_FreeSurface"))
(define sdl-lock-surface (c-lambda ((pointer "SDL_Surface")) int32 "SDL_LockSurface"))
(define sdl-unlock-surface (c-lambda ((pointer "SDL_Surface")) void "SDL_UnlockSurface"))


;;; SDL_image ;;;
(define sdl-img-load (c-lambda (char-string) (pointer "SDL_Surface") "IMG_Load"))

;;; GL Types ;;;
(c-define-type GLbitfield unsigned-int)
(c-define-type GLclampf float)
(c-define-type GLenum unsigned-int)
(c-define-type GLdouble double)
(c-define-type GLfloat float)
(c-define-type GLint int)
(c-define-type GLuint unsigned-int)
(c-define-type GLuint* (pointer unsigned-int))
(c-define-type GLsizei int)
(c-define-type GLvoid void)
(c-define-type GLvoid* (pointer GLvoid))


;;; GL Constants ;;;
(define GL_NO_ERROR              #x0000)
(define GL_QUADS                 #x0007)
(define GL_MODELVIEW             #x1700)
(define GL_PROJECTION            #x1701)
(define GL_COLOR_BUFFER_BIT  #x00004000)
(define GL_UNPACK_ALIGNMENT      #x0CF5)
(define GL_ALPHA_TEST		 #x0BC0)
(define GL_GREATER		 #x0204)
(define GL_TEXTURE_2D		 #x0DE1)
(define GL_RGB			 #x1907)
(define GL_RGBA			 #x1908)
(define GL_UNSIGNED_BYTE	 #x1401)
(define GL_TEXTURE_MAG_FILTER	 #x2800)
(define GL_TEXTURE_MIN_FILTER	 #x2801)
(define GL_LINEAR		 #x2601)


;;; GL Functions ;;;
(define gl-clear-color (c-lambda (GLclampf GLclampf GLclampf GLclampf) void "glClearColor"))
(define gl-matrix-mode (c-lambda (GLenum) void "glMatrixMode"))
(define gl-load-identity (c-lambda () void "glLoadIdentity"))
(define gl-ortho (c-lambda (GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble) void "glOrtho"))
(define gl-get-error (c-lambda () GLenum "glGetError"))
(define gl-translate-f (c-lambda (GLfloat GLfloat GLfloat) void "glTranslatef"))
(define gl-clear (c-lambda (GLbitfield) void "glClear"))
(define gl-begin (c-lambda (GLenum) void "glBegin"))
(define gl-color-4f (c-lambda (GLfloat GLfloat GLfloat GLfloat) void "glColor4f"))
(define gl-vertex-3f (c-lambda (GLfloat GLfloat GLfloat) void "glVertex3f"))
(define gl-end (c-lambda () void "glEnd"))
(define gl-pixel-store-i (c-lambda (GLenum GLint) void "glPixelStorei"))
(define gl-enable (c-lambda (GLenum) void "glEnable"))
(define gl-alpha-func (c-lambda (GLenum GLclampf) void "glAlphaFunc"))
(define gl-bind-texture (c-lambda (GLenum GLuint) void "glBindTexture"))
(define gl-tex-image-2d (c-lambda (GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum GLvoid*) void "glTexImage2D"))
(define gl-tex-parameter-i (c-lambda (GLenum GLenum GLint) void "glTexParameteri"))
(define gl-tex-coord-2f (c-lambda (GLfloat GLfloat) void "glTexCoord2f"))


; uses glGenTextures and allocates 1 texture, returning the int
(define gen-texture (c-lambda () unsigned-int
                       "GLuint texture; glGenTextures(1,&texture); ___result=texture;"))

;;; Sample Code ;;;

(define (init-gl)
  (gl-enable GL_ALPHA_TEST)
  (gl-alpha-func GL_GREATER 0.5)
  (gl-enable GL_TEXTURE_2D)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-ortho 0.0 640.0 480.0 0.0 -1.0 1.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  (if (equal? (gl-get-error) GL_NO_ERROR)
     #t
     #f))

(define (draw-square x y tex)
  (gl-translate-f x y 0.0)
  (gl-bind-texture GL_TEXTURE_2D tex)
  (gl-begin GL_QUADS)
  (gl-color-4f 1.0 1.0 1.0 1.0)

  (gl-tex-coord-2f 0.0 0.0)
  (gl-vertex-3f 0.0    0.0    0.0)

  (gl-tex-coord-2f 1.0 0.0)
  (gl-vertex-3f 100.0  0.0    0.0)
  
  (gl-tex-coord-2f 1.0 1.0)
  (gl-vertex-3f 100.0  100.0  0.0)

  (gl-tex-coord-2f 0.0 1.0)
  (gl-vertex-3f 0.0    100.0  0.0)
  
  (gl-end)
  (gl-load-identity))


(sdl-init SDL-INIT-EVERYTHING)
(sdl-set-video-mode 640 480 32 SDL-OPENGL)
(sdl-wm-set-caption "OpenGL Test" "Test Window!")

(if (not (init-gl))
    (print "Error while initializing OpenGL.\n"))

; load resources
(define test-surface (sdl-img-load "./water-128x128.png"))
(gl-pixel-store-i GL_UNPACK_ALIGNMENT 4)
(define texture (gen-texture))
(gl-bind-texture GL_TEXTURE_2D texture)
(sdl-lock-surface test-surface)
; assumes a 4 bpp alphamapped image
(gl-tex-image-2d GL_TEXTURE_2D 0 4 (sdl-surface->w test-surface) (sdl-surface->h test-surface) 0
                 GL_RGBA GL_UNSIGNED_BYTE (sdl-surface->pixels test-surface))

        
(gl-tex-parameter-i GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(gl-tex-parameter-i GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(sdl-unlock-surface test-surface)
(sdl-free-surface test-surface)

(let game-loop ()
  (gl-clear GL_COLOR_BUFFER_BIT)
  (draw-square 40.0 40.0 texture)
  (draw-square 140.0 40.0 texture)
  (sdl-gl-swap-buffers)
  (sdl-delay (floor 1000/60))
  (game-loop))

(sdl-quit)
