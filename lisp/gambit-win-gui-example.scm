;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2011-March/004939.html
;; File: "windows-gui-example.scm"

;; On Windows, compile and run with:
;;
;;   $ gsc -exe windows-gui-example.scm
;;   $ ./windows-gui-example.exe

;;-----------------------------------------------------------------------------

;; Win32 types, constants and functions.

(c-define-type HWND "HWND")
(c-define-type WPARAM "WPARAM")
(c-define-type LPARAM "LPARAM")

(define-macro (import-int-constants . names)
  `(begin
     ,@(map (lambda (name)
              `(define ,name
                 ((c-lambda () int ,(string-append "___result = " (symbol->string name) ";")))))
            names)))

(import-int-constants
 WM_DESTROY
 WM_CLOSE
)

(define DefWindowProc
  (c-lambda (HWND unsigned-int WPARAM LPARAM) long "DefWindowProc"))

(define DestroyWindow
  (c-lambda (HWND) bool "DestroyWindow"))

(define PostQuitMessage
  (c-lambda (int) void "PostQuitMessage"))

;;-----------------------------------------------------------------------------

;; Utility functions to create windows and run the message loop.

(c-declare #<<end-of-c-declare

#include <windows.h>

/* The window procedure which handles events is defined in Scheme */

long CALLBACK window_procedure(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

HWND create_window(char *window_class, char *title, int width, int height)
{
  WNDCLASSEX wc;
  HWND hwnd;

  /* Get the program startup information which was passed to WinMain */

  HINSTANCE hInstance;
  HINSTANCE hPrevInstance;
  LPSTR lpCmdLine;
  int nCmdShow;

  ___program_startup_info_struct *psi = ___EXT(___get_program_startup_info)();

  hInstance     = psi->hInstance;
  hPrevInstance = psi->hPrevInstance;
  lpCmdLine     = psi->lpCmdLine;
  nCmdShow      = psi->nCmdShow;

  /* Register the Window Class */

  wc.cbSize        = sizeof(WNDCLASSEX);
  wc.style         = 0;
  wc.lpfnWndProc   = window_procedure;
  wc.cbClsExtra    = 0;
  wc.cbWndExtra    = 0;
  wc.hInstance     = hInstance;
  wc.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
  wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
  wc.lpszMenuName  = NULL;
  wc.lpszClassName = window_class;
  wc.hIconSm       = LoadIcon(NULL, IDI_APPLICATION);

  if (!RegisterClassEx(&wc))
    return NULL;

  /* Create the window */

  hwnd = CreateWindowEx(
           WS_EX_CLIENTEDGE,
           window_class,
           title,
           WS_OVERLAPPEDWINDOW,
           CW_USEDEFAULT, CW_USEDEFAULT, width, height,
           NULL, NULL, hInstance, NULL);

  if (hwnd == NULL)
    return NULL;

  /* Show the window */

  ShowWindow(hwnd, nCmdShow);
  UpdateWindow(hwnd);

  return hwnd;
}

int message_loop()
{
  MSG Msg;

  while (GetMessage(&Msg, NULL, 0, 0) > 0)
  {
    TranslateMessage(&Msg);
    DispatchMessage(&Msg);
  }

  return Msg.wParam;
}

end-of-c-declare
)

(define create-window
  (c-lambda (char-string char-string int int) HWND "create_window"))

(define message-loop
  (c-lambda () int "message_loop"))

(c-define (window-procedure hwnd msg wparam lparam) (HWND unsigned-int WPARAM LPARAM) long "window_procedure" "CALLBACK"

  ;; (pp msg)

  (cond ((= msg WM_CLOSE)
         (DestroyWindow hwnd)
         0)
        ((= msg WM_DESTROY)
         (PostQuitMessage 0)
         0)
        (else
         (DefWindowProc hwnd msg wparam lparam))))

;;-----------------------------------------------------------------------------

;; Main program.  Open a window which can be moved, resized and closed.

(define hwnd
  (create-window "mywindowclass" "My Window" 500 200))

(message-loop) ;; run the message loop, which calls "window-procedure"

;;-----------------------------------------------------------------------------
