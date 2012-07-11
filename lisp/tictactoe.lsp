;;;; tic-tac-toe
;;;; hmisty 2012/7/12
;;;; 2 hrs quick hack :)
#!/usr/bin/newlisp

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

;;;; describe the GUI
(gs:frame 'TicTacToe 100 100 640 640 "The Tic Tac Toe Game by hmisty")
(gs:set-resizable 'TicTacToe nil)
(gs:canvas 'MyCanvas)
(gs:add-to 'TicTacToe 'MyCanvas)
(gs:set-background 'MyCanvas gs:white)
(gs:mouse-clicked 'MyCanvas 'mouse-clicked-action true)
(gs:set-visible 'TicTacToe true)

;;;; draw the grid
(set 'min_ 10 'max_ 610 'step 200)
(for (row min_ max_ step) (gs:draw-line 'VL row min_ row max_ gs:black))
(for (col min_ max_ step)	(gs:draw-line 'HL min_ col max_ col gs:black))

;;;; define draw
;; draw 0 = draw a circle; draw 1 = draw a cross
(set 'what 0)
(set 'chesses '())
(define (handler ev n) nil)
(define (draw row col)
	(if (= (ref (list row col) chesses) nil)
		(draw-chess row col)
		(gs:confirm-dialog 'TicTacToe 'handler "Error" "Cannot put the chess here" "yes-no")))

(define (draw-chess row col)
	(push (list row col) chesses)
	(set	'what (- 1 what)) 
	(if (= what 0) (draw-circle row col) (draw-cross row col)))

(define (draw-circle row col)
	(set 'x (+ (+ (* col step) (/ step 2)) min_)  'y (+ (+ (* row step) (/ step 2)) min_) 'r (/ step 2))
	(print (string "draw circle: x:" x "y:" y))
	(gs:draw-circle 'XO x y r gs:red))
(define (draw-cross row col)
	(set 'x0 (+ (* col step) min_) 'y0 (+ (* row step) min_)
			'x1 (+ (+ (* col step) step) min_) 'y1 (+ (+ (* row step) step) min_))
	(print (string "draw line: x0:" x0 "y0:" y0 "x1:" x1 "y1:" y1))
	(gs:draw-line 'XO x0 y0 x1 y1 gs:blue)
	(gs:draw-line 'XO x0 y1 x1 y0 gs:blue))

;;;; define actions
(define (mouse-clicked-action x y button cnt modifiers tags)
	(set 'row (/ (- y min_) step) 'col (/ (- x min_) step))
	(gs:set-text 'TicTacToe 
		(string "clicked row: " row " col:" col
			" button: " button " count:" cnt " key:" modifiers " tags:" tags))
	(draw row col)
	(gs:show-tag 'XO)
)

(gs:listen)

;(exit)