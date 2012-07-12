;;;; tic-tac-toe
;;;; hmisty 2012/7/12
;;;; 2 hrs quick hacking :)
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
(for (col min_ max_ step) (gs:draw-line 'HL min_ col max_ col gs:black))

;;;; make the pairs from a lst
(define (pairs lst)
  (if (rest lst)
    (append (map (lambda (x) (list (first lst) x)) (rest lst))
            (pairs (rest lst)))
  ))

;;;; get chess-triplets
(define (chess-triplets newchess chess-pairs)
  (println (string "chess-pairs: " chess-pairs))
  (map (lambda (x) (cons newchess x)) chess-pairs)
  )

;;;; if-line-up three chesses
(define (if-line-up chess1 chess2 chess3)
    (or (= (chess1 0) (chess2 0) (chess3 0))
        (= (chess1 1) (chess2 1) (chess3 1))
        (= (intersect '((0 0) (1 1) (2 2)) (list chess1 chess2 chess3)) '((0 0) (1 1) (2 2)))
        (= (intersect '((0 2) (1 1) (2 0)) (list chess1 chess2 chess3)) '((0 2) (1 1) (2 0)))
        )
  )

;;;; if-win triplets lst
(define (if-win chess-triplets)
  (println (string "if-line-up: " chess-triplets))
  (ref 'true (map (lambda (c) (if-line-up (c 0) (c 1) (c 2))) chess-triplets))
  )

;;;; judge the winner
(define (winner oldchesses newchess)
  (println (string "newchess: " newchess " oldchesses: " oldchesses))
  (if-win (chess-triplets newchess (pairs oldchesses))))

;;;; define draw
;; draw 0 = draw a circle; draw 1 = draw a cross
(set 'what 0)
(set 'chessO '()) ;;; O chesses already put. (row col)
(set 'chessX '()) ;;; X chesses already put. (row col)
(define (handler ev n) nil)

(define (win name)
    (gs:confirm-dialog 'TicTacToe 'handler "Win!" (string name " win!") "yes-no")
    (gs:delete-tag 'X)
    (gs:delete-tag 'O)
    (set 'what 0)
    (set 'chessO '())
    (set 'chessX '())
  )

(define (draw row col)
  (if (= (ref (list row col) (append chessO chessX)) nil)
    (draw-chess row col)
    (gs:confirm-dialog 'TicTacToe 'handler "Error" "Cannot put the chess here" "yes-no")))

(define (draw-chess row col)
  (if (= what 0) (draw-circle row col) (draw-cross row col))
  (set 'what (- 1 what)))

(define (draw-circle row col)
  (set 'x (+ (+ (* col step) (/ step 2)) min_) 'y (+ (+ (* row step) (/ step 2)) min_) 'r (/ step 2))
  (println (string "draw circle: x:" x "y:" y))
  (gs:draw-circle 'O x y r gs:red)
  (gs:show-tag 'O)
  ;;;; side-effect
  (set 'newchess (list row col))
  (if (winner chessO newchess) (win "O") (push newchess chessO))
  )

(define (draw-cross row col)
  (set 'x0 (+ (* col step) min_) 'y0 (+ (* row step) min_)
       'x1 (+ (+ (* col step) step) min_) 'y1 (+ (+ (* row step) step) min_))
  (println (string "draw line: x0:" x0 "y0:" y0 "x1:" x1 "y1:" y1))
  (gs:draw-line 'X x0 y0 x1 y1 gs:blue)
  (gs:draw-line 'X x0 y1 x1 y0 gs:blue)
  (gs:show-tag 'X)
  ;;;; side-effect
  (set 'newchess (list row col))
  (if (winner chessX newchess) (win "X") (push newchess chessX))
  )

;;;; define actions
(define (mouse-clicked-action x y button cnt modifiers tags)
  (set 'row (/ (- y min_) step) 'col (/ (- x min_) step))
  (gs:set-text 'TicTacToe 
               (string "clicked row: " row " col:" col
                       " button: " button " count:" cnt " key:" modifiers " tags:" tags))
  (draw row col)
  )

(gs:listen)

;(exit)
