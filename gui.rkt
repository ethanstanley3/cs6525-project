#lang racket

(require racket/gui/base)
(require "checkers.rkt")

;; MUTABLE STATE
; state of the checkers game
(define state start-state)
; move selected by gui user
(define req-move (move #f #f))
; maximum depth of minimax search
(define search-depth 5)
;; END MUTABLE STATE

; diameter of pieces (pixels)
(define piece-size 75)

(define (y row)
  	(* piece-size row))
(define (x col)
  	(* piece-size col))

; callback when user clicks on the board
; if selected which piece should move, update req-move
; if selected where piece should move, send move to model
(define (select-square r c)
  	(if (move-from req-move)
	    (begin (set! req-move (move (move-from req-move) (square r c)))
		   	   (set! state (try-move state req-move))
		       (set! req-move (move #f #f))
		       (send canvas refresh))
	    (set! req-move (move (square r c) #f))))

; draw the board
; callback for canvas refresh event
(define (draw-state canvas dc)
	(for ([pieces (state-board state)]
	      [row (range 0 (length (state-board state)))]) 
	     (draw-row canvas dc pieces row))
	(send turn-indicator set-label (format "~a to move" (if (equal? w (state-turn state)) "White" "Red"))))

; ask model to compute best move and update the state
(define (ai-move)
	(set! state (try-move state (best-move state search-depth)))
	(send canvas refresh))

(define (draw-row canvas dc pieces row)
	(for ([piece pieces]
	      [col (range 0 (length pieces))])
	     (begin (if (equal? 0 (modulo (+ row col) 2)) ; checkerboard pattern
		      		(send dc set-brush "white" 'solid)
					(send dc set-brush "black" 'solid))
		    	(when (and (move-from req-move)
				           (equal? row (square-row (move-from req-move))) 
						   (equal? (square-col (move-from req-move)) col))
		      	  	  (send dc set-brush "gray" 'solid)) ; highlight selected piece with gray
	            (send dc draw-rectangle (x col) (y row) piece-size piece-size)
		    	(when (equal? piece w) ; white piece
					  (send dc set-brush "white" 'solid) 
		      		  (send dc draw-ellipse (x col) (y row) piece-size piece-size))
		    	(when (equal? piece b) ; red piece
					  (send dc set-brush "red" 'solid) 
		      		  (send dc draw-ellipse (x col) (y row) piece-size piece-size))
		    	(when (equal? piece W) ; white king
					  (send dc set-brush "white" 'solid)
		      		  (send dc draw-ellipse (x col) (y row) piece-size piece-size)
					  (send dc set-brush "black" 'solid)
					  (send dc draw-ellipse (x (+ 0.25 col)) (y (+ 0.25 row)) (* 0.5 piece-size) (* 0.5 piece-size)))
		    	(when (equal? piece B) ; red king
					  (send dc set-brush "red" 'solid)
					  (send dc draw-ellipse (x col) (y row) piece-size piece-size)
					  (send dc set-brush "black" 'solid)
					  (send dc draw-ellipse (x (+ 0.25 col)) (y (+ 0.25 row)) (* 0.5 piece-size) (* 0.5 piece-size))))))

; gui window
(define frame (new frame% [label "Example"] [width (* (length (first (state-board state))) piece-size)] [height (* (+ 2 (length (state-board state))) piece-size)]))

; drawable canvas class
; override draw, keyboard, and mouse event callbacks
(define board-canvas
  (class canvas%
    (inherit get-width get-height refresh)

    (define/override (on-char ch)
		(when (equal? #\a (send ch get-key-code))
		      (ai-move)))

    (define/override (on-event event)
		(when (and (is-a? event mouse-event%) (send event button-down?))
	      	  (let ([r (quotient (send event get-y) piece-size)]
		    		[c (quotient (send event get-x) piece-size)])
		   		   (select-square r c)
		   		   (send canvas refresh))))

    (super-new (paint-callback draw-state))))

(define canvas (new board-canvas (parent frame)))

(define turn-indicator (new message% [parent frame]
                          	     	 [label "aksjdalksdjalksdjal"]))

; text box to control search depth
(define search-depth-control (new text-field% [parent frame]
				  	      [init-value "5"]
					      [label "Search depth"]
					      [callback (lambda (text-field event) (set! search-depth (string->number (send text-field get-value))))]))

(define reset-button (new button% [parent frame]
			  	  [label "Reset game"]
				  [callback (lambda (button event) (set! state start-state)
					      			   (send frame refresh))]))

(define ai-button (new button% [parent frame]
			       [label "AI move"]
			       [callback (lambda (button event) (ai-move))]))

(send frame show #t)