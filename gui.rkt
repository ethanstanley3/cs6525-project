#lang racket

(require racket/gui/base)
(require "checkers.rkt")

(define state start-state)
(define req-move (move #f #f))
(define search-depth 5)

(define piece-size 75)

(define (y row)
  	(* piece-size row))
(define (x col)
  	(* piece-size col))

(define (select-square r c)
  	(if (move-from req-move)
	    (begin (set! req-move (move (move-from req-move) (square r c)))
		   (set! state (try-move state req-move))
		   (set! req-move (move #f #f))
		   (send canvas refresh))
	    (set! req-move (move (square r c) #f))))

(define (draw-state canvas dc)
	(for ([pieces (state-board state)]
	      [row (range 0 (length (state-board state)))]) 
	     (draw-row canvas dc pieces row))
	(send turn-indicator set-label (format "~a to move" (if (equal? w (state-turn state)) "White" "Red")))
	;(send utility-indicator set-label (format "~a" (state-utility-for-white state 0)))
	)

(define (ai-move)
        (set! state (try-move state (best-move state search-depth)))
	(send canvas refresh))

(define (draw-row canvas dc pieces row)
	(for ([piece pieces]
	      [col (range 0 (length pieces))])
	     (begin (if (equal? 0 (modulo (+ row col) 2))
		      	(send dc set-brush "white" 'solid)
			(send dc set-brush "black" 'solid))
		    (when (and (move-from req-move) (equal? row (square-row (move-from req-move))) (equal? (square-col (move-from req-move)) col))
		      	  (send dc set-brush "gray" 'solid))
	            (send dc draw-rectangle (x col) (y row) piece-size piece-size)
		    (when (equal? piece w) (send dc set-brush "white" 'solid) 
		      			  (send dc draw-ellipse (x col) (y row) piece-size piece-size))
		    (when (equal? piece b) (send dc set-brush "red" 'solid) 
		      			  (send dc draw-ellipse (x col) (y row) piece-size piece-size))
		    (when (equal? piece W) (send dc set-brush "white" 'solid)
		      			   (send dc draw-ellipse (x col) (y row) piece-size piece-size)
					   (send dc set-brush "black" 'solid)
					   (send dc draw-ellipse (x (+ 0.25 col)) (y (+ 0.25 row)) (* 0.5 piece-size) (* 0.5 piece-size)))
		    (when (equal? piece B) (send dc set-brush "red" 'solid)
		      			   (send dc draw-ellipse (x col) (y row) piece-size piece-size)
					   (send dc set-brush "black" 'solid)
					   (send dc draw-ellipse (x (+ 0.25 col)) (y (+ 0.25 row)) (* 0.5 piece-size) (* 0.5 piece-size))))))
	
(define frame (new frame% [label "Example"] [width (* (length (first (state-board state))) piece-size)] [height (* (+ 2 (length (state-board state))) piece-size)]))

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

;(define utility-indicator (new message% [parent frame]
;                          	        [label "alskdajlskdajlds"]))

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

(send canvas refresh)

