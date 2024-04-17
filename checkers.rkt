#lang racket

(provide (all-defined-out))

(define w 1)
(define b 2)
(define W 3)
(define B 4)

(struct state (board turn jumper won) #:transparent)
(struct square (row col) #:transparent)
(struct move (from to) #:transparent)

(define start-board (list (list 0 w 0 w 0 w 0 w)
                          (list w 0 w 0 w 0 w 0)
                          (list 0 w 0 w 0 w 0 w)
                          (list 0 0 0 0 0 0 0 0)
                          (list 0 0 0 0 0 0 0 0)
                          (list b 0 b 0 b 0 b 0)
                          (list 0 b 0 b 0 b 0 b)
                          (list b 0 b 0 b 0 b 0)))



(define board2 (list (list 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0)
                     (list 0 0 0 0 w 0 0 0)
                     (list 0 0 0 w 0 b 0 0)
                     (list 0 0 b 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0 0)))
(define start-state (state start-board b #f #f))

(define (piece state square)
	(if (and (>= (square-row square) 0) (< (square-row square) 8) (>= (square-col square) 0) (< (square-col square) 8))
            (list-ref (list-ref (state-board state) (square-row square)) (square-col square))
        -1))

(define (get board row col)
    (if (and (>= row 0) (< row 8) (>= col 0) (< col 8))
        (list-ref (list-ref board row) col)
        -1))

(define (update-board board square val)
    (list-set board (square-row square) (list-set (list-ref board (square-row square)) (square-col square) val)))

(define (opponent player)
    (cond [(equal? player w) b]
	  [(equal? player b) w]
	  [(equal? player W) B]
	  [(equal? player B) W]))

(define (do-jump board move)
  	(if (equal? (abs (- (square-row (move-from move)) (square-row (move-to move)))) 2)
	    (update-board board
			  (square (quotient (+ (square-row (move-from move)) (square-row (move-to move))) 2)
				  (quotient (+ (square-col (move-from move)) (square-col (move-to move))) 2))
			  0)
	    board))

(define (promote-kings board)
  	(let* ([black (list-set board 0 (map (lambda (piece) (if (equal? piece b) B piece)) (list-ref board 0)))])
	      (list-set black 7 (map (lambda (piece) (if (equal? piece w) W piece)) (list-ref black 7)))))

(define (jump? move)
  	(equal? (abs (- (square-row (move-from move)) (square-row (move-to move)))) 2))

(define (try-move the-state move)
  	(if (not (member move (legal-moves the-state)))
	    the-state
	    (let* ([jump (jump? move)]
		   [board (state-board the-state)]
		   [new-board (promote-kings (do-jump (update-board (update-board board 
							     			  (move-to move) 
										  (piece the-state (move-from move)))
								    (move-from move)
								    0)
			       		      move))]
		   [additional-jumps (not (empty? (legal-moves (state new-board (state-turn the-state) (move-to move) #f))))])
	          (state new-board 
			 (if (and jump additional-jumps) (state-turn the-state) (opponent (state-turn the-state))) 
			 (if (and jump additional-jumps) (move-to move) #f)
			 #f)))) 

  ;(if (not (equal? (state-turn the-state) (piece the-state (move-from move))))
;	    the-state
;	    (state (update-board (update-board (state-board the-state) (move-to move) (state-turn the-state)) (move-from move) 0) (state-turn the-state) (state-won the-state))))

(define (reverse-move old-move)
  	(move (square (- 7 (square-row (move-from old-move))) (square-col (move-from old-move)))
	      (square (- 7 (square-row (move-to old-move))) (square-col (move-to old-move)))))

(define (promote piece)
  	(if (equal? piece w)
	    W
	    (if (equal? piece b)
	        B
		piece)))

(define (same-color? piece1 piece2)
  	(equal? (promote piece1) (promote piece2)))

(define (legal-moves game-state)
    (let* ([player (state-turn game-state)]
	   [jumper (state-jumper game-state)]
	   [board (if (equal? player b) (reverse (state-board game-state)) (state-board game-state))]
           [right-jumps (for/list ([r (range 8)]
                                   #:when #t
                                   [c (range 8)]
                                   #:when (and (same-color? (get board r c) player) 
                                               (equal? (get board (+ r 2) (+ c 2)) 0)
                                               (same-color? (get board (+ r 1) (+ c 1)) (opponent player))))
                                  (move (square r c) (square (+ r 2) (+ c 2))))]
           [left-jumps (for/list ([r (range 8)]
                                  #:when #t
                                  [c (range 8)]
                                  #:when (and (same-color? (get board r c) player) 
                                              (equal? (get board (+ r 2) (- c 2)) 0)
                                              (same-color? (get board (+ r 1) (- c 1)) (opponent player))))
                                 (move (square r c) (square (+ r 2) (- c 2))))]

	   [king-left-jumps (for/list ([r (range 8)]
                                  #:when #t
                                  [c (range 8)]
                                  #:when (and (equal? (get board r c) (promote player)) 
                                              (equal? (get board (- r 2) (- c 2)) 0)
                                              (same-color? (get board (- r 1) (- c 1)) (opponent player))))
                                 (move (square r c) (square (- r 2) (- c 2))))]
	   [king-right-jumps (for/list ([r (range 8)]
                                  #:when #t
                                  [c (range 8)]
                                  #:when (and (equal? (get board r c) (promote player)) 
                                              (equal? (get board (- r 2) (+ c 2)) 0)
                                              (same-color? (get board (- r 1) (+ c 1)) (opponent player))))
                                 (move (square r c) (square (- r 2) (+ c 2))))]
           [king-right-steps (if (equal? 0 (length (append left-jumps right-jumps king-left-jumps king-right-jumps)))
                            (for/list ([r (range 8)]
                                       #:when #t
                                       [c (range 8)]
                                       #:when (and (equal? (get board r c) (promote player)) 
						   (equal? (get board (- r 1) (+ c 1)) 0)))
                                      (move (square r c) (square (- r 1) (+ c 1))))
                            (list))]
           [king-left-steps (if (equal? 0 (length (append left-jumps right-jumps king-left-jumps king-right-jumps)))
                           (for/list ([r (range 8)]
                                      #:when #t
                                      [c (range 8)]
                                      #:when (and (equal? (get board r c) (promote player)) 
						  (equal? (get board (- r 1) (- c 1)) 0)))
                                     (move (square r c) (square (- r 1) (- c 1))))
                           (list))]
	   [right-steps (if (equal? 0 (length (append left-jumps right-jumps king-left-jumps king-right-jumps)))
                            (for/list ([r (range 8)]
                                       #:when #t
                                       [c (range 8)]
                                       #:when (and (same-color? (get board r c) player) (equal? (get board (+ r 1) (+ c 1)) 0)))
                                      (move (square r c) (square (+ r 1) (+ c 1))))
                            (list))]
           [left-steps (if (equal? 0 (length (append left-jumps right-jumps king-left-jumps king-right-jumps)))
                           (for/list ([r (range 8)]
                                      #:when #t
                                      [c (range 8)]
                                      #:when (and (same-color? (get board r c) player) (equal? (get board (+ r 1) (- c 1)) 0)))
                                     (move (square r c) (square (+ r 1) (- c 1))))
                           (list))]
           [all-moves (if (equal? player b) 
              (map reverse-move (append right-jumps left-jumps right-steps left-steps king-left-jumps king-right-jumps king-left-steps king-right-steps))
              (append right-jumps left-jumps right-steps left-steps king-right-jumps king-left-jumps king-left-steps king-right-steps))]
	   [jumper-moves (if jumper
			     (filter (lambda (move) (and (jump? move) (equal? (move-from move) jumper))) all-moves)
			     all-moves)])
      jumper-moves))

(define (print-board board)
    (begin (for ([row board])
                (begin (display row)
                       (display "\n")))
           (display "\n")))

(define (print-moves moves)
    (for ([board moves])
         (print-board board)))

