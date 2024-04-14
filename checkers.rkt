#lang racket

(define w 1)
(define b 2)
(define W 3)
(define B 4)

(define start-board (list (list 0 w 0 w 0 w 0 w)
                          (list w 0 w 0 w 0 w 0)
                          (list 0 w 0 w 0 w 0 w)
                          (list 0 0 0 0 0 0 0 0)
                          (list 0 0 0 0 0 0 0 0)
                          (list b 0 b 0 b 0 b 0)
                          (list 0 b 0 b 0 b 0 b)
                          (list b 0 b 0 b 0 b 0)))

(define board2 (list (list 0 w 0 w 0 w 0 w)
                     (list w 0 w 0 w 0 w 0)
                     (list 0 w 0 w 0 w 0 w)
                     (list 0 0 0 0 0 0 b 0)
                     (list 0 0 0 0 0 0 0 0)
                     (list b 0 b 0 b 0 b 0)
                     (list 0 b 0 b 0 b 0 b)
                     (list b 0 b 0 b 0 b 0)))

(define (get board row col)
    (if (and (>= row 0) (< row 8) (>= col 0) (< col 8))
        (list-ref (list-ref board row) col)
        -1))

(define (update-board board row col val)
    (list-set board row (list-set (list-ref board row) col val)))

(define (opponent player)
    (- 3 player))

; TO-DO: double jumps, king promotions/moves
(define (legal-moves board player)
    (let* ([board (if (equal? player b) (reverse board) board)]
           [right-jumps (for/list ([r (range 8)]
                                   #:when #t
                                   [c (range 8)]
                                   #:when (and (equal? (get board r c) player) 
                                               (equal? (get board (+ r 2) (+ c 2)) 0)
                                               (equal? (get board (+ r 1) (+ c 1)) (opponent player))))
                                  (update-board (update-board (update-board board r c 0) 
                                                              (+ r 1) 
                                                              (+ c 1) 
                                                              0)
                                                (+ r 2)
                                                (+ c 2)
                                                player))]
           [left-jumps (for/list ([r (range 8)]
                                  #:when #t
                                  [c (range 8)]
                                  #:when (and (equal? (get board r c) player) 
                                              (equal? (get board (+ r 2) (- c 2)) 0)
                                              (equal? (get board (+ r 1) (- c 1)) (opponent player))))
                                 (update-board (update-board (update-board board r c 0) 
                                                             (+ r 1) 
                                                             (- c 1) 
                                                             0)
                                               (+ r 2)
                                               (- c 2)
                                               player))]
           [right-steps (if (equal? 0 (length (append left-jumps right-jumps)))
                            (for/list ([r (range 8)]
                                       #:when #t
                                       [c (range 8)]
                                       #:when (and (equal? (get board r c) player) (equal? (get board (+ r 1) (+ c 1)) 0)))
                                      (update-board (update-board board r c 0) (+ r 1) (+ c 1) player))
                            (list))]
           [left-steps (if (equal? 0 (length (append left-jumps right-jumps)))
                           (for/list ([r (range 8)]
                                      #:when #t
                                      [c (range 8)]
                                      #:when (and (equal? (get board r c) player) (equal? (get board (+ r 1) (- c 1)) 0)))
                                     (update-board (update-board board r c 0) (+ r 1) (- c 1) player))
                           (list))])
          (if (equal? player 2) 
              (map reverse (append right-jumps left-jumps right-steps left-steps))
              (append right-jumps left-jumps right-steps left-steps))))

(define (print-board board)
    (begin (for ([row board])
                (begin (display row)
                       (display "\n")))
           (display "\n")))

(define (print-moves moves)
    (for ([board moves])
         (print-board board)))

(print-moves (legal-moves board2 w))