#lang racket

(require "common.rkt")

(provide day8 day8-exe)

(define (parse-grid input)
  (define rows (length input))
  (define columms (string-length (first input)))
  (for*/fold ([grid (make-2d-grid columms rows)]) ([(line y) (in-indexed input)] [(tile x) (in-indexed line)])
    (define height (string->number (string tile)))
    (2d-grid-set grid (2d-cord x y) height)))

(define (day8-part1 grid)
  (define seen-from-left
    (for/fold ([seen-grid (make-2d-grid (2d-grid-columms grid) (2d-grid-rows grid))])
              ([y (in-range (2d-grid-rows grid))])
      (for*/fold ([seen-grid seen-grid] [height -1] #:result seen-grid)
                 ([x (in-range (2d-grid-columms grid))]
                  [cords (in-value (2d-cord x y))]
                  [tile (in-value (2d-grid-ref grid cords))]
                  #:when (> tile height))
        (values (2d-grid-set seen-grid cords #t) tile))))
  (define seen-from-right
    (for/fold ([seen-grid seen-from-left])
              ([y (in-range (2d-grid-rows grid))])
      (for*/fold ([seen-grid seen-grid] [height -1] #:result seen-grid)
                 ([x (in-inclusive-range (- (2d-grid-columms grid) 1) 0 -1)]
                  [cords (in-value (2d-cord x y))]
                  [tile (in-value (2d-grid-ref grid cords))]
                  #:when (> tile height))
        (values (2d-grid-set seen-grid cords #t) tile))))
  (define seen-from-top
    (for/fold ([seen-grid seen-from-right])
              ([x (in-range (2d-grid-columms grid))])
      (for*/fold ([seen-grid seen-grid] [height -1] #:result seen-grid)
                 ([y (in-range (2d-grid-rows grid))]
                  [cords (in-value (2d-cord x y))]
                  [tile (in-value (2d-grid-ref grid cords))]
                  #:when (> tile height))
        (values (2d-grid-set seen-grid cords #t) tile))))
  (define seen-from-bottom
    (for/fold ([seen-grid seen-from-top])
              ([x (in-range (2d-grid-columms grid))])
      (for*/fold ([seen-grid seen-grid] [height -1] #:result seen-grid)
                 ([y (in-inclusive-range (- (2d-grid-rows grid) 1) 0 -1)]
                  [cords (in-value (2d-cord x y))]
                  [tile (in-value (2d-grid-ref grid cords))]
                  #:when (> tile height))
        (values (2d-grid-set seen-grid cords #t) tile))))
  (for*/sum ([y (in-range (2d-grid-rows seen-from-bottom))]
             [x (in-range (2d-grid-columms seen-from-bottom))]
             [cords (in-value (2d-cord x y))]
             [tile (in-value (2d-grid-ref seen-from-bottom cords))])
    (if tile 1 0)))

(define (day8-part2 grid)
  (for*/fold ([best-score 0])
             ([y (in-range (2d-grid-rows grid))]
              [x (in-range (2d-grid-columms grid))]
              [height (in-value (2d-grid-ref grid (2d-cord x y)))])
    (max
     best-score
     (* (for*/sum ([x (in-range (+ 1 x) (2d-grid-columms grid))]
                   [cords (in-value (2d-cord x y))]
                   [tile (in-value (2d-grid-ref grid cords))]
                   #:final (>= tile height))
          1)
        (for*/sum ([x (in-inclusive-range (- x 1) 0 -1)]
                   [cords (in-value (2d-cord x y))]
                   [tile (in-value (2d-grid-ref grid cords))]
                   #:final (>= tile height))
          1)
        (for*/sum ([y (in-range (+ 1 y) (2d-grid-rows grid))]
                   [cords (in-value (2d-cord x y))]
                   [tile (in-value (2d-grid-ref grid cords))]
                   #:final (>= tile height))
          1)
        (for*/sum ([y (in-inclusive-range (- y 1) 0 -1)]
                   [cords (in-value (2d-cord x y))]
                   [tile (in-value (2d-grid-ref grid cords))]
                   #:final (>= tile height))
          1)))))

(define (day8 input)
  (define grid (parse-grid (line-split input)))
  (list (day8-part1 grid) (day8-part2 grid)))

(define (day8-exe) (exe-day day8 "input/Day8.txt"))