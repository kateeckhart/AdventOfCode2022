#lang racket

(provide line-split block-split exe-day 2d-cord 2d-cord-x 2d-cord-y 2d-cord-+ 2d-cord-minus 2d-grid make-2d-grid 2d-grid-columms 2d-grid-rows 2d-grid-ref 2d-grid-set)

(define (line-split str)
  (string-split str "\n"))

(define (block-split str)
  (map line-split (string-split str "\n\n")))

(define (exe-day day-func input-loc)
  (define input-file (open-input-file input-loc))
  (define input (port->string input-file))
  (close-input-port input-file)
  (day-func input))

(struct 2d-cord (x y)
  #:transparent
  #:guard (lambda (x y type-name)
            (if (and (integer? x) (exact? x) (integer? y) (exact? y))
                (values x y)
                (error type-name "Cords need to be exact integers!"))))

(define (2d-cord-+-two-arg self other) (2d-cord (+ (2d-cord-x self) (2d-cord-x other)) (+ (2d-cord-y self) (2d-cord-y other))))
(define (2d-cord-+ . args) (foldl 2d-cord-+-two-arg (2d-cord 0 0) args))
(define (2d-cord-minus-one-arg self) (2d-cord (- (2d-cord-x self)) (- (2d-cord-y self))))
(define (2d-cord-minus-two-arg self other) (2d-cord-+ self (2d-cord-minus-one-arg other)))
(define 2d-cord-minus
  (case-lambda
    [(self) (2d-cord-minus-one-arg self)]
    [(self arg1 . rest)
     (define init (2d-cord-minus-two-arg self arg1))
     (foldl (lambda (a b) (2d-cord-minus-two-arg b a)) init rest)]))

(struct 2d-grid (columms rows inner)
  #:constructor-name raw-2d-grid)

(define (make-2d-grid columms rows [default #f])
            (define inner (for*/fold ([inner (hash)]) ([y (in-range rows)] [x (in-range columms)])
                            (hash-set inner (2d-cord x y) default)))
            (raw-2d-grid columms rows inner))

(define (check-2d-grid-in-range self cords)
  (when (or (>= (2d-cord-x cords) (2d-grid-columms self)) (>= (2d-cord-y cords) (2d-grid-rows self))) (error "2d-grid" "Out of range")))

(define (2d-grid-ref self cords)
  (check-2d-grid-in-range self cords)
  (hash-ref (2d-grid-inner self) cords))

(define (2d-grid-set self cords value)
  (check-2d-grid-in-range self cords)
  (struct-copy 2d-grid self [inner (hash-set (2d-grid-inner self) cords value)]))