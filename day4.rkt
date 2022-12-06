#lang racket

(require "common.rkt")

(provide day4 day4-exe)

(define input-regex #px"^(\\d+)-(\\d+),(\\d+)-(\\d+)$")

(define (parse-input input)
  (for/list ([line (in-list (line-split input))])
    (define match (regexp-match input-regex line))
    (define pair1-a (string->number (list-ref match 1)))
    (define pair1-b (string->number (list-ref match 2)))
    (define pair2-a (string->number (list-ref match 3)))
    (define pair2-b (string->number (list-ref match 4)))
    (list (list pair1-a pair1-b) (list pair2-a pair2-b))))

(define (overlap-range a b)
  (define leser (max (first a) (first b)))
  (define greater (min (second a) (second b)))
  (if (<= leser greater) (list leser greater) #f))

(define (total-overlap-range a b)
  (define overlap (overlap-range a b))
  (or (equal? overlap a) (equal? overlap b)))

(define (day4-part1 input)
  (for/sum ([assign (in-list (parse-input input))])
    (if (apply total-overlap-range assign) 1 0)))

(define (day4-part2 input)
  (for/sum ([assign (in-list (parse-input input))])
    (if (apply overlap-range assign) 1 0)))

(define (day4 input) (list (day4-part1 input) (day4-part2 input)))

(define (day4-exe) (exe-day day4 "input/Day4.txt"))