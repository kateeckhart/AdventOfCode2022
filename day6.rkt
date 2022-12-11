#lang racket

(require "common.rkt")

(provide day6 day6-exe)

(define (see-char map char)
  (define count (hash-ref map char 0))
  (hash-set map char (+ count 1)))

(define (unsee-char map char)
  (define count (- (hash-ref map char 0) 1))
  (if
   (= count 0)
   (hash-remove map char)
   (hash-set map char count)))

(define (calc-start-of-p input-str diff-count)
  (define input (string->list input-str))
  (let loop ([seen-chars (foldl (lambda (char map) (see-char map char)) (hash) (take input diff-count))]
             [i diff-count]
             [leading input]
             [trailing (drop input diff-count)])
    (if (= (hash-count seen-chars) diff-count) i
        (loop (unsee-char (see-char seen-chars (first trailing)) (first leading)) (+ 1 i) (rest leading) (rest trailing)))))

(define (day6 input) (list (calc-start-of-p input 4) (calc-start-of-p input 14)))

(define (day6-exe) (exe-day day6 "input/Day6.txt"))