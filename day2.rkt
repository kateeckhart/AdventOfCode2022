#lang racket

(require "common.rkt")

(provide day2 day2-exe)

;Shape index is score - 1
;Index is my-shape-index + 3 * elf-shape-index
(define rps-res-lookup #('draw 'win 'lose 'lose 'draw 'win 'win 'lose 'draw))

(define rps-p1-score-lookup
  (let ([i 0])
    (vector->immutable-vector
     (vector-map (lambda [res] (define score (case res
                                               [('win) (+ i 6 1)]
                                               [('lose) (+ i 0 1)]
                                               [('draw) (+ i 3 1)]))
                   (set! i (remainder (+ i 1) 3))
                   score) rps-res-lookup))))

(define (rps-res elf me) (vector-ref rps-res-lookup (+ me (* elf 3))))
(define (rps-part1 elf me) (vector-ref rps-p1-score-lookup (+ me (* elf 3))))

(define rps-p2-score-lookup
  (vector->immutable-vector
   (let ([vec (make-vector 9)])
     (for* ([me (in-range 3)] [elf (in-range 3)])
       (define elf-index (* elf 3))
       (define res (rps-res elf me))
       (define my-index (case res
                          [('lose) 0]
                          [('draw) 1]
                          [('win) 2]
                          ))
       (define score (rps-part1 elf me))
       (vector-set! vec (+ elf-index my-index) score))
     vec)
   ))

(define (rps-part2 elf me) (vector-ref rps-p2-score-lookup (+ me (* elf 3))))

(define (process-input input)
  (map (lambda [line]
         (define split-line (string-split line " "))
         (define my-shape (- (char->integer (string-ref (first split-line) 0)) (char->integer #\A)))
         (define elf-shape (- (char->integer (string-ref (second split-line) 0)) (char->integer #\X)))
         (list my-shape elf-shape)) (line-split input)))

(define (day2-part1 input) (foldl (lambda [match acc] (+ acc (apply rps-part1 match))) 0 (process-input input)))
(define (day2-part2 input) (foldl (lambda [match acc] (+ acc (apply rps-part2 match))) 0 (process-input input)))
(define (day2 input) (list (day2-part1 input) (day2-part2 input)))

(define (day2-exe) (exe-day day2 "input/Day2.txt"))