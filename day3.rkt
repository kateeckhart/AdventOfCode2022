#lang racket

(require "common.rkt")

(provide day3 day3-exe)

(define (calc-priority racksack)
  (sequence-fold (lambda [acc item]
                   (define priority
                     (if
                      (char>=? item #\a)
                      (+ 1 (- (char->integer item) (char->integer #\a)))
                      (+ 27 (- (char->integer item) (char->integer #\A)))))
                   (+ acc priority)
                   )
                 0
                 (in-immutable-set racksack)))

(define (day3-part1 input)
  (foldl (lambda [line acc]
           (define half-size (/ (string-length line) 2))
           (define first-half (list->set (string->list (substring line 0 half-size))))
           (define second-half (list->set (string->list (substring line half-size))))
           (define common (set-intersect first-half second-half))
           (+ acc (calc-priority common)))
         0
         (line-split input)))

(define (day3-part2 input)
  (let run ([acc 0] [list (line-split input)] )
    (if (null? list) acc
        (let*
            ([first-sack (list->set (string->list (first list)))]
             [second-sack (list->set (string->list (second list)))]
             [third-sack (list->set (string->list (third list)))]
             [rest (list-tail list 3)]
             [common (set-intersect first-sack second-sack third-sack)])
          (run (+ acc (calc-priority common)) rest)))))

(define (day3 input) (list (day3-part1 input) (day3-part2 input)))

(define (day3-exe) (exe-day day3 "input/Day3.txt"))