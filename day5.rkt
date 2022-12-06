#lang racket

(require "common.rkt")

(provide day5 day5-exe)

(define ins-regex #px"^move (\\d+) from (\\d+) to (\\d+)$")

(define (parse-stack in-stack)
  (define stack-str (rest (reverse in-stack)))
  (define stack-count (/ (+ (string-length (first stack-str)) 1) 4))
  (define stack (make-vector stack-count null))
  (for* ([line (in-list stack-str)] [i (in-range stack-count)])
    (define stack-char (string-ref line (+ (* i 4) 1)))
    (unless (char=? stack-char #\space)
      (vector-set! stack i (cons stack-char (vector-ref stack i)))))
  (vector->immutable-vector stack))

(define (parse-instruction ins)
  (define match (regexp-match ins-regex ins))
  (list (string->number (list-ref match 1)) (- (string->number (list-ref match 2)) 1) (- (string->number (list-ref match 3)) 1)))

(define (listify-stack stacks)
  (list->string (for/list ([stack (in-vector stacks)]) (first stack))))

(define (day5-part1 in-stack instructions)
  (define stack (vector-copy in-stack))
  (for* ([ins (in-list instructions)] [i (in-range (first ins))])
    (define from-stack-i (second ins))
    (define from-stack (vector-ref stack from-stack-i))
    (define to-stack-i (third ins))
    (define to-stack (vector-ref stack to-stack-i))
    (define top-crate (first from-stack))
    (vector-set! stack from-stack-i (rest from-stack))
    (vector-set! stack to-stack-i (cons top-crate to-stack)))
  (listify-stack stack))

(define (day5-part2 in-stack instructions)
  (define stack (vector-copy in-stack))
  (for ([ins (in-list instructions)])
    (define count (first ins))
    (define from-stack-i (second ins))
    (define old-from-stack (vector-ref stack from-stack-i))
    (define to-stack-i (third ins))
    (define to-stack (vector-ref stack to-stack-i))
    (define-values (top-crates new-from-stack) (split-at old-from-stack count))
    (vector-set! stack from-stack-i new-from-stack)
    (vector-set! stack to-stack-i (append top-crates to-stack)))
  (listify-stack stack))

(define (day5 input)
  (define blocks (block-split input))
  (define stack (parse-stack (first blocks)))
  (define instructions (map parse-instruction (second blocks)))
  (list (day5-part1 stack instructions) (day5-part2 stack instructions)))

(define (day5-exe) (exe-day day5 "input/Day5.txt"))