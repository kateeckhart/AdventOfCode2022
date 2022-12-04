#lang racket

(require "common.rkt")

(provide day1 day1-exe)

(define (day1 input-str)
  (define elves (map (lambda [elf] (map string->number elf)) (block-split input-str)))
  (define elf-totals (sort (map (lambda [elf] (foldl + 0 elf)) elves) >))
  (list (first elf-totals) (apply + (take elf-totals 3)))
  )

(define (day1-exe) (exe-day day1 "input/Day1.txt"))