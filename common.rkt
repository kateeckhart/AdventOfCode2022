#lang racket

(provide line-split block-split exe-day)

(define (line-split str)
  (string-split str "\n"))

(define (block-split str)
  (map line-split (string-split str "\n\n")))

(define (exe-day day-func input-loc)
  (define input-file (open-input-file input-loc))
  (define input (port->string input-file))
  (close-input-port input-file)
  (day-func input)
  )