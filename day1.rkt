#lang racket

(require "common.rkt")

(provide day1 day1-exe)

(define (day1 input_str)
  (define input-port (open-input-string input_str))
  (define elves '())
  (define current_elf '())
  (define (process-line)
    (define line (read-line input-port))
    (if
     (eof-object? line)
     (begin
       (set! elves (cons current_elf elves))
       (set! current_elf '())
       )
     (begin
       (if
        (equal? line "")
        (begin
          (set! elves (cons current_elf elves))
          (set! current_elf '())
          )
        (let* ((num-port (open-input-string line)) (num (read num-port)))
          (close-input-port num-port)
          (if
           (not (integer? num))
           (error "A non number found in input")
           (void)
           )
          (set! current_elf (cons num current_elf))
          ))
       (process-line))))
  (process-line)
  (close-input-port input-port)
  (define elf-totals (sort (map (lambda (elf) (foldl + 0 elf)) elves) >))
  `(,(car elf-totals) ,(+ (car elf-totals) (second elf-totals) (third elf-totals)))
  )

(define (day1-exe)
  (define input-file (open-input-file "input/Day1.txt"))
  (define input (port->string input-file))
  (close-input-port input-file)
  (day1 input)
  )