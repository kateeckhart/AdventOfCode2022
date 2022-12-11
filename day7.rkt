#lang racket

(require "common.rkt")

(provide day7 day7-exe)

;Parent is dir, size is exact int, children is a hash table from string to (dir or exact int).
(struct dir (raw-size children))
(define (dir-size dir) (if (dir? dir) (dir-raw-size dir) (+ dir)))

(define empty-dir (dir 0 (hash)))

(struct stack (dir name rest))
(define (reverse-stack curr-stack)
  (let rs ([curr-stack curr-stack] [new-stack null])
    (if (null? curr-stack) new-stack
        (rs (stack-rest curr-stack) (stack (stack-dir curr-stack) (stack-name curr-stack) new-stack)))))
(define root-stack (stack empty-dir "/" null))

(define (dir-root? dir-stack) (null? (stack-rest dir-stack)))
(define (dir-root dir-stack) (if (dir-root? dir-stack) dir-stack (dir-root (stack-rest dir-stack))))
(define (dir-up dir-stack) (if (dir-root? dir-stack) dir-stack (stack-rest dir-stack)))
(define (calc-dir-size dir-children)
  (for/sum ([dir (in-hash-values dir-children)]) (dir-size dir)))

(define (update-stack dir-stack new-dir new-dir-name)
  (let us ([new-stack null] [dir-stack dir-stack] [new-dir new-dir] [new-dir-name new-dir-name])
    (if (null? dir-stack)
        (reverse-stack new-stack)
        (let* (
               [curr-dir (stack-dir dir-stack)]
               [curr-dir-name (stack-name dir-stack)]
               [new-children (hash-set (dir-children curr-dir) new-dir-name new-dir)]
               [nd (dir (calc-dir-size new-children) new-children)]
               [ds (stack-rest dir-stack)])
          (us (stack nd curr-dir-name new-stack) ds nd curr-dir-name)))))

(define (cd dir-stack dir-name)
  (define curr-dir (stack-dir dir-stack))
  (define child-dir (hash-ref (dir-children curr-dir) dir-name #f))
  (cond
    [(equal? dir-name "..") (dir-up dir-stack)]
    [(equal? dir-name "/") (dir-root dir-stack)]
    [child-dir (stack child-dir dir-name dir-stack)]
    [else (cd (update-stack dir-stack empty-dir dir-name) dir-name)]))

(define (parse-tree input)
  (let pt ([input (line-split input)] [dir-stack root-stack])
    (if (null? input) (dir-root dir-stack)
        (let* (
               [split-input (string-split (first input) " ")]
               [file-size (string->number (first split-input))])
          (cond
            [file-size (pt (rest input) (update-stack dir-stack file-size (second split-input)))]
            [(and (equal? (first split-input) "$") (equal? (second split-input) "cd")) (pt (rest input) (cd dir-stack (third split-input)))]
            [else (pt (rest input) dir-stack)])))))

(define (day7-part1 tree)
  (let sum ([dir (stack-dir tree)] [acc 0])
    (define this-size (let ([size (dir-size dir)]) (if (<= size 100000) size 0)))
    (if (dir? dir)
        (let ([this-size (let ([size (dir-size dir)]) (if (<= size 100000) size 0))]
              [child-size (for/sum ([child (in-hash-values (dir-children dir))]) (sum child 0))])
          (+ this-size child-size)) 
        0)))

(define (smallest-del-dir dir needed-free-space)
  (if (dir? dir)
      (for/fold ([ans (dir-size dir)]) ([child (in-hash-values (dir-children dir))])
        (define child-size (smallest-del-dir child needed-free-space))
        (if (and (dir? child) (>= child-size needed-free-space))
            (min child-size ans)
            ans))
      0))

(define (day7-part2 tree)
  (define root (stack-dir tree))
  (define free-space (- 70000000 (dir-size root)))
  (define needed-free-space (- 30000000 free-space))
  (smallest-del-dir root needed-free-space))

(define (day7 input)
  (define tree (parse-tree input))
  (list (day7-part1 tree) (day7-part2 tree)))

(define (day7-exe) (exe-day day7 "input/Day7.txt"))