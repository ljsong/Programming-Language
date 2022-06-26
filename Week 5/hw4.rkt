
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map string-append xs))

(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
  (letrec ([wrapper (lambda (xs s n)
                  (if (> n 0)
                      (cons (car (s)) (wrapper xs (cdr (s)) (- n 1)))
                      xs))])
  (wrapper null s n)))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons x (lambda () (f
                                              (if (= (remainder (+ x 1) 5) 0)
                                                  (- (+ (abs x) 1))
                                                  (+ (abs x) 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if (even? x)
                              (cons "dan.jpg" (lambda () (f (+ x 1))))
                              (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 0))))

(define (stream-add-zero s)
  0)

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))
