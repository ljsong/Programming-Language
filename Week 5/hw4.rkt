
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (xs s n)
                (if (> n 0)
                    (cons (car (s)) (f xs (cdr (s)) (- n 1)))
                    xs))])
    (f null s n)))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons x (lambda ()
                                    (let ([real-num (+ (abs x) 1)])
                                      (f (if (= (remainder real-num 5) 0)
                                             (- real-num)
                                             real-num))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons (if (even? x)
                                    "dan.jpg"
                                    "dog.jpg")
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (stream-add-zero s)
  (letrec ([f (lambda (pr) (cons (cons 0 (car pr)) (lambda () (f ((cdr pr))))))])
    (lambda() (f (s)))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons (cons (list-nth-mod xs x) (list-nth-mod ys x))
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (vec n)
                (let ([it (if (< n (vector-length vec))
                              (vector-ref vec n)
                              #f)])
                  (if (not it) #f
                      (cond [(pair? it) (if (equal? (car it) v)
                                            it
                                            (f vec (+ n 1)))]
                            [#t (f vec (+ n 1))]))))])
    (f vec 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [slot 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([missed-item (assoc v xs)])
                        (begin
                          (vector-set! memo slot missed-item)
                          (set! slot (remainder (+ slot 1) n))
                          missed-item)))))])
    f))
                  
              
