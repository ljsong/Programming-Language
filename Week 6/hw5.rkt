;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
;; test case (list (list (int 3) (int 4)) (list (list (int 5)) (list (int 6) (int 7))) (int 9)))
(define (racketlist->mupllist rkt-list)
  (cond [(null? rkt-list) (aunit)]
        [(pair? (car rkt-list)) (apair (racketlist->mupllist (car rkt-list)) (racketlist->mupllist (cdr rkt-list)))]
        [else (apair (car rkt-list) (racketlist->mupllist (cdr rkt-list)))]))

;; reverse mupl-list
;; (apair (apair (int 3) (apair (int 4) (aunit))) (apair (apair (apair (int 5) (aunit)) (apair (apair (int 6) (apair (int 7) (aunit))) (aunit))) (apair (int 9) (aunit))))
(define (mupllist->racketlist mupl-list)
  (cond [(aunit? mupl-list) null]
        [(apair? (apair-e1 mupl-list)) (cons (mupllist->racketlist (apair-e1 mupl-list)) (mupllist->racketlist (apair-e2 mupl-list)))]
        [else (cons (apair-e1 mupl-list) (mupllist->racketlist (apair-e2 mupl-list)))]))


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
;; (define add (eval-exp (fun #f "x" (fun #f "y" (add (var "x") (var "y"))))))
;; (define func (call (call add (int 3)) (int 7)))
;; (define x (eval-exp (call mupl-map (fun #f "x" (add (var "x") (int 7))))))
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL: addition applied to non-number")))]
        [(or (int? e) (closure? e) (aunit? e)) e]
        [(fun? e) (let ([fun-name (fun-nameopt e)]
                        [fun-code (fun-body e)])
                        (if fun-name
                            (closure (cons (cons fun-name e) env) e)
                            (closure env e)))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL: ifgreater applied to non-number")))]
        [(call? e) (letrec ([fun-arg (eval-under-env (call-actual e) env)]
                            [f (lambda (sub-exp env)
                                 (cond [(or (call? sub-exp) (var? sub-exp)) (f (eval-under-env sub-exp env) env)]     ; for function-name-bindings or curried cases
                                       [(mlet? sub-exp) (let ([new-env (cons (cons (mlet-var sub-exp) (mlet-e sub-exp)) env)])
                                                          (f (eval-under-env (mlet-body sub-exp) new-env) new-env))]
                                       [(fun? sub-exp) (let ([fun-result (eval-under-env sub-exp env)])
                                                         (f fun-result (closure-env fun-result)))]    ; for (call (fun #f "x" (add (var "x") (int 5))) (int 9)
                                       [(closure? sub-exp) (let* ([fun (closure-fun sub-exp)]        ; for recursive
                                                            [fun-param (fun-formal fun)]
                                                            [fun-code (fun-body fun)]
                                                            [new-env (cons (cons fun-param fun-arg) (closure-env sub-exp))])
                                                       (eval-under-env fun-code new-env))]
                                       [else (error (format "MUPL: Function ~v has an illegal format" sub-exp))]))])
                     (f (call-funexp e) env))]
        [(mlet? e) (let* ([value (eval-under-env (mlet-e e) env)]
                          [new-env (cons (cons (mlet-var e) value) env)])
                     (eval-under-env (mlet-body e) new-env))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env)
                           (eval-under-env (apair-e2 e) env))]
        [(fst? e) (let ([result (eval-under-env (fst-e e) env)])
                    (if (apair? result)
                        (apair-e1 result)
                        (error "MUPL: fst only accepted apair as an argument!")))]
        [(snd? e) (let ([result (eval-under-env (snd-e e) env)])
                    (if (apair? result)
                      (apair-e2 result)
                      (error "MUPL: snd only accepted apair as an argument!")))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env))
                           (int 1)
                           (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) (if (null? lstlst)
                              e2
                              (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4) (mlet "_x" e1
                                 (mlet "_y" e2
                                       (ifgreater (var "_x") (var "_y")
                                                  e4
                                                  (ifgreater (var "_y") (var "_x")
                                                             e4
                                                             e3)))))

;; Problem 4

(define mupl-map
  (fun #f "func"
       (fun "fn" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "func") (fst (var "lst")))
                            (call (var "fn") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
        (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
