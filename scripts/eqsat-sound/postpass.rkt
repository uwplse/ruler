#lang rosette

(define failed
  (let ([fnm "failed-validation.txt"])
    (if (file-exists? fnm)
        (begin
          (delete-file fnm)
          (open-output-file fnm))
        (open-output-file fnm))
    ))

(define num_unsound 0)
(provide (all-defined-out))

(define (mk_query as l r)
  (append as (list `(assert (eq? ,l ,r)))))

(define (invoke-solver defs)
  (letrec (
           [symbv (thunk* (define-symbolic* x real?) x)]
           [sym-map (make-hash)]
           [rec 
               (lambda (def acc)
                 (match def 
                   [(list 'assume (list '! (list 'eq? l r)))  (assume (! (eq? (rec l acc) (rec r acc))))]
                   [(list 'assert (list 'eq? l r)) (verify (assert (eq? (rec l acc) (rec r acc))))]
                   [(list '/ x y) (/ (rec x acc) (rec y acc))]
                   [(list '+ x y) (+ (rec x acc) (rec y acc))]
                   [(list '- x y) (- (rec x acc) (rec y acc))]
                   [(list '* x y) (* (rec x acc) (rec y acc))]
                   [(list '~ x) (~ (rec x acc))]
                   [(list 'fabs x) (fabs (rec x acc))]
                   [(? number? x) x]
                   [x (hash-ref! sym-map x symbv)]
                   ))])
    (foldl rec (void) defs)))


(define (<=> l r)
  (clear-vc!)
  (let* ([ds (append (denoms l) (denoms r))]
         [as (map (lambda (d) (mk_div_vc d)) ds)]
         )
    (define queries (mk_query as l r))
    (define sol (invoke-solver queries))
    (cond [(! (unsat? sol))
           (let* (
                  [lhs (format "~a" l)] 
                  [rhs (format "~a" r)] 
                  [st (string-append lhs " => " rhs "\n")]
                  )
             (set! num_unsound (add1 num_unsound))
             (display st failed))
           ])))

(define => <=>)

(define (~ x) (- 0 x))

(define (fabs x)
  (if (or (positive? x) (zero? x))
      x
      (~ x)))

(define (denoms expr)
  (cond
    [(null? expr) null]
    [(list? expr)
     (cond
       [(equal? (car expr) '/)
        (cons (caddr expr)
              (append (denoms (cadr expr))
                      (denoms (caddr expr))))]
       [(equal? (car expr) '+)
        (append (denoms (cadr expr))
                (denoms (caddr expr)))]
       [(equal? (car expr) '*)
        (append (denoms (cadr expr))
                (denoms (caddr expr)))]
       [(equal? (car expr) '-)
        (append (denoms (cadr expr))
                (denoms (caddr expr)))]
       [(equal? (car expr) '~)
        (denoms (cadr expr))]
       [(equal? (car expr) 'fabs)
        (denoms (cadr expr))]
       [#t null])]
    [#t null]))

(define (mk_div_vc d)
  `(assume (! (eq? ,d 0))))



( => '(+ ?a ?b) '1 )
;( => '(/ ?c (/ ?b ?a)) '(/ ?a (/ ?b 1)))
( => '(+ ?a ?b) '(+ ?b ?a) )
( => '(* ?a ?b) '(* ?b ?a) )
( => '(* ?a 0) '0 )
( => '(/ ?a ?a) '1 )
( => '?a '(/ ?a 1) )
( => '(/ 0 ?a) '0 )
( => '?a '(+ ?a 0) )
( => '(- ?a ?a) '0 )
( => '?a '(- ?a 0) )
( => '?a '(* ?a 1) )
( => '(~ ?a) '(* ?a -1) )
( => '(~ ?a) '(/ ?a -1) )
( => '(~ ?a) '(- 0 ?a) )
( => '(- ?a -1) '(+ ?a 1) )
( => '(- ?a 1) '(+ ?a -1) )
( => '(/ (* ?a ?b) (* ?b ?c)) '(/ ?a ?c) )
( => '(+ (+ ?a ?b) (- ?c ?a)) '(+ ?b ?c) )
( => '(+ (- ?a ?b) (- ?b ?c)) '(- ?a ?c) )
( => '(/ (* ?a ?b) (/ ?b ?c)) '(* ?a ?c) )
( => '(* (/ ?a ?b) (/ ?b ?c)) '(/ ?a ?c) )
( => '(- (+ ?a ?b) (- ?b ?c)) '(+ ?a ?c) )
( => '(- (+ ?a ?b) (+ ?a ?c)) '(- ?b ?c) )
( => '(* (* ?a ?b) (/ ?c ?b)) '(* ?a ?c) )
( => '(/ (/ ?a ?b) (/ ?a ?c)) '(/ ?c ?b) )
( => '(- (- ?a ?b) (- ?c ?b)) '(- ?a ?c) )
( => '(- (- ?a ?b) (- ?a ?c)) '(- ?c ?b) )
( => '(- (- ?a ?b) (+ ?a ?c)) '(~ (+ ?b ?c)) )
( => '(* ?a (* ?b ?c)) '(* ?b (* ?a ?c)) )
( => '(/ ?a (/ ?b ?c)) '(/ ?c (/ ?b ?a)) )
( => '(- ?a (- ?b ?c)) '(- ?c (- ?b ?a)) )
( => '(+ ?a (+ ?b ?c)) '(+ ?b (+ ?a ?c)) )
( => '(- (+ ?a ?b) ?c) '(+ ?a (- ?b ?c)) )
( => '(* ?a (/ ?b ?c)) '(* ?b (/ ?a ?c)) )
( => '(/ (* ?a ?b) ?c) '(* ?a (/ ?b ?c)) )
( => '(/ ?a (- ?b ?c)) '(/ (~ ?a) (- ?c ?b)) )
( => '(* ?a (- ?b ?c)) '(* (~ ?a) (- ?c ?b)) )
( => '(+ (* ?a ?b) (* ?a ?c)) '(* ?a (+ ?b ?c)) )
( => '(/ (+ ?a ?b) ?c) '(+ (/ ?b ?c) (/ ?a ?c)) )
( => '(/ (/ ?a ?b) (* ?a ?c)) '(/ 1 (* ?b ?c)) )
( => '(/ (- ?a ?b) ?c) '(- (/ ?a ?c) (/ ?b ?c)) )
( => '(- (* ?a ?b) (* ?b ?c)) '(* ?b (- ?a ?c)) )
( => '(- (~ ?a) (+ ?b ?c)) '(- (~ ?b) (+ ?a ?c)) )
( => '(/ (~ ?a) (* ?b ?c)) '(/ (/ ?a ?b) (~ ?c)) )
( => '(/ (/ -1 ?a) (* ?b ?c)) '(/ (/ -1 ?c) (* ?a ?b)) )
( => '(- (- 1 ?a) (+ ?b ?c)) '(- (- 1 ?b) (+ ?a ?c)) )
( => '(/ (/ ?a ?b) (+ ?c ?c)) '(/ (/ ?a ?c) (+ ?b ?b)) )
( => '(- (* ?a ?b) (/ 1 ?c)) '(+ (* ?a ?b) (/ -1 ?c)) )
( => '(- (/ ?a ?b) (/ -1 ?c)) '(+ (/ ?a ?b) (/ 1 ?c)) )
( => '(* ?a (/ ?b (* ?c ?c))) '(* ?a (/ (/ ?b ?c) ?c)) )
( => '(/ (/ ?a ?b) (* ?c ?c)) '(/ (/ ?a ?c) (* ?b ?c)) )
( => '(/ (/ 1 ?a) (* ?b ?c)) '(/ (/ 1 ?c) (* ?a ?b)) )
( => '(- (+ ?a ?b) (+ ?c ?c)) '(+ ?b (- (- ?a ?c) ?c)) )
( => '(- (- ?a ?b) (+ ?c ?c)) '(- (- ?a ?c) (+ ?b ?c)) )
( => '(/ (- ?a ?b) (- ?b ?a)) '-1 )
( => '(/ ?a ?b) '(* (+ ?a ?a) (/ 1/2 ?b)) )
( => '(fabs (- ?a ?b)) '(fabs (- ?b ?a)) )
( => '(fabs (* ?a ?b)) '(* (fabs ?a) (fabs ?b)) )
( => '(fabs (/ ?a ?b)) '(/ (fabs ?a) (fabs ?b)) )
( => '(- -1 (/ ?a ?b)) '(+ -1 (/ (~ ?a) ?b)) )
( => '(- ?a (/ ?a ?b)) '(+ ?a (/ (~ ?a) ?b)) )
( => '(/ (fabs ?a) (* ?a ?b)) '(/ (/ ?a ?b) (fabs ?a)) )
( => '(- (~ ?a) (/ ?a ?b)) '(- (/ (~ ?a) ?b) ?a) )
( => '(fabs ?a) '(fabs (fabs ?a)) )
( => '(fabs (* ?a ?a)) '(* ?a ?a) )
( => '(fabs (+ ?a ?a)) '(* (fabs ?a) 2) )
( => '(+ -1 (* ?a ?a)) '(* (+ ?a -1) (+ ?a 1)) )
( => '(- 1 (* ?a ?a)) '(* (- -1 ?a) (+ ?a -1)) )

(close-output-port failed)