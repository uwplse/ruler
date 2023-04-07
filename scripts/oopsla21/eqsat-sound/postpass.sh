#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

if [ -z "$1" ] || [ -z  "$2" ]; then
    echo "
    Rosette and z3 needed for this to work.
    Usage: ./postpass.sh rulefile.json < rational | 8 | 16 | 32 >
    "
    exit 1
else
    RULES="$1"
    DOMAIN="$2"
fi

bv="#lang rosette

; h/t Oak for this coercing trick during symbolic evaluation
(require (only-in racket [#%datum racket:#%datum])
         syntax/parse/define
         racket/splicing
         racket/stxparam)

(define-syntax-parser #%datum
  [(_ . x)
   #:when (syntax-parameter-value #'coerce-to-bv?)
   #'(bv (racket:#%datum . x) (racket:#%datum . $DOMAIN))]
  [(_ . x)
   #'(racket:#%datum . x)])
(define-syntax-parameter coerce-to-bv? #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define num_unsound 0)

(provide (all-defined-out))
(define-symbolic ?a ?b ?c (bitvector $DOMAIN))

(define failed
  (let ([fnm \"failed-validation.txt\"])
  (open-output-file #:exists'append fnm)))


(define (<=> l r)
  (define sol (verify (assert (eq? l r))))
  (cond [(! (unsat? sol))
    (let* (
      [lhs (format \"~a\" l)] 
      [rhs (format \"~a\" r)] 
      [st (string-append lhs \" => \" rhs \"\n\")]
      )
    (set! num_unsound (add1 num_unsound))
    (display st failed))
  ])
)

(define => <=>)
(define + bvadd)
(define -- bvsub)
(define * bvmul)
(define >> bvlshr)
(define << bvshl)
(define - bvneg)
(define ~ bvnot)
(define & bvand)
(define || bvor)
(define ^ bvxor)

(splicing-syntax-parameterize
    ([coerce-to-bv? #t])
"
rational="#lang rosette

(define failed
  (let ([fnm \"failed-validation.txt\"])
  (open-output-file #:exists'append fnm)))

(define num_unsound 0)
(provide (all-defined-out))

(define (mk_query as l r)
  (append as (list \`(assert (eq? ,l ,r)))))

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
                  [lhs (format \"~a\" l)] 
                  [rhs (format \"~a\" r)] 
                  [st (string-append lhs \" => \" rhs \"\n\")]
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
  \`(assume (! (eq? ,d 0))))"

squote="'"
UNKNOWN=0
UNSOUND=0
TIMEOUT="3s"

if [ $DOMAIN == "rational" ]; then
    jq  --arg FIELD "$squote" -r '[.all_eqs] | flatten | map("( => " + $FIELD + .lhs + " " + $FIELD +.rhs + " )") | .[]' \
        $RULES > to-check.txt
    while read p; do
      echo "$rational" > verify-rat.rkt
      echo "$p" >> verify-rat.rkt
      echo "(printf \"~a \n\" num_unsound)" >> verify-rat.rkt
      echo "(close-output-port failed)" >> verify-rat.rkt
      res=$(timeout -k $TIMEOUT $TIMEOUT racket verify-rat.rkt)
      if [ $? -ne 0 ]; then
          ((UNKNOWN=UNKNOWN+1))
          echo "$p" > unknown.txt
          echo "Rosette timed out." >&2
      else
          if [ $res -eq 1 ]; then
              ((UNSOUND=UNSOUND+1))
          fi
      fi
    done < to-check.txt
elif [ $DOMAIN = "4" ] || [ $DOMAIN = "8" ] || [ $DOMAIN = "16" ] || [ $DOMAIN = "32" ]; then
    # | => ||, racket doesn't like |
    sed 's/|/||/g' $RULES > tmpf.json
    jq -r '[.all_eqs] | flatten | map("( => " + .lhs + " " + .rhs + " )") | .[]' tmpf.json > to-check.txt
    while read p; do
      echo "$bv" > verify-bv.rkt
      echo "$p" >> verify-bv.rkt
      echo ")"  >> verify-bv.rkt
      echo "(close-output-port failed)" >> verify-bv.rkt
      echo "(printf \"~a \n\" num_unsound)" >> verify-bv.rkt
      res="$(timeout -k $TIMEOUT $TIMEOUT racket verify-bv.rkt)"
      if [ $? -ne 0 ]; then
          ((UNKNOWN=UNKNOWN+1))
          echo "$p" > unknown.txt
	  echo "Rosette timed out." >&2
      else
          if [ "$res" -eq 1 ]; then
              ((UNSOUND=UNSOUND+1))
          fi
      fi
    done < to-check.txt 
    rm tmpf.json
else
    echo "$2 is a bad domain" >&2
    exit 1
fi
echo "{\"unsound\": $UNSOUND, \"unknown\": $UNKNOWN}" > post_pass.json
