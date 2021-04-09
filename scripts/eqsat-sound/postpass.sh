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
    Usage: ./postpass.sh rulefile.json < rat | bool | 8 | 16 | 32 >
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
  (if (file-exists? fnm)
    (begin
      (delete-file fnm)
      (open-output-file fnm))
    (open-output-file fnm))
  ))

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

(define num_unsound 0)

(provide (all-defined-out))
(define-symbolic ?a ?b ?c real?)

(define failed
  (let ([fnm \"failed-validation.txt\"])
  (if (file-exists? fnm)
    (begin
      (delete-file fnm)
      (open-output-file fnm))
    (open-output-file fnm))
  ))

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

(define (~ x) (- 0 x))
(define (fabs x)
  (if (|| (positive? x) (zero? x))
     (x)
     (~ x)))
"

squote="'"
if [ $DOMAIN == 'rat' ]; then
    echo "$rational" > verify-rat.rkt
    jq -r '[.eqs] | flatten | map("( => " + .lhs + " " + .rhs + " )") | .[]' $RULES >> verify-rat.rkt
    # jq  --arg FIELD "$squote" -r '[.eqs] | flatten | map("( => " + $FIELD + .lhs + " " + $FIELD +.rhs + " )") | .[]' \
    #      $RULES >> verify-rat.rkt
    echo "(printf \"~a \n\" num_unsound)" >> verify-rat.rkt
    echo "(close-output-port failed)" >> verify-rat.rkt
    racket verify-rat.rkt
elif [ $DOMAIN == '4' ] || [ $DOMAIN == '8' ] || [ $DOMAIN == '16' ] || [ $DOMAIN == '32' ]; then
    # | => ||, racket doesn't like |
    sed 's/\|/||/g' $RULES > tmp.json
    echo "$bv" > verify-bv.rkt
    jq -r '[.eqs] | flatten | map("( => " + .lhs + " " + .rhs + " )") | .[]' tmp.json >> verify-bv.rkt
    echo ")"  >> verify-bv.rkt
    echo "(close-output-port failed)" >> verify-bv.rkt
    echo "(printf \"~a \n\" num_unsound)" >> verify-bv.rkt
    racket verify-bv.rkt
    rm tmp.json
else
    echo "$2 is a bad domain"
    exit 1
fi

