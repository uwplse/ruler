#lang plai

;(provide (all-defined-out))
;(provide all-rules)

;; rules

(define commutativity
  (list
   '[+-commutative     (+ a b)               (+ b a)]
   '[*-commutative     (* a b)               (* b a)]))

(define associativity
  (list
   '[associate-+r+     (+ a (+ b c))         (+ (+ a b) c)]
   '[associate-+l+     (+ (+ a b) c)         (+ a (+ b c))]
   '[associate-+r-     (+ a (- b c))         (- (+ a b) c)]
   '[associate-+l-     (+ (- a b) c)         (- a (- b c))]
   '[associate--r+     (- a (+ b c))         (- (- a b) c)]
   '[associate--l+     (- (+ a b) c)         (+ a (- b c))]
   '[associate--l-     (- (- a b) c)         (- a (+ b c))]
   '[associate--r-     (- a (- b c))         (+ (- a b) c)]
   '[associate-*r*     (* a (* b c))         (* (* a b) c)]
   '[associate-*l*     (* (* a b) c)         (* a (* b c))]
   '[associate-*r/     (* a (/ b c))         (/ (* a b) c)]
   '[associate-*l/     (* (/ a b) c)         (/ (* a c) b)]
   '[associate-/r*     (/ a (* b c))         (/ (/ a b) c)]
   '[associate-/l*     (/ (* b c) a)         (/ b (/ a c))]
   '[associate-/r/     (/ a (/ b c))         (* (/ a b) c)]
   '[associate-/l/     (/ (/ b c) a)         (/ b (* a c))]))

(define identity
  (list
   '[+-lft-identity    (+ 0 a)               a]
   '[+-rgt-identity    (+ a 0)               a]
   '[--rgt-identity    (- a 0)               a]
   '[sub0-neg          (- 0 a)               (neg a)]
   '[remove-double-neg (neg (neg a))             a]
   '[*-lft-identity    (* 1 a)               a]
   '[*-rgt-identity    (* a 1)               a]
   '[/-rgt-identity    (/ a 1)               a]
   '[mul-1-neg         (* -1 a)              (neg a)]))


(define counting
  (list '[count-2   (+ x x)   (* 2 x)]))

; Distributivity
(define distributivity
  (list
   '[distribute-lft-in      (* a (+ b c))         (+ (* a b) (* a c))]
   '[distribute-rgt-in      (* a (+ b c))         (+ (* b a) (* c a))]
   '[distribute-lft-out     (+ (* a b) (* a c))   (* a (+ b c))]
   '[distribute-lft-out--   (- (* a b) (* a c))   (* a (- b c))]
   '[distribute-rgt-out     (+ (* b a) (* c a))   (* a (+ b c))]
   '[distribute-rgt-out--   (- (* b a) (* c a))   (* a (- b c))]
   '[distribute-lft1-in     (+ (* b a) a)         (* (+ b 1) a)]
   '[distribute-rgt1-in     (+ a (* c a))         (* (+ c 1) a)]))

; Safe Distributiviity
(define distributivity-fp-safe
  (list
   '[distribute-lft-neg-in  (neg (* a b))           (* (neg a) b)]
   '[distribute-rgt-neg-in  (neg (* a b))           (* a (neg b))]
   '[distribute-lft-neg-out (* (neg a) b)           (neg (* a b))]
   '[distribute-rgt-neg-out (* a (neg b))           (neg (* a b))]
   '[distribute-neg-in      (neg (+ a b))           (+ (neg a) (neg b))]
   '[distribute-neg-out     (+ (neg a) (neg b))       (neg (+ a b))]
   '[distribute-frac-neg    (/ (neg a) b)           (neg (/ a b))]
   '[distribute-neg-frac    (neg (/ a b))           (/ (neg a) b)]))

; Difference of squares
(define difference-of-squares-canonicalize
  (list
   '[swap-sqr              (* (* a b) (* a b))   (* (* a a) (* b b))]
   '[unswap-sqr            (* (* a a) (* b b))   (* (* a b) (* a b))]
   '[difference-of-squares (- (* a a) (* b b))   (* (+ a b) (- a b))]
   '[difference-of-sqr-1   (- (* a a) 1)         (* (+ a 1) (- a 1))]
   '[difference-of-sqr--1  (+ (* a a) -1)        (* (+ a 1) (- a 1))]
   '[sqr-pow               (pow a b)             (* (pow a (/ b 2)) (pow a (/ b 2)))]
   '[pow-sqr               (* (pow a b) (pow a b)) (pow a (* 2 b))]))

(define difference-of-squares-flip
  (list
   '[flip-+     (+ a b)  (/ (- (* a a) (* b b)) (- a b))]
   '[flip--     (- a b)  (/ (- (* a a) (* b b)) (+ a b))]))

; Identity
(define id-reduce
  (list
   '[remove-double-div (/ 1 (/ 1 a))         a]
   '[rgt-mult-inverse  (* a (/ 1 a))         1]
   '[lft-mult-inverse  (* (/ 1 a) a)         1]))

(define id-reduce-fp-safe-nan
  (list
   '[+-inverses        (- a a)               0]
   '[*-inverses        (/ a a)               1]
   '[div0              (/ 0 a)               0]
   '[mul0-lft          (* 0 a)               0]
   '[mul0-rgt          (* a 0)               0]))

(define id-reduce-fp-safe
  (list
   '[+-lft-identity    (+ 0 a)               a]
   '[+-rgt-identity    (+ a 0)               a]
   '[--rgt-identity    (- a 0)               a]
   '[sub0-neg          (- 0 a)               (neg a)]
   '[remove-double-neg (neg (neg a))             a]
   '[*-lft-identity    (* 1 a)               a]
   '[*-rgt-identity    (* a 1)               a]
   '[/-rgt-identity    (/ a 1)               a]
   '[mul-1-neg         (* -1 a)              (neg a)]))

(define nan-transform-fp-safe
  (list
   '[sub-neg           (- a b)               (+ a (neg b))]
   '[unsub-neg         (+ a (neg b))           (- a b)]
   '[neg-sub0          (neg b)                 (- 0 b)]
   '[neg-mul-1         (neg a)                 (* -1 a)]))

(define id-transform
  (list
   '[div-inv           (/ a b)               (* a (/ 1 b))]
   '[un-div-inv        (* a (/ 1 b))         (/ a b)]
   '[clear-num         (/ a b)               (/ 1 (/ b a))]))


(define id-transform-fp-safe
  (list
   '[*-un-lft-identity a                     (* 1 a)]))

; Difference of cubes
(define difference-of-cubes
  (list
   '[sum-cubes        (+ (pow a 3) (pow b 3))
                      (* (+ (* a a) (- (* b b) (* a b))) (+ a b))]
   '[difference-cubes (- (pow a 3) (pow b 3))
                      (* (+ (* a a) (+ (* b b) (* a b))) (- a b))]
   '[flip3-+          (+ a b)
                      (/ (+ (pow a 3) (pow b 3)) (+ (* a a) (- (* b b) (* a b))))]
   '[flip3--          (- a b)
                      (/ (- (pow a 3) (pow b 3)) (+ (* a a) (+ (* b b) (* a b))))]))

; Dealing with fractions
(define fractions-distribute
  (list
   '[div-sub     (/ (- a b) c)        (- (/ a c) (/ b c))]
   '[times-frac  (/ (* a b) (* c d))  (* (/ a c) (/ b d))]))

(define fractions-transform
  (list
   '[sub-div     (- (/ a c) (/ b c))  (/ (- a b) c)]
   '[frac-add    (+ (/ a b) (/ c d))  (/ (+ (* a d) (* b c)) (* b d))]
   '[frac-sub    (- (/ a b) (/ c d))  (/ (- (* a d) (* b c)) (* b d))]
   '[frac-times  (* (/ a b) (/ c d))  (/ (* a c) (* b d))]
   '[frac-2neg   (/ a b)              (/ (neg a) (neg b))]))

; Square root
(define squares-reduce
  (list
   '[rem-square-sqrt   (* (sqrt x) (sqrt x))     x]
   '[rem-sqrt-square   (sqrt (* x x))     (fabs x)]))

(define squares-reduce-fp-sound
  (list
   '[sqr-neg           (* (neg x) (neg x))        (* x x)]))

(define squares-transform
  (list
   '[sqrt-prod         (sqrt (* x y))         (* (sqrt x) (sqrt y))]
   '[sqrt-div          (sqrt (/ x y))         (/ (sqrt x) (sqrt y))]
   '[sqrt-pow1         (sqrt (pow x y))       (pow x (/ y 2))]
   '[sqrt-pow2         (pow (sqrt x) y)       (pow x (/ y 2))]
   '[sqrt-unprod       (* (sqrt x) (sqrt y))  (sqrt (* x y))]
   '[sqrt-undiv        (/ (sqrt x) (sqrt y))  (sqrt (/ x y))]
   '[add-sqr-sqrt      x                      (* (sqrt x) (sqrt x))]))

; Cube root
(define cubes-reduce
  (list
   '[rem-cube-cbrt     (pow (cbrt x) 3) x]
   '[rem-cbrt-cube     (cbrt (pow x 3)) x]
   '[cube-neg          (pow (neg x) 3)    (neg (pow x 3))]))

(define cubes-distribute
  (list
   '[cube-prod       (pow (* x y) 3) (* (pow x 3) (pow y 3))]
   '[cube-div        (pow (/ x y) 3) (/ (pow x 3) (pow y 3))]
   '[cube-mult       (pow x 3)       (* x (* x x))]))

(define cubes-transform
  (list
   '[cbrt-prod         (cbrt (* x y))           (* (cbrt x) (cbrt y))]
   '[cbrt-div          (cbrt (/ x y))           (/ (cbrt x) (cbrt y))]
   '[cbrt-unprod       (* (cbrt x) (cbrt y))    (cbrt (* x y))]
   '[cbrt-undiv        (/ (cbrt x) (cbrt y))    (cbrt (/ x y))]
   '[add-cube-cbrt     x                        (* (* (cbrt x) (cbrt x)) (cbrt x))]
   '[add-cbrt-cube     x                        (cbrt (* (* x x) x))]))

(define cubes-canonicalize
  (list
   '[cube-unmult       (* x (* x x))          (pow x 3)]))

; Exponentials
(define exp-expand
  (list
   '[add-exp-log  x                    (exp (log x))]
   '[add-log-exp  x                    (log (exp x))]))

(define exp-reduce
  (list
   '[rem-exp-log  (exp (log x))        x]
   '[rem-log-exp  (log (exp x))        x]))

(define exp-constants
  (list
   '[exp-0        (exp 0)              1]
   '[exp-1-e      (exp 1)              E]
   '[1-exp        1                    (exp 0)]
   '[e-exp-1      E                    (exp 1)]))

(define exp-distribute
  (list
   '[exp-sum      (exp (+ a b))        (* (exp a) (exp b))]
   '[exp-neg      (exp (neg a))          (/ 1 (exp a))]
   '[exp-diff     (exp (- a b))        (/ (exp a) (exp b))]))

(define exp-factor
  (list
   '[prod-exp     (* (exp a) (exp b))  (exp (+ a b))]
   '[rec-exp      (/ 1 (exp a))        (exp (neg a))]
   '[div-exp      (/ (exp a) (exp b))  (exp (- a b))]
   '[exp-prod     (exp (* a b))        (pow (exp a) b)]
   '[exp-sqrt     (exp (/ a 2))        (sqrt (exp a))]
   '[exp-cbrt     (exp (/ a 3))        (cbrt (exp a))]
   '[exp-lft-sqr  (exp (* a 2))        (* (exp a) (exp a))]
   '[exp-lft-cube (exp (* a 3))        (pow (exp a) 3)]))

; Powers
(define pow-reduce
  (list
   '[unpow-1        (pow a -1)                 (/ 1 a)]))

(define pow-reduce-fp-safe
  (list
   '[unpow1         (pow a 1)                  a]))

(define pow-reduce-fp-safe-nan
  (list
   '[unpow0         (pow a 0)                  1]
   '[pow-base-1     (pow 1 a)                  1]))

(define pow-expand-fp-safe
  (list
   '[pow1           a                           (pow a 1)]))

(define pow-canonicalize
  (list
   '[exp-to-pow      (exp (* (log a) b))        (pow a b)]
   '[pow-plus        (* (pow a b) a)            (pow a (+ b 1))]
   '[unpow1/2        (pow a 1/2)                (sqrt a)]
   '[unpow2          (pow a 2)                  (* a a)]
   '[unpow3          (pow a 3)                  (* (* a a) a)]
   '[unpow1/3        (pow a 1/3)                (cbrt a)]))

(define pow-transform
  (list
   '[pow-exp          (pow (exp a) b)             (exp (* a b))]
   '[pow-to-exp       (pow a b)                   (exp (* (log a) b))]
   '[pow-prod-up      (* (pow a b) (pow a c))     (pow a (+ b c))]
   '[pow-prod-down    (* (pow b a) (pow c a))     (pow (* b c) a)]
   '[pow-pow          (pow (pow a b) c)           (pow a (* b c))]
   '[pow-neg          (pow a (neg b))               (/ 1 (pow a b))]
   '[pow-flip         (/ 1 (pow a b))             (pow a (neg b))]
   '[pow-div          (/ (pow a b) (pow a c))     (pow a (- b c))]
   '[pow-sub          (pow a (- b c))             (/ (pow a b) (pow a c))]
   '[pow-unpow        (pow a (* b c))             (pow (pow a b) c)]
   '[unpow-prod-up    (pow a (+ b c))             (* (pow a b) (pow a c))]
   '[unpow-prod-down  (pow (* b c) a)             (* (pow b a) (pow c a))]
   '[pow1/2           (sqrt a)                    (pow a 1/2)]
   '[pow2             (* a a)                     (pow a 2)]
   '[pow1/3           (cbrt a)                    (pow a 1/3)]
   '[pow3             (* (* a a) a)               (pow a 3)]))

(define pow-transform-fp-safe-nan
  (list
   '[pow-base-0       (pow 0 a)                   0]))

(define pow-transform-fp-safe
  (list
   '[inv-pow          (/ 1 a)                     (pow a -1)]))

; Logarithms
(define log-distribute
  (list
   '[log-prod     (log (* a b))       (+ (log a) (log b))]
   '[log-div      (log (/ a b))       (- (log a) (log b))]
   '[log-rec      (log (/ 1 a))       (neg (log a))]
   '[log-pow      (log (pow a b))     (* b (log a))]))

(define log-distribute-fp-safe
  (list '[log-E        (log E)             1]))

(define log-factor
  (list
   '[sum-log      (+ (log a) (log b))  (log (* a b))]
   '[diff-log     (- (log a) (log b))  (log (/ a b))]
   '[neg-log      (neg (log a))          (log (/ 1 a))]))

; Trigonometry
(define trig-reduce 
  (list
   '[cos-sin-sum (+ (* (cos a) (cos a)) (* (sin a) (sin a))) 1]
   '[1-sub-cos   (- 1 (* (cos a) (cos a)))   (* (sin a) (sin a))]
   '[1-sub-sin   (- 1 (* (sin a) (sin a)))   (* (cos a) (cos a))]
   '[-1-add-cos  (+ (* (cos a) (cos a)) -1)  (neg (* (sin a) (sin a)))]
   '[-1-add-sin  (+ (* (sin a) (sin a)) -1)  (neg (* (cos a) (cos a)))]
   '[sub-1-cos   (- (* (cos a) (cos a)) 1)   (neg (* (sin a) (sin a)))]
   '[sub-1-sin   (- (* (sin a) (sin a)) 1)   (neg (* (cos a) (cos a)))]
   '[sin-PI/6    (sin (/ PI 6))        1/2]
   '[sin-PI/4    (sin (/ PI 4))        (/ (sqrt 2) 2)]
   '[sin-PI/3    (sin (/ PI 3))        (/ (sqrt 3) 2)]
   '[sin-PI/2    (sin (/ PI 2))        1]
   '[sin-PI      (sin PI)              0]
   '[sin-+PI     (sin (+ x PI))        (neg (sin x))]
   '[sin-+PI/2   (sin (+ x (/ PI 2)))  (cos x)]
   '[cos-PI/6    (cos (/ PI 6))        (/ (sqrt 3) 2)]
   '[cos-PI/4    (cos (/ PI 4))        (/ (sqrt 2) 2)]
   '[cos-PI/3    (cos (/ PI 3))        1/2]
   '[cos-PI/2    (cos (/ PI 2))        0]
   '[cos-PI      (cos PI)              -1]
   '[cos-+PI     (cos (+ x PI))        (neg (cos x))]
   '[cos-+PI/2   (cos (+ x (/ PI 2)))  (neg (sin x))]
   '[tan-PI/6    (tan (/ PI 6))        (/ 1 (sqrt 3))]
   '[tan-PI/4    (tan (/ PI 4))        1]
   '[tan-PI/3    (tan (/ PI 3))        (sqrt 3)]
   '[tan-PI      (tan PI)              0]
   '[tan-+PI     (tan (+ x PI))        (tan x)]
   '[tan-+PI/2   (tan (+ x (/ PI 2)))  (/ -1 (tan x))]
   '[hang-0p-tan (/ (sin a) (+ 1 (cos a)))     (tan (/ a 2))]
   '[hang-0m-tan (/ (neg (sin a)) (+ 1 (cos a))) (tan (/ (neg a) 2))]
   '[hang-p0-tan (/ (- 1 (cos a)) (sin a))     (tan (/ a 2))]
   '[hang-m0-tan (/ (- 1 (cos a)) (neg (sin a))) (tan (/ (neg a) 2))]
   '[hang-p-tan  (/ (+ (sin a) (sin b)) (+ (cos a) (cos b)))
                 (tan (/ (+ a b) 2))]
   '[hang-m-tan  (/ (- (sin a) (sin b)) (+ (cos a) (cos b)))
                 (tan (/ (- a b) 2))]))

(define trig-reduce-fp-sound
  (list
   '[sin-0       (sin 0)               0]
   '[cos-0       (cos 0)               1]
   '[tan-0       (tan 0)               0]))

(define trig-reduce-fp-sound-nan
  (list
   '[sin-neg     (sin (neg x))           (neg (sin x))]
   '[cos-neg     (cos (neg x))           (cos x)]
   '[tan-neg     (tan (neg x))           (neg (tan x))]))

(define trig-expand
  (list
   '[sin-sum     (sin (+ x y))             (+ (* (sin x) (cos y)) (* (cos x) (sin y)))]
   '[cos-sum     (cos (+ x y))             (- (* (cos x) (cos y)) (* (sin x) (sin y)))]
   '[tan-sum     (tan (+ x y))             (/ (+ (tan x) (tan y)) (- 1 (* (tan x) (tan y))))]
   '[sin-diff    (sin (- x y))             (- (* (sin x) (cos y)) (* (cos x) (sin y)))]
   '[cos-diff    (cos (- x y))             (+ (* (cos x) (cos y)) (* (sin x) (sin y)))]
   '[sin-2       (sin (* 2 x))
                 (* 2 (* (sin x) (cos x)))]
   '[sin-3       (sin (* 3 x))
                 (- (* 3 (sin x)) (* 4 (pow (sin x) 3)))]
   '[2-sin       (* 2 (* (sin x) (cos x)))
                 (sin (* 2 x))]
   '[3-sin       (- (* 3 (sin x)) (* 4 (pow (sin x) 3)))
                 (sin (* 3 x))]
   '[cos-2       (cos (* 2 x))
                 (- (* (cos x) (cos x)) (* (sin x) (sin x)))]
   '[cos-3       (cos (* 3 x))
                 (- (* 4 (pow (cos x) 3)) (* 3 (cos x)))]
   '[2-cos       (- (* (cos x) (cos x)) (* (sin x) (sin x)))
                 (cos (* 2 x))]
   '[3-cos       (- (* 4 (pow (cos x) 3)) (* 3 (cos x)))
                 (cos (* 3 x))]
   '[tan-2       (tan (* 2 x))             (/ (* 2 (tan x)) (- 1 (* (tan x) (tan x))))]
   '[2-tan       (/ (* 2 (tan x)) (- 1 (* (tan x) (tan x)))) (tan (* 2 x))]
   '[sqr-sin-a   (* (sin x) (sin x))       (- 1/2 (* 1/2 (cos (* 2 x))))]
   '[sqr-cos-a   (* (cos x) (cos x))       (+ 1/2 (* 1/2 (cos (* 2 x))))]
   '[diff-sin    (- (sin x) (sin y))       (* 2 (* (sin (/ (- x y) 2)) (cos (/ (+ x y) 2))))]
   '[diff-cos    (- (cos x) (cos y))       (* -2 (* (sin (/ (- x y) 2)) (sin (/ (+ x y) 2))))]
   '[sum-sin     (+ (sin x) (sin y))       (* 2 (* (sin (/ (+ x y) 2)) (cos (/ (- x y) 2))))]
   '[sum-cos     (+ (cos x) (cos y))       (* 2 (* (cos (/ (+ x y) 2)) (cos (/ (- x y) 2))))]
   '[cos-mult    (* (cos x) (cos y))       (/ (+ (cos (+ x y)) (cos (- x y))) 2)]
   '[sin-mult    (* (sin x) (sin y))       (/ (- (cos (- x y)) (cos (+ x y))) 2)]
   '[sin-cos-mult (* (sin x) (cos y))      (/ (+ (sin (- x y)) (sin (+ x y))) 2)]
   '[diff-atan   (- (atan x) (atan y))     (atan2 (- x y) (+ 1 (* x y)))]
   '[sum-atan    (+ (atan x) (atan y))     (atan2 (+ x y) (- 1 (* x y)))]
   '[tan-quot    (tan x)                   (/ (sin x) (cos x))]
   '[quot-tan    (/ (sin x) (cos x))       (tan x)]
   '[tan-hang-p  (tan (/ (+ a b) 2))
                 (/ (+ (sin a) (sin b)) (+ (cos a) (cos b)))]
   '[tan-hang-m  (tan (/ (- a b) 2))
                 (/ (- (sin a) (sin b)) (+ (cos a) (cos b)))]))

(define trig-expand-fp-safe
  (list
   '[sqr-sin-b   (* (sin x) (sin x))       (- 1 (* (cos x) (cos x)))]
   '[sqr-cos-b   (* (cos x) (cos x))       (- 1 (* (sin x) (sin x)))]))

(define trig-inverses
  (list
   '[sin-asin    (sin (asin x))         x]
   '[cos-acos    (cos (acos x))         x]
   '[tan-atan    (tan (atan x))         x]
   '[atan-tan    (atan (tan x))         (remainder x PI)]
   '[asin-sin    (asin (sin x))         (- (fabs (remainder (+ x (/ PI 2)) (* 2 PI))) (/ PI 2))]
   '[acos-cos    (acos (cos x))         (fabs (remainder x (* 2 PI)))]))

(define trig-inverses-simplified
  (list
   '[atan-tan-s  (atan (tan x))         x]
   '[asin-sin-s  (asin (sin x))         x]
   '[acos-cos-s  (acos (cos x))         x]))

(define atrig-expand 
  (list
   '[cos-asin    (cos (asin x))         (sqrt (- 1 (* x x)))]
   '[tan-asin    (tan (asin x))         (/ x (sqrt (- 1 (* x x))))]
   '[sin-acos    (sin (acos x))         (sqrt (- 1 (* x x)))]
   '[tan-acos    (tan (acos x))         (/ (sqrt (- 1 (* x x))) x)]
   '[sin-atan    (sin (atan x))         (/ x (sqrt (+ 1 (* x x))))]
   '[cos-atan    (cos (atan x))         (/ 1 (sqrt (+ 1 (* x x))))]
   '[asin-acos   (asin x)               (- (/ PI 2) (acos x))]
   '[acos-asin   (acos x)               (- (/ PI 2) (asin x))]
   '[asin-neg    (asin (neg x))           (neg (asin x))]
   '[acos-neg    (acos (neg x))           (- PI (acos x))]
   '[atan-neg    (atan (neg x))           (neg (atan x))]))

; Hyperbolic trigonometric functions
(define htrig-reduce 
  (list
   '[sinh-def    (sinh x)               (/ (- (exp x) (exp (neg x))) 2)]
   '[cosh-def    (cosh x)               (/ (+ (exp x) (exp (neg x))) 2)]
   '[tanh-def-a  (tanh x)               (/ (- (exp x) (exp (neg x))) (+ (exp x) (exp (neg x))))]
   '[tanh-def-b  (tanh x)               (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1))]
   '[tanh-def-c  (tanh x)               (/ (- 1 (exp (* -2 x))) (+ 1 (exp (* -2 x))))]
   '[sinh-cosh   (- (* (cosh x) (cosh x)) (* (sinh x) (sinh x))) 1]
   '[sinh-+-cosh (+ (cosh x) (sinh x))  (exp x)]
   '[sinh---cosh (- (cosh x) (sinh x))  (exp (neg x))]))

(define htrig-expand 
  (list
   '[sinh-undef  (- (exp x) (exp (neg x)))                       (* 2 (sinh x))]
   '[cosh-undef  (+ (exp x) (exp (neg x)))                       (* 2 (cosh x))]
   '[tanh-undef  (/ (- (exp x) (exp (neg x))) (+ (exp x) (exp (neg x)))) (tanh x)]
   '[cosh-sum    (cosh (+ x y))         (+ (* (cosh x) (cosh y)) (* (sinh x) (sinh y)))]
   '[cosh-diff   (cosh (- x y))         (- (* (cosh x) (cosh y)) (* (sinh x) (sinh y)))]
   '[cosh-2      (cosh (* 2 x))         (+ (* (sinh x) (sinh x)) (* (cosh x) (cosh x)))]
   '[cosh-1/2    (cosh (/ x 2))         (sqrt (/ (+ (cosh x) 1) 2))]
   '[sinh-sum    (sinh (+ x y))         (+ (* (sinh x) (cosh y)) (* (cosh x) (sinh y)))]
   '[sinh-diff   (sinh (- x y))         (- (* (sinh x) (cosh y)) (* (cosh x) (sinh y)))]
   '[sinh-2      (sinh (* 2 x))         (* 2 (* (sinh x) (cosh x)))]
   '[sinh-1/2    (sinh (/ x 2))         (/ (sinh x) (sqrt (* 2 (+ (cosh x) 1))))]
   '[tanh-sum    (tanh (+ x y))         (/ (+ (tanh x) (tanh y)) (+ 1 (* (tanh x) (tanh y))))]
   '[tanh-2      (tanh (* 2 x))         (/ (* 2 (tanh x)) (+ 1 (* (tanh x) (tanh x))))]
   '[tanh-1/2    (tanh (/ x 2))         (/ (sinh x) (+ (cosh x) 1))]
   '[tanh-1/2*   (tanh (/ x 2))         (/ (- (cosh x) 1) (sinh x))]
   '[sum-sinh    (+ (sinh x) (sinh y))  (* 2 (* (sinh (/ (+ x y) 2)) (cosh (/ (- x y) 2))))]
   '[sum-cosh    (+ (cosh x) (cosh y))  (* 2 (* (cosh (/ (+ x y) 2)) (cosh (/ (- x y) 2))))]
   '[diff-sinh   (- (sinh x) (sinh y))  (* 2 (* (cosh (/ (+ x y) 2)) (sinh (/ (- x y) 2))))]
   '[diff-cosh   (- (cosh x) (cosh y))  (* 2 (* (sinh (/ (+ x y) 2)) (sinh (/ (- x y) 2))))]))

(define htrig-expand-fp-safe
  (list
   '[sinh-neg    (sinh (neg x))           (neg (sinh x))]
   '[sinh-0      (sinh 0)               0]
   '[cosh-neg    (cosh (neg x))           (cosh x)]
   '[cosh-0      (cosh 0)               1]))

(define ahtrig-expand
  (list
   '[asinh-def   (asinh x)              (log (+ x (sqrt (+ (* x x) 1))))]
   '[acosh-def   (acosh x)              (log (+ x (sqrt (- (* x x) 1))))]
   '[atanh-def   (atanh x)              (/ (log (/ (+ 1 x) (- 1 x))) 2)]
   '[acosh-2     (acosh (- (* 2 (* x x)) 1)) (* 2 (acosh x))]
   '[asinh-2     (acosh (+ (* 2 (* x x)) 1)) (* 2 (asinh x))]
   '[sinh-asinh  (sinh (asinh x))       x]
   '[sinh-acosh  (sinh (acosh x))       (sqrt (- (* x x) 1))]
   '[sinh-atanh  (sinh (atanh x))       (/ x (sqrt (- 1 (* x x))))]
   '[cosh-asinh  (cosh (asinh x))       (sqrt (+ (* x x) 1))]
   '[cosh-acosh  (cosh (acosh x))       x]
   '[cosh-atanh  (cosh (atanh x))       (/ 1 (sqrt (- 1 (* x x))))]
   '[tanh-asinh  (tanh (asinh x))       (/ x (sqrt (+ 1 (* x x))))]
   '[tanh-acosh  (tanh (acosh x))       (/ (sqrt (- (* x x) 1)) x)]
   '[tanh-atanh  (tanh (atanh x))       x]))

; Specialized numerical functions
(define special-numerical-reduce 
  (list
   '[expm1-def   (- (exp x) 1)              (expm1 x)]
   '[log1p-def   (log (+ 1 x))              (log1p x)]
   '[log1p-expm1 (log1p (expm1 x))          x]
   '[expm1-log1p (expm1 (log1p x))          x]
   '[hypot-def   (sqrt (+ (* x x) (* y y))) (hypot x y)]
   '[hypot-1-def (sqrt (+ 1 (* y y)))       (hypot 1 y)]
   '[fma-def     (+ (* x y) z)              (fma x y z)]
   '[fma-neg     (- (* x y) z)              (fma x y (neg z))]
   '[fma-udef    (fma x y z)                (+ (* x y) z)]))

(define special-numerical-expand 
  (list
   '[expm1-udef    (expm1 x)      (- (exp x) 1)]
   '[log1p-udef    (log1p x)      (log (+ 1 x))]
   '[log1p-expm1-u x              (log1p (expm1 x))]
   '[expm1-log1p-u x              (expm1 (log1p x))]
   '[hypot-udef    (hypot x y)    (sqrt (+ (* x x) (* y y)))]))

(define numerics-papers
  (list
   ;  "Further Analysis of Kahan's Algorithm for
   ;   the Accurate Computation of 2x2 Determinants"
   ;  Jeannerod et al., Mathematics of Computation, 2013
   ;
   ;  a * b - c * d  ===> fma(a, b, -(d * c)) + fma(-d, c, d * c)
   '[prod-diff    (- (* a b) (* c d))
                  (+ (fma a b (neg (* d c)))
                     (fma (neg d) c (* d c)))]))

(define bool-reduce 
  (list
   '[not-true     (not TRUE)       FALSE]
   '[not-false    (not FALSE)      TRUE]
   '[not-not      (not (not a))    a]
   '[not-and      (not (and a b))  (or  (not a) (not b))]
   '[not-or       (not (or  a b))  (and (not a) (not b))]
   '[and-true-l   (and TRUE a)     a]
   '[and-true-r   (and a TRUE)     a]
   '[and-false-l  (and FALSE a)    FALSE]
   '[and-false-r  (and a FALSE)    FALSE]
   '[and-same     (and a a)        a]
   '[or-true-l    (or TRUE a)      TRUE]
   '[or-true-r    (or a TRUE)      TRUE]
   '[or-false-l   (or FALSE a)     a]
   '[or-false-r   (or a FALSE)     a]
   '[or-same      (or a a)         a]))

(define compare-reduce
  (list
   '[lt-same      (<  x x)         FALSE]
   '[gt-same      (>  x x)         FALSE]
   '[lte-same     (<= x x)         TRUE]
   '[gte-same     (>= x x)         TRUE]
   '[not-lt       (not (<  x y))   (>= x y)]
   '[not-gt       (not (>  x y))   (<= x y)]
   '[not-lte      (not (<= x y))   (>  x y)]
   '[not-gte      (not (>= x y))   (<  x y)]))

(define branch-reduce-rules
  (list
   '[if-true        (if TRUE x y)       x]
   '[if-false       (if FALSE x y)      y]
   '[if-same        (if a x x)          x]
   '[if-not         (if (not a) x y)    (if a y x)]
   '[if-if-or       (if a x (if b x y)) (if (or a b) x y)]
   '[if-if-or-not   (if a x (if b y x)) (if (or a (not b)) x y)]
   '[if-if-and      (if a (if b x y) y) (if (and a b) x y)]
   '[if-if-and-not  (if a (if b y x) y) (if (and a (not b)) x y)]))

(define erf-rules
  (list
   '[erf-odd          (erf (neg x))          (neg (erf x))]
   '[erf-erfc         (erfc x)             (- 1 (erf x))]
   '[erfc-erf         (erf x)              (- 1 (erfc x))]))

(define all-rules
  (append
   commutativity
   associativity
   identity
   counting
   distributivity
   distributivity-fp-safe
   difference-of-squares-canonicalize
   difference-of-squares-flip
   id-reduce
   id-reduce-fp-safe-nan
   id-reduce-fp-safe
   nan-transform-fp-safe
   id-transform
   id-transform-fp-safe
   difference-of-cubes
   fractions-distribute
   fractions-transform
   squares-reduce
   squares-reduce-fp-sound
   squares-transform
   cubes-reduce
   cubes-distribute
   cubes-transform
   cubes-canonicalize
   exp-expand
   exp-reduce
   exp-constants
   exp-distribute
   exp-factor
   pow-reduce
   pow-reduce-fp-safe
   pow-reduce-fp-safe-nan
   pow-expand-fp-safe
   pow-canonicalize
   pow-transform
   pow-transform-fp-safe-nan
   pow-transform-fp-safe
   log-distribute
   log-distribute-fp-safe
   log-factor
   trig-reduce
   trig-reduce-fp-sound
   trig-reduce-fp-sound-nan
   trig-expand
   trig-expand-fp-safe
   trig-inverses
   trig-inverses-simplified
   atrig-expand
   htrig-reduce
   htrig-expand
   htrig-expand-fp-safe
   ahtrig-expand
   special-numerical-reduce
   special-numerical-expand
   numerics-papers
   bool-reduce
   compare-reduce
   branch-reduce-rules
   erf-rules))

(define all-rules-by-category
  (list
   (cons "commutativity" commutativity)
   (cons "associativity" associativity)
   (cons "identity" identity)
   (cons "counting" counting)
   (cons "distributivity" distributivity)
   (cons "distributivity-fp-safe" distributivity-fp-safe)
   (cons "difference-of-squares-canonicalize" difference-of-squares-canonicalize)
   (cons "difference-of-squares-flip" difference-of-squares-flip)
   (cons "id-reduce" id-reduce)
   (cons "id-reduce-fp-safe-nan" id-reduce-fp-safe-nan)
   (cons "id-reduce-fp-safe" id-reduce-fp-safe)
   (cons "nan-transform-fp-safe" nan-transform-fp-safe)
   (cons "id-transform" id-transform)
   (cons "id-transform-fp-safe" id-transform-fp-safe)
   (cons "difference-of-cubes" difference-of-cubes)
   (cons "fractions-distribute" fractions-distribute)
   (cons "fractions-transform" fractions-transform)
   (cons "squares-reduce" squares-reduce)
   (cons "squares-reduce-fp-sound" squares-reduce-fp-sound)
   (cons "squares-transform" squares-transform)
   (cons "cubes-reduce" cubes-reduce)
   (cons "cubes-distribute" cubes-distribute)
   (cons "cubes-transform" cubes-transform)
   (cons "cubes-canonicalize" cubes-canonicalize)
   (cons "exp-expand" exp-expand)
   (cons "exp-reduce" exp-reduce)
   (cons "exp-constants" exp-constants)
   (cons "exp-distribute" exp-distribute)
   (cons "exp-factor" exp-factor)
   (cons "pow-reduce" pow-reduce)
   (cons "pow-reduce-fp-safe" pow-reduce-fp-safe)
   (cons "pow-reduce-fp-safe-nan" pow-reduce-fp-safe-nan)
   (cons "pow-expand-fp-safe" pow-expand-fp-safe)
   (cons "pow-canonicalize" pow-canonicalize)
   (cons "pow-transform" pow-transform)
   (cons "pow-transform-fp-safe-nan" pow-transform-fp-safe-nan)
   (cons "pow-transform-fp-safe" pow-transform-fp-safe)
   (cons "log-distribute" log-distribute)
   (cons "log-distribute-fp-safe" log-distribute-fp-safe)
   (cons "log-factor" log-factor)
   (cons "trig-reduce" trig-reduce)
   (cons "trig-reduce-fp-sound" trig-reduce-fp-sound) 
   (cons "trig-reduce-fp-sound-nan" trig-reduce-fp-sound-nan)
   (cons "trig-expand" trig-expand)
   (cons "trig-expand-fp-safe" trig-expand-fp-safe)
   (cons "trig-inverses" trig-inverses)
   (cons "trig-inverses-simplified" trig-inverses-simplified)
   (cons "atrig-expand" atrig-expand)
   (cons "htrig-reduce" htrig-reduce)
   (cons "htrig-expand" htrig-expand)
   (cons "htrig-expand-fp-safe" htrig-expand-fp-safe)
   (cons "ahtrig-expand" ahtrig-expand)
   (cons "special-numerical-reduce" special-numerical-reduce)
   (cons "special-numerical-expand" special-numerical-expand)
   (cons "numerics-papers" numerics-papers)
   (cons "bool-reduce" bool-reduce)
   (cons "compare-reduce" compare-reduce)
   (cons "branch-reduce-rules" branch-reduce-rules)
   (cons  "erf-rules" erf-rules)))
