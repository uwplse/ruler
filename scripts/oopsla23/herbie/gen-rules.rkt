#lang racket

(define rules-prelude '(
;; Arithmetic identities for rewriting programs.

(require "../common.rkt" "../errors.rkt" "types.rkt" "syntax.rkt" "sugar.rkt")

(provide *rules* *simplify-rules* *fp-safe-simplify-rules* (struct-out rule))

(module+ internals
  (provide define-ruleset define-ruleset* register-ruleset! *rulesets*))

;; Rulesets
(define *rulesets* (make-parameter '()))

;; Rules
(define *rules* (make-parameter '()))
(define *simplify-rules* (make-parameter '()))
(define *fp-safe-simplify-rules* (make-parameter '()))

;; Note on rules
;; fp-safe-simplify ⊂ simplify ⊂ all
;;
;; all                    requires at least one tag of an active group of rules
;; simplify               same req. as all + 'simplify' tag
;; fp-safe-simplify       same req. as simplify + 'fp-safe' tag ('fp-safe' does not imply 'simplify')
;;

(define (update-rules rules groups)
  (when (ormap (curry flag-set? 'rules) groups)             ; update all
    (*rules* (append (*rules*) rules))
    (when (set-member? groups 'simplify)                    ; update simplify
      (*simplify-rules* (append (*simplify-rules*) rules))
      (when (set-member? groups 'fp-safe)                   ; update fp-safe
        (*fp-safe-simplify-rules*
          (append (*fp-safe-simplify-rules*) rules))))))

;; Rule struct

(struct rule (name input output itypes otype)
        ;; Input and output are patterns
        ;; itypes is a mapping, variable name -> representation
        ;; otype is a representation
        #:methods gen:custom-write
        [(define (write-proc rule port mode)
           (fprintf port "#<rule ~a>" (rule-name rule)))])

(define (rule-ops-supported? rule)
  (define (ops-in-expr expr)
    (cond
      [(list? expr)
       (and (impl-exists? (car expr))
            (for/and ([subexpr (cdr expr)])
              (ops-in-expr subexpr)))]
      [else true]))
  (ops-in-expr (rule-output rule)))

(register-reset
 #:priority 10 ; Must be higher than priority for pruning operators
 (λ ()
   (*rulesets*
    (for/list ([ruleset (*rulesets*)])
      (match-define (list rules groups types) ruleset)
      (list (filter rule-ops-supported? rules) groups types)))))

;;
;;  Rule loading
;;

(define-values (type-of-rule repr-of-rule)
  (let () ; `let` not `begin` since these are expanded in different phases
    (define ((type/repr-of-rule get-info name) input output ctx)
      (let loop ([input input] [output output])
        (cond [(list? input)    (if (equal? (car input) 'if)
                                    ; special case for 'if'
                                    ; return the 'type/repr-of-rule' of the ift branch
                                    (loop (caddr input) output)
                                    (get-info (car input) 'otype))]
              [(list? output)   (if (equal? (car output) 'if)
                                    ; special case for 'if'
                                    ; return the 'type/repr-of-rule' of the ift branch
                                    (loop input (caddr output))
                                    (get-info (car output) 'otype))]
              ;; fallback: if symbol, check the ctx for the type
              [(symbol? input)  (dict-ref ctx input)]
              [(symbol? output) (dict-ref ctx output)]
              [else             (error name "could not compute type of rule ~a -> ~a"
                                            input output)])))
    (values (type/repr-of-rule real-operator-info 'type-of-rule)
            (type/repr-of-rule operator-info 'repr-of-rule))))

;; Rulesets defined by reprs. These rulesets are unique
(define (register-ruleset! name groups var-ctx rules)
  (define rules*
    (for/list ([r rules])
      (match-define (list rname input output) r)
      (rule rname input output var-ctx
            (repr-of-rule input output var-ctx))))
  (*rulesets* (cons (list rules* groups var-ctx) (*rulesets*)))
  (update-rules rules* groups))

(define-syntax define-ruleset
  (syntax-rules ()
   [(define-ruleset name groups [rname input output] ...)
    (define-ruleset name groups #:type () [rname input output] ...)]
   [(define-ruleset name groups #:type ([var type] ...) [rname input output] ...)
    (register-ruleset! 'name 'groups `((var . ,(get-representation 'type)) ...)
                       '((rname input output) ...))]))

(define (register-ruleset*! name groups var-ctx rules)
  (define rules*
    (for/list ([ru (in-list rules)])
      (match-define (list rname input output) ru)
      (rule rname input output var-ctx
            (type-of-rule input output var-ctx))))
  (*rulesets* (cons (list rules* groups var-ctx) (*rulesets*)))
  (update-rules rules* groups))

(define-syntax define-ruleset*
  (syntax-rules ()
   [(define-ruleset* name groups [rname input output] ...)
    (define-ruleset* name groups #:type () [rname input output] ...)]
   [(define-ruleset* name groups #:type ([var type] ...) [rname input output] ...)
    (register-ruleset*! 'name 'groups `((var . type) ...)
                        '((rname input output) ...))])))
)

(define numerics-rulesets '(
; Specialized numerical functions
(define-ruleset* special-numerical-reduce (numerics simplify)
  #:type ([x real] [y real] [z real])
  [expm1-def   (- (exp x) 1)              (expm1 x)]
  [log1p-def   (log (+ 1 x))              (log1p x)]
  [log1p-expm1 (log1p (expm1 x))          x]
  [expm1-log1p (expm1 (log1p x))          x]
  [hypot-def   (sqrt (+ (* x x) (* y y))) (hypot x y)]
  [hypot-1-def (sqrt (+ 1 (* y y)))       (hypot 1 y)]
  [fma-def     (+ (* x y) z)              (fma x y z)]
  [fma-neg     (- (* x y) z)              (fma x y (neg z))]
  [fma-udef    (fma x y z)                (+ (* x y) z)])

(define-ruleset* special-numerical-expand (numerics)
  #:type ([x real] [y real])
  [expm1-udef    (expm1 x)      (- (exp x) 1)]
  [log1p-udef    (log1p x)      (log (+ 1 x))]
  [log1p-expm1-u x              (log1p (expm1 x))]
  [expm1-log1p-u x              (expm1 (log1p x))]
  [hypot-udef    (hypot x y)    (sqrt (+ (* x x) (* y y)))])

(define-ruleset* numerics-papers (numerics)
  #:type ([a real] [b real] [c real] [d real])
  ;  "Further Analysis of Kahan's Algorithm for
  ;   the Accurate Computation of 2x2 Determinants"
  ;  Jeannerod et al., Mathematics of Computation, 2013
  ;
  ;  a * b - c * d  ===> fma(a, b, -(d * c)) + fma(-d, c, d * c)
  [prod-diff    (- (* a b) (* c d))
                (+ (fma a b (neg (* d c)))
                  (fma (neg d) c (* d c)))]))
)

(define expansive-rules-fix '(
(define (expansive-rule-fix!)
(define ops (mutable-set))
(define (letter-at idx)
  (string->symbol (string (integer->char (+ idx 97)))))
(define (ops-in-expr expr)
  (let loop ([expr expr])
    (match expr
      [(list op arg args ...)
      (set-add! ops op)
      (for-each loop (cons arg args))]
      [_
      (void)])))
(for ([rs (in-list (*rulesets*))])
  (match-define (list rules groups types) rs)
  (for ([r (in-list rules)])
    (ops-in-expr (rule-input r))
    (ops-in-expr (rule-output r))))
(define ops*
  (reap [sow]
    (for ([op (in-set ops)])
      (unless (equal? op 'if)
        (when (equal? (real-operator-info op 'otype) 'real)
          (sow op))))))
(register-ruleset*!
  'oliver-expand '(arithmetic) '((a . real) (b . real) (c . real))
  (for/list ([op (in-list ops*)] [i (in-naturals)])
    (define len (length (real-operator-info op 'itype)))
    (define lhs (cons op (for/list ([i (in-range len)]) (letter-at i))))
    (list (string->symbol (format "oliver-~a" i))
          lhs
          (list '+ lhs 0)))))

(expansive-rule-fix!))
)

;;
;;  Rule generation
;;

(define (get-config config)
  (match config
    ['enumo
     #f]
    ['enumo-rat
     #f]
    ['enumo-no-ff
     #f]
    ['ruler
     #f]
    [_
     (error 'get-config "unexpected configuration ~a" config)]))

;;
;;  File handling
;;

(define (emit-rules! outfile)
  (void))

(define (emit-prelude! outfile)
  (displayln "#lang racket\n" outfile)
  (for ([line (in-list rules-prelude)])
    (pretty-write line outfile)
    (newline outfile)))

(define (emit-ending! outfile)
  (for ([line (in-list numerics-rulesets)])
    (pretty-write line outfile)
    (newline outfile))
  (for ([line (in-list expansive-rules-fix)])
    (pretty-write line outfile)
    (newline outfile)))

(define (gen-rules-rkt config json-path out-path)
  (printf "Generating rules.rkt for ~a from ~a with result at ~a\n" config json-path out-path)
  (define jsonfile (open-input-file json-path))
  (define outfile (open-output-file #:exists 'replace out-path))
  (define cfg (get-config config))

  (emit-prelude! outfile)
  (emit-rules! outfile)
  (emit-ending! outfile)

  (close-input-port jsonfile)
  (close-output-port outfile)
  (void))

;;
;;  Command line
;;

(module+ main
  (command-line
    #:args (config json-path rkt-path)
    (case config
      [("main") (error 'gen-rules.rkt "Configuration not supported ~a" config)]
      [("enumo") (gen-rules-rkt (string->symbol config)
                                (string->path json-path)
                                (string->path rkt-path))]
      [("enumo-rat") (gen-rules-rkt (string->symbol config)
                                    (string->path json-path)
                                    (string->path rkt-path))]
      [("enumo-no-ff") (gen-rules-rkt (string->symbol config)
                                      (string->path json-path)
                                      (string->path rkt-path))]
      [("ruler") (gen-rules-rkt (string->symbol config)
                                (string->path json-path)
                                (string->path rkt-path))]
      [else (error 'gen-rules.rkt "Unknown configuration ~a" config)])))
