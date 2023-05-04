#lang racket

(require json)

(define features '(no-expansive-bool no-xor))
(define quiet-mode? #f)

(define (log! msg . args)
  (unless quiet-mode? (apply printf msg args)))

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

(define bool-op-table '(("&" . "and")
                        ("|" . "or")
                        ("~" . "not")
                        ("true" . "(TRUE)")
                        ("false" . "(FALSE)")))

(define rational-op-table '(("~" . "neg")))

(define/contract (string-replace* str changes)
  (-> string? (listof (cons/c string? string?)) string?)
  (let loop ([str str] [changes changes])
    (match changes
      [(? null?) str]
      [_ (let ([change (car changes)])
           (loop (string-replace str (car change) (cdr change)) (cdr changes)))])))

(define (parse-ruler-rule lhs rhs table)
  (define (ruler-string-expr->expr str)
    (call-with-input-string (string-replace* str table) read))

  (define (parse-expr expr)
    (define vars (mutable-set))
    (values
      (let loop ([expr (ruler-string-expr->expr expr)])
        (match expr
          [(list 'if c ift iff)
           (list 'if (list '!= (loop c) 0) (loop ift) (loop iff))]
          [(list 'sqr arg)
           (define arg* (loop arg))
           (list '* arg* arg*)]
          [(list op args ...)
           (cons op (map loop args))]
          [(? number?) expr]
          [(? symbol?)
           (define str-repr (symbol->string expr))
           (cond
             [(eq? (string-ref str-repr 0) #\?)
              (set-add! vars (substring str-repr 1))
              (string->symbol (substring str-repr 1))]
             [else
              (list expr)])]))
      (for/set ([s (in-mutable-set vars)]) s)))

  (define (expr-has-op? expr)
    (match expr
      [(list op head rest ...) #t]
      [_ #f]))

  (define-values (lhs* lhs-vars) (parse-expr lhs))
  (define-values (rhs* rhs-vars) (parse-expr rhs))
  (define rule (list lhs* rhs* (and (expr-has-op? lhs*) 'simplify)))
  (values rule (set-union lhs-vars rhs-vars)))

(define (ops-in-expr expr)
  (define ops (mutable-set))
  (let loop ([expr expr])
    (match expr
      [(list op args ...)
       (set-add! ops op)
       (for-each loop args)]
      [_
       (void)]))
  ops)

(define (make-ruleset name groups var-ctx rules)
  (log! "  Registering ruleset `~a` ...\n" name)
  (log! "   Groups: ~a\n" groups)
  (log! "   Vars:   ~a\n" var-ctx)
  (log! "   Rules:  ~a\n" (length rules))
  `(register-ruleset*!
    ,name ,groups ,var-ctx
    ,(for/list ([rule (in-list rules)] [i (in-naturals 1)])
      (match-define (list lhs rhs _) rule)
      (define rule-name (string->symbol (format "~a-~a" name i)))
      (log! "    ~a: ~a => ~a\n" rule-name lhs rhs)
      (list rule-name lhs rhs))))

;;
;;  Core
;;

(define (get-rules json keys baseline?)
  (for/list ([key (in-list keys)])
    (match-define (list spec baseline groups type op-table) key)
    (let/ec return
      (for ([entry (in-list json)])
        (when (and (hash? entry)
                   (hash-has-key? entry 'enumo_spec_name)
                   (hash-has-key? entry 'baseline_name)
                   (equal? (hash-ref entry 'enumo_spec_name) spec)
                   (equal? (hash-ref entry 'baseline_name) baseline))
          (return
            (list
              spec
              (if baseline?
                  (let ([derivability (hash-ref entry 'enumo_to_baseline_all)])
                    (append (hash-ref derivability 'enumo_derives_baseline_derivable)
                            (hash-ref derivability 'enumo_derives_baseline_underivable)))
                  (hash-ref (hash-ref entry 'rules) 'rules))
              groups
              type
              op-table))))
      (error 'get-rules "missing entry (~a . ~a)" spec baseline))))

(define (get-rule-jsons config jsonfile)
  (define json (read-json jsonfile))
  (match config
    ['enumo
     (define keys `(("bool" "oopsla" (bool) bool ,bool-op-table)
                    ("rational_best" "rational_best" (rational) real ,rational-op-table)
                    ("exponential" "herbie" (exponential) real ,rational-op-table)
                    ("trig" "herbie" (trig) real ,rational-op-table)))
     (define baseline? #f)
     (get-rules json keys baseline?)]
    ['enumo-rat
     (define keys `(("rational_best" "rational_best" (rational) real ,rational-op-table)))
     (define baseline? #f)
     (get-rules json keys baseline?)]
    ['enumo-no-ff
     (define keys `(("rational_best" "rational_best" (rational) real ,rational-op-table)))
     (define baseline? #f)
     (get-rules json keys baseline?)]
    ['ruler
     (define keys `(("rational_best" "oopsla" (rational) real ,rational-op-table)))
     (define baseline? #t)
     (get-rules json keys baseline?)]
    [_
     (error 'get-rule-jsons "unsupported configuration ~a" config)]))

;;
;;  File handling
;;

(define (emit-rules! jsons outfile)
  (for ([entry (in-list jsons)])
    (log! "  Parsing rules ...\n")
    (match-define (list name json groups type op-table) entry)
    (define vars (mutable-set))
    (define rules
      (for/fold ([rules '()] #:result (reverse rules))
                ([rule (in-list json)] [counter (in-naturals 1)])
        (match-define (list lhs rhs) (string-split rule " ==> "))
        (define-values (rule* rule-vars) (parse-ruler-rule lhs rhs op-table))
        (set-union! vars rule-vars)
        (cond
          [(and (set-member? features 'no-xor)
                (or (set-member? (ops-in-expr (first rule*)) '^)
                    (set-member? (ops-in-expr (second rule*)) '^)))
           rules]
          [else
           (cons rule* rules)])))

    (define-values (simplify non-simplify)
      (let-values ([(simplify non-simplify) (partition (λ (r) (eq? (third r) 'simplify)) rules)])
        (cond
          [(and (eq? type 'bool) (set-member? features 'no-expansive-bool))
           (values simplify (filter (λ (r) (not (symbol? (first r)))) non-simplify))]
          [else
           (values simplify non-simplify)])))

    (define var-ctx (for/list ([v (in-set vars)]) (cons (string->symbol v) type)))
    (pretty-write (make-ruleset name groups var-ctx non-simplify) outfile)
    (newline outfile)
    (pretty-write (make-ruleset (format "~a-simplify" name) (cons 'simplify groups) var-ctx simplify) outfile)
    (newline outfile)
    (log! "  Done\n")))

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
  (define jsons (get-rule-jsons config jsonfile))

  (emit-prelude! outfile)
  (emit-rules! jsons outfile)
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
