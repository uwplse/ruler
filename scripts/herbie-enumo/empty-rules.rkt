#lang racket

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
                        '((rname input output) ...))]))
