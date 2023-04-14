#lang racket

(require "../syntax/types.rkt" "../syntax/syntax.rkt" "../syntax/rules.rkt")
(require "../common.rkt" "../programs.rkt" "../timeline.rkt"
         "../errors.rkt" "../alternative.rkt" "egg-herbie.rkt")

(provide simplify-expr simplify-batch get-proof
         (struct-out simplify-input))

(module+ test
  (require rackunit "../load-plugin.rkt")
  (load-herbie-plugins))

;; The input and output of simplify- simplify is re-run when proofs are needed
(struct simplify-input (exprs proofs rules precompute?))

(define/contract (simplify-expr expr #:rules rls #:precompute [precompute? false] #:prove [prove? false])
  (->* (expr? #:rules (listof rule?)) (#:precompute boolean?) expr?)
  (last (first (simplify-batch (list expr) #:rules rls #:precompute precompute?))))

;; for each expression, returns a list of simplified versions corresponding to egraph iterations
;; the last expression is the simplest unless something went wrong due to unsoundness
;; if the input specifies proofs, it instead returns proofs for these expressions
(define/contract (simplify-batch input)
  (->* ((struct/c simplify-input
                  (listof expr?)
                  (listof (cons/c expr? expr?))
                  (listof rule?)
                  boolean?))
         (listof (listof expr?)))

  (timeline-push! 'inputs (map ~a (simplify-input-exprs input)))

  (define results
    (run-simplify-input
     input
     (lambda (egg-graph node-ids iter-data)
       (map (lambda (id)
              (for/list ([iter (in-range (length iter-data))])
                (egraph-get-simplest egg-graph id iter)))
            node-ids))))

  (define out
    (for/list ([result results] [expr (simplify-input-exprs input)])
      (remove-duplicates (cons expr result))))
  (timeline-push! 'outputs (map ~a (apply append out)))
    
  out)

(define (run-simplify-input input egraph-func)
  (define exprs (simplify-input-exprs input))
  (define precompute? (simplify-input-precompute? input))
  (define proofs (simplify-input-proofs input))
  (define rules (simplify-input-rules input))
  
  (timeline-push! 'method "egg-herbie")

  (define vars (mutable-set))
  (define (vars-in-expr expr)
    (let loop ([expr expr])
      (match expr
        [(list op args ...)
         (for-each loop args)]
        [(? symbol?)
         (set-add! vars expr)]
        [else
         (void)])))
  (for-each vars-in-expr exprs)

  (with-egraph
   (lambda (egg-graph)
     (define node-ids (map (curry egraph-add-expr egg-graph) exprs))
     (for ([v (in-set vars)])
       (define repr (context-lookup (*context*) v))
       (define add (get-parametric-operator '+ repr repr))
       (define expr (list add v 0))
       (egraph-add-expr egg-graph expr))
     (define iter-data (egraph-run-rules egg-graph (*node-limit*) rules node-ids (and precompute? true)))
        
     (when (egraph-is-unsound-detected egg-graph)
       (warn 'unsound-rules #:url "faq.html#unsound-rules"
             "Unsound rule application detected in e-graph. Results from simplify may not be sound."))

     (egraph-func egg-graph node-ids iter-data))))

(define (get-proof input start end)
  (run-simplify-input
    input
    (lambda (egg-graph node-ids iter-data)
      (define proof (egraph-get-proof egg-graph start end))
      (when (null? proof)
        (error (format "Failed to produce proof for ~a to ~a" start end)))
      proof)))

(module+ test
  (require "../syntax/types.rkt" "../syntax/rules.rkt")

  ;; set parameters
  (define vars '(x a b c))
  (*context* (make-debug-context vars))
  (*needed-reprs* (list (get-representation 'binary64)
                        (get-representation 'binary32)))
  (define all-simplify-rules (*simplify-rules*))

  ;; check that no rules in simplify match on bare variables
  ;; this would be bad because we don't want to match type-specific operators on a value of a different type
  (for ([rule all-simplify-rules])
    (check-true
     (not (symbol? (rule-input rule)))
     (string-append "Rule failed: " (symbol->string (rule-name rule)))))
  
  (define (test-simplify . args)
    (map last (simplify-batch (simplify-input args empty (*simplify-rules*) true))))

  (define test-exprs
    #hash([1 . 1]
          [0 . 0]
          [(+.f64 1 0) . 1]
          [(+.f64 1 5) . 6]
          [(+.f64 x 0) . x]
          [(-.f64 x 0) . x]
          [(*.f64 x 1) . x]
          [(/.f64 x 1) . x]
          [(-.f64 (*.f64 1 x) (*.f64 (+.f64 x 1) 1)) . -1]
          [(-.f64 (+.f64 x 1) x) . 1]
          [(-.f64 (+.f64 x 1) 1) . x]
          [(/.f64 (*.f64 x 3) x) . 3]
          [(-.f64 (*.f64 (sqrt.f64 (+.f64 x 1)) (sqrt.f64 (+.f64 x 1)))
              (*.f64 (sqrt.f64 x) (sqrt.f64 x))) . 1]
          [(+.f64 1/5 3/10) . 1/2]
          [(cos.f64 (PI.f64)) . -1]
          [(pow.f64 (E.f64) 1) . (E.f64)]
          ;; this test is problematic and runs out of nodes currently
          ;[(/ 1 (- (/ (+ 1 (sqrt 5)) 2) (/ (- 1 (sqrt 5)) 2))) . (/ 1 (sqrt 5))]
          ))

  (*timeline-disabled* true)
  (define outputs (apply test-simplify (hash-keys test-exprs)))
  (for ([(original target) test-exprs] [output outputs])
    (with-check-info (['original original])
       (check-equal? output target)))

  (check set-member? '((*.f64 x 6) (*.f64 6 x)) 
                     (first (test-simplify '(+.f64 (+.f64 (+.f64 (+.f64 (+.f64 x x) x) x) x) x))))

  (define no-crash-exprs
    '((exp.f64 (/.f64 (/.f64 (*.f64 (*.f64 c a) 4) 
                      (-.f64 (neg.f64 b) (sqrt.f64 (-.f64 (*.f64 b b) (*.f64 4 (*.f64 a c)))))) (*.f64 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr])
       (check-not-exn (λ () (test-simplify expr))))))
