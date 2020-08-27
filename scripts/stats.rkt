#lang plai

;; parse the
;; collect all counting functions

;; auto-parse s-expressions
;; recursively trawl

;; define the language (Herbie)
;;(+ (neg a) (neg b))       (neg (+ a b))

;; parse s-expr into language

(require "inputs/herbie-rules.rkt")

;; run visitor over AST

(define e 2.718281828)

(define-type OperatorType
  [Plus]
  [Minus]
  [Divide]
  [Multiply]
  [Neg]
  [Pow]
  [Equal]
  [Sqrt]
  [Cbrt]
  [Exp]
  [Log]
  [Fabs]
  [Remainder]
  ;; Logic
  [Or]
  [And]
  [Not]
  ;; Trig
  [Asin]
  [Sinh]
  [Asinh]
  [Sin]
  [Atan]
  [Tanh]
  [Atanh]
  [Tan]
  [Acos]
  [Cosh]
  [Acosh]
  [Cos]
  [Atan2]
  ;; Stuff I don't understand
  [Expm1]
  [Log1p]
  [Hypot]
  [Fma]
  [Erf]
  [Erfc]
  ;; Inequalities
  [Lt]
  [Le]
  [Gt]
  [Ge])

(define-type Type
  [Variable (str symbol?)]
  [Operator (char OperatorType?)]
  [Constant (val number?)]
  [If])


(define-type ast
  [node (type Type?) (children list?)])



(define (parse sexp)
  (match sexp
    ['PI (node (Constant pi) empty)]
    ['e (node (Constant e) empty)]
    [(? symbol?) (node (Variable sexp) empty)]
    [(? number?) (node (Constant sexp) empty)]
    ;;[(? boolean?) (s-bool sexp)]
    [(list '+ lexp rexp) (node (Operator (Plus)) (list (parse lexp) (parse rexp)))]
    [(list '- lexp rexp) (node (Operator (Minus)) (list (parse lexp) (parse rexp)))]
    [(list '* lexp rexp) (node (Operator (Multiply)) (list (parse lexp) (parse rexp)))]
    [(list '/ lexp rexp) (node (Operator (Divide)) (list (parse lexp) (parse rexp)))]
    [(list '> lexp rexp) (node (Operator (Gt)) (list (parse lexp) (parse rexp)))]
    [(list '>= lexp rexp) (node (Operator (Ge)) (list (parse lexp) (parse rexp)))]
    [(list '< lexp rexp) (node (Operator (Lt)) (list (parse lexp) (parse rexp)))]
    [(list '<= lexp rexp) (node (Operator (Le)) (list (parse lexp) (parse rexp)))]
    [(list '= lexp rexp) (node (Operator (Equal)) (list (parse lexp) (parse rexp)))]
    [(list 'neg exp) (node (Operator (Neg)) (list (parse exp)))]
    [(list 'fabs exp) (node (Operator (Fabs)) (list (parse exp)))]
    [(list 'exp exp) (node (Operator (Exp)) (list (parse exp)))]
    [(list 'cbrt exp) (node (Operator (Cbrt)) (list (parse exp)))]
    [(list 'sqrt exp) (node (Operator (Sqrt)) (list (parse exp)))]
    [(list 'pow lexp rexp) (node (Operator (Pow)) (list (parse lexp) (parse rexp)))]
    [(list 'remainder lexp rexp) (node (Operator (Remainder)) (list (parse lexp) (parse rexp)))]
    [(list 'or lexp rexp) (node (Operator (Or)) (list (parse lexp) (parse rexp)))]
    [(list 'and lexp rexp) (node (Operator (And)) (list (parse lexp) (parse rexp)))]
    [(list 'not exp) (node (Operator (Not)) (list (parse exp)))]
    [(list 'if test lexp rexp) (node (If) (list (parse test) (parse lexp) (parse rexp)))]
    [(list 'log1p exp) (node (Operator (Log1p)) (list (parse exp)))]
    [(list 'expm1 exp) (node (Operator (Expm1)) (list (parse exp)))]
    [(list 'erf exp) (node (Operator (Erf)) (list (parse exp)))]
    [(list 'erfc exp) (node (Operator (Erfc)) (list (parse exp)))]
    [(list 'fma exp1 exp2 exp3) (node (Operator (Fma)) (list (parse exp1) (parse exp2) (parse exp3)))]
    [(list 'log exp) (node (Operator (Log)) (list (parse exp)))]
    [(list 'sin exp) (node (Operator (Sin)) (list (parse exp)))]
    [(list 'cos exp) (node (Operator (Cos)) (list (parse exp)))]
    [(list 'tan exp) (node (Operator (Tan)) (list (parse exp)))]
    [(list 'asin exp) (node (Operator (Asin)) (list (parse exp)))]
    [(list 'acos exp) (node (Operator (Acos)) (list (parse exp)))]
    [(list 'atan exp) (node (Operator (Atan)) (list (parse exp)))]
    [(list 'sinh exp) (node (Operator (Sinh)) (list (parse exp)))]
    [(list 'cosh exp) (node (Operator (Cosh)) (list (parse exp)))]
    [(list 'tanh exp) (node (Operator (Tanh)) (list (parse exp)))]
    [(list 'asinh exp) (node (Operator (Asinh)) (list (parse exp)))]
    [(list 'acosh exp) (node (Operator (Acosh)) (list (parse exp)))]
    [(list 'atanh exp) (node (Operator (Atanh)) (list (parse exp)))]
    [(list 'atan2 lexp rexp) (node (Operator (Atan2)) (list (parse lexp) (parse rexp)))]
    [(list 'hypot lexp rexp) (node (Operator (Hypot)) (list (parse lexp) (parse rexp)))]))


;; A rule has a name, LHS, and RHS 
(define-type rule-ast
  [rule (name symbol?) (left node?) (right node?)])

(define (parse-ast sexp)
  (match sexp
    [(? symbol?) sexp]
    [(list name left right) (rule (parse-ast name) (parse left) (parse right))]))

(parse-ast '[difference-of-squares (- (* a a) (* b b))   (* (+ a b) (- a b))])


(define-type place-acc
  [depth (val number?)])

(define-type info-acc
  ;; todo technically might need scoping 
  [info (count number?) (unique-vars list?) (num-vars number?) (num-ops number?) (num-consts number?)])

(define print-json-info
  (λ (rule-name info-tuple)
    (local [(define depth (depth-val (first info-tuple)))
            (define info (second info-tuple))]
      (foldr (λ (x acc) (string-append x acc))
             ""
             (list "\"" (symbol->string rule-name) "\"" ": {"
                   "\"num_nodes\": " (number->string (info-count info)) ","
                   "\"depth\": " (number->string depth) ","
                   "\"unique_vars\": [" (string-join
                                         (map (λ(x) (string-append "\"" (symbol->string x) "\""))
                                              (info-unique-vars info))
                                         ",") "],"
                                              "\"num_consts\": " (number->string (info-num-consts info)) ","
                                              "\"num_vars\": " (number->string (info-num-vars info)) ","
                                              "\"num_ops\": " (number->string (info-num-ops info)) "}"
                                              )))))

    
(define run
  (λ (node)
    (local [(define-type wle-entry [worklist (node node?) (info place-acc?)])
            ;; should return a new analysis
            (define run-analysis (λ (node acc)
                                   (info (+ 1 (info-count acc))
                                         (type-case Type (node-type node)
                                           [Variable (name)
                                                     (if (empty? (filter (λ (exists) (symbol=? exists name))
                                                                         (info-unique-vars acc)))
                                                         (cons name (info-unique-vars acc))
                                                         (info-unique-vars acc))]
                                           [else (info-unique-vars acc)])
                                         (type-case Type (node-type node)
                                           [Variable (name) (+ (info-num-vars acc) 1)]
                                           [else (info-num-ops acc)])
                                         (type-case Type (node-type node)
                                           [Operator (type) (+ (info-num-ops acc) 1)]
                                           [If () (+ (info-num-ops acc) 1)]
                                           [else (info-num-ops acc)])
                                         (type-case Type (node-type node)
                                           [Constant (type) (+ (info-num-consts acc) 1)]
                                           [else (info-num-consts acc)]))))
            (define run-rec
              (λ (node node-info todo acc)
                ;; todo need some end condition stuff
                (local [(define new-todo (append (map ;; currently only storing depth
                                                  (λ (c) (worklist c
                                                                   (depth (+ (depth-val node-info) 1))))
                                                  (node-children node))
                                                 (if (empty? todo)
                                                     empty
                                                     (rest todo))))]
                  (if (empty? new-todo)
                      (list node-info (run-analysis node acc))
                      (run-rec (worklist-node (first new-todo)) (worklist-info (first new-todo))
                               new-todo
                               (run-analysis node acc))))))]
      (run-rec node (depth 0) empty (info 0 empty 0 0 0)))))


(define run-rule
  (λ (ast)
    (list (run (rule-left ast))
          (run (rule-right ast)))))

(define pretty-print-rule
  (λ (ast)
    (string-append
     "{ \"rulename\": " "\"" (symbol->string (rule-name ast)) "\","
     (print-json-info 'left (run (rule-left ast)))
     ", "
     (print-json-info 'right (run (rule-right ast)))
     "}")))

;; parse and run
(define parse-and-run-all
  (λ (all-rules)
    (map run-rule
         (map parse-ast all-rules))))

(define parse-and-print-all-by-category
  (λ (all-rules)
    (string-append
     "["
     (string-join
       (map (λ(ruleset)
              (string-append
               "{\"ruleset\": \""
               (car ruleset)
               "\", \"rules\": "
              (parse-and-print-all (cdr ruleset))
              "}")) all-rules)
       ",")
     "]")))

(define parse-and-print-all
  (λ (all-rules)
    (string-append
     "["
     (string-join (map pretty-print-rule
                       (map parse-ast all-rules))
                  ",\n")
     "]")))

(define basic-math
  (node (Operator (Plus))
        (list (node (Constant 5) empty)
              (node (Constant 6) empty))))
(define var-eq
  (node (Operator (Plus))
        (list (node (Variable 'foo) empty)
              (node (Variable 'bar) empty))))

(define multi-var-eq
  (node (Operator (Plus))
        (list (node (Variable 'foo) empty)
              (node (Operator (Minus)) (list (node (Variable 'foo) empty)
                                             (node (Constant 3) empty))))))

;; no longer function
;;(test (run basic-math) (info 3 '() 1))
;;(test (run var-eq) (info 3 '(bar 'foo) 1))
;;(test (run multi-var-eq) (info 5 '('foo) 2))

;;(display (parse-and-print-all all-rules))
(display (parse-and-print-all-by-category all-rules-by-category))

