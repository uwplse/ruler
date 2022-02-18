#lang racket

;; TODO: remove constants, re, im etc.

(define (ops-in-expr expr)
  (define ops (mutable-set))
  (let loop ([expr expr])
    (match expr
     [(list 'let (list (list vars vals) ...) body)
      (set-add! ops 'let)
      (for ([val vals]) (loop val))
      (loop body)]
     [(list 'let* (list (list vars vals) ...) body)
      (set-add! ops 'let*)
      (for ([val vals]) (loop val))
      (loop body)]
     [(list op args ...)
      (set-add! ops op)
      (for ([arg args]) (loop arg))]
     [_ (void)]))
  (map symbol->string (set->list ops)))

(define (syntax-e* stx)
  (match (syntax-e stx)
   [(list xs ...) (map syntax-e* xs)]
   [x x]))

(define (load-file file ops invert?)
  (define (valid? in-expr)
    (if invert?
        (andmap (negate (curry set-member? ops)) in-expr)
        (subset? in-expr ops)))
  (call-with-input-file file
    (λ (port)
      (port-count-lines! port)
      (for ([core (in-port (curry read-syntax file) port)])
        (define core* (syntax-e* core))
        (match-define (list 'FPCore vars props ... body) core*)
        (define in-expr (ops-in-expr body))
        (when (valid? in-expr)
          (print (get-expr core*) (current-output-port) 1)
          (newline))))))

(define (load-directory dir ops invert?)
  (for ([fname (in-directory dir)]
        #:when (file-exists? fname)
        #:when (equal? (filename-extension fname) #"fpcore"))
    (load-file fname ops invert?)))

(define (filter-cores path ops invert?)
  (define path* (if (string? path) (string->path path) path))
  (define out
    (cond
     [(directory-exists? path*)
      (load-directory path* ops invert?)]
     [else
      (load-file path* ops invert?)]))
  (void))

(define (get-expr prog)
(match prog
   [`(FPCore ,name (,vars ...) ,properties ... ,body) body]
   [`(FPCore (,vars ...) ,properties ... ,body) body]))

(module+ main
 (define invert? #f)
 (define ops '())
 (command-line
  #:program "filter.rkt"
  #:once-each
  [("-i" "--invert") "Invert the filter"
   (set! invert? #t)]
  ["--operators" str "List of operators (single string)"
   (set! ops (string-split str " "))]
  #:args paths
 (for ([path paths]) (filter-cores path ops invert?))
 (void)))
