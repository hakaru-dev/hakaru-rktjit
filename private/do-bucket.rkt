#lang racket

(require "utils.rkt")
(require "ast.rkt")

(provide bucket->for)

(define bucket->for
  (expr/pass
   [(expr-lets typs vars vals body)
    (define nvals (for/list ([var vars]
                            [val vals])
                   (match val
                     [(expr-bucket t start end reducer)
                      (do-bucket var t start end reducer)]
                     [else val])))
    (expr-lets typs vars nvals body)]
   [(expr-let t var (expr-bucket t start end reducer) body)
    (expr-let t var (do-bucket var t start end reducer) body)]))

(define (do-bucket result t start end reducer)
    (define ind (expr-var 'nat (gensym^ (symbol-append (expr-var-sym result) 'i)) '_))
    (printf "doing bucket:\n \ttype: ~a, size: ~a\n" t (print-expr end))
    (printf "\treducer: " ) (pretty-display (pr reducer))
    (define-values (init-vars init-vals) (get-init '() result t reducer))
    (define red-stmt (expr-lets '() init-vars init-vals
                                ;; (expr-block
                                ;;  '?
                                ;;  (stmt-for ind start end (get-accum ind (list ind) result t reducer))
                                ;;  result)
                                (expr-val '? 0)
                                ))
    (pretty-display (pe red-stmt))
    (newline)
    red-stmt)

(define (expr-sym-append var sym)
  (match-define (expr-var t s o) var)
  (expr-var t (symbol-append s sym) o))

(define (get-print-type t)
  (match t
    [`(array ,tar) #:when (symbol? tar)
     (symbol-append tar '-p)]
    [`(array ,tar) (symbol-append (get-print-type tar) 'p)]
    [symbol? t]))
(define (get-init binds result t reducer)
  (printf "get-init\t result: ~a, type: ~a\n" (pe result) t)
  (match* (t reducer)
    [('nat (reducer-add _)) (values (list result) (list (expr-val 'nat 0)))]
    [(`(pair ,ta ,tb) (reducer-split _ ra rb))
     (define-values (vra vla) (get-init binds (expr-sym-append result 'a) ta ra))
     (define-values (vrb vlb) (get-init binds (expr-sym-append result 'b) tb rb))
     (values (append vra vrb) (append vla vlb))]
    [(`(pair ,ta ,tb) (reducer-fanout ra rb))
     (define-values (vra vla) (get-init binds (expr-sym-append result 'a) ta ra))
     (define-values (vrb vlb) (get-init binds (expr-sym-append result 'b) tb rb))
     (values (append vra vrb) (append vla vlb))]
    [(`(array ,tar) (reducer-index n _ ra))
     (define ptar (get-print-type tar))
     (define arr-size (assign-binds binds n))
     (define arrn (expr-var '? (gensym^ 'arri) '_))
     (define fori (expr-var 'nat (gensym^ 'fi) '_))
     (define arr-init (expr-app tar (expr-intrf (symbol-append 'empty-array- ptar))
                                (list arr-size)))
     (define-values (vra vla) (get-init (cons fori binds) result tar ra))
     (define arrv
       (expr-let
        '? arrn arr-init
        (expr-block
         '?
         (stmt-for
          fori (expr-val 'nat 0) arr-size
          (stmt-assign
           (expr-app
            '?
            (expr-intrf (symbol-append
                            'index-array- ptar))
            (list fori))
           (car vla)))
         arrn)))
     (values (cons result vra)
             (cons arrv
                   vla))]
    [('unit (reducer-nop)) (values '() '())]
;    [(e r) (values '() '())]
    ))

(define (get-accum i binds result t reducer)
  (printf "get-accum\t binds: ~a\n" (map pe binds))
  (match* (reducer t)
    [((reducer-split (expr-bind bvar bbody) a b) `(pair ,ta ,tb))
     ;; (printf "\t reducer-split typea: ~a, typeb: ~a\n" ta tb)
     (stmt-if (expr-let '? bvar i bbody)
              (get-accum i binds (expr-var '? (symbol-append (expr-var-sym result) 'a) '_) ta a)
              (get-accum i binds (expr-var '? (symbol-append (expr-var-sym result) 'b) '_) tb b))]
    [((reducer-fanout a b) `(pair ,ta ,tb))
     ;; (printf "\t reducer-fanout typea: ~a, typeb: ~a\n" ta tb)
     (stmt-block
      (list
       (get-accum i binds (expr-var '? (symbol-append (expr-var-sym result) 'a) '_) ta a)
       (get-accum i binds (expr-var '? (symbol-append (expr-var-sym result) 'b) '_) tb b)))]
    [((reducer-add e) te)
     (printf "\t reducer-add type: ~a a: ~a\n" te (pe e))
     (printf "\t reducer-add binds: ~a" (map pe binds))
     (stmt-assign result (expr-app '? (expr-intrf '+)  (list result (assign-binds binds e))))]
    [((reducer-nop) 'unit)
     (stmt-void)]
    [((reducer-index n ind a) ti)
     (printf "\t reducer-index type: ~a\n" ti)
     (define ind-result (expr-app '? (expr-intrf 'index)
                           (list result (assign-binds binds ind))))
     (get-accum i
                 (cons ind-result binds)
                 ind-result t a)]
    [(r t) (printf "unknown reducer type: ~a\n" t)]))

(define (assign-binds vars bind)
  (printf "assign-binds vars: ~a, bind: ~a\n" (map pe vars) (pe bind))
  (if (and (empty? vars) (not (expr-bind? bind)))
      bind
      (match bind
           [(expr-bind v b)
            (expr-let '? v (car vars)
                      (assign-binds (cdr vars) b))])))
