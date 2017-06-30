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
    (define red-stmt (stmt-for ind start end (do-reducer ind (list ind) result t reducer)))
    (pretty-display (ps red-stmt))
    (newline)
    (expr-block '? red-stmt result))

(define (do-reducer i binds result t reducer)
  (printf "do-reducer\t binds: ~a\n" (map pe binds))
  (match* (reducer t)
    [((reducer-split (expr-bind bvar bbody) a b) `(pair ,ta ,tb))
     ;; (printf "\t reducer-split typea: ~a, typeb: ~a\n" ta tb)
     (stmt-if (expr-let '? bvar i bbody)
              (do-reducer i binds (expr-var '? (symbol-append (expr-var-sym result) 'a) '_) ta a)
              (do-reducer i binds (expr-var '? (symbol-append (expr-var-sym result) 'b) '_) tb b))]
    [((reducer-fanout a b) `(pair ,ta ,tb))
     ;; (printf "\t reducer-fanout typea: ~a, typeb: ~a\n" ta tb)
     (stmt-block
      (list
       (do-reducer i binds (expr-var '? (symbol-append (expr-var-sym result) 'a) '_) ta a)
       (do-reducer i binds (expr-var '? (symbol-append (expr-var-sym result) 'b) '_) tb b)))]
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
     (do-reducer i
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
