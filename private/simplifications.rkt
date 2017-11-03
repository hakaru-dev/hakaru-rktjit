#lang racket
(require "ast.rkt"
         "utils.rkt")

(provide simplify-match
         simplify-lets
         mbind->let
         macro-functions
         remove-pairs
         remove-array-literals
         cleanup)

(define simplify-match
  (create-rpass
   (expr [(expr-match t tst brs)
          (if (eq? (typeof tst) 'bool) (toif t tst brs) (extract-pair t tst brs))])
   (reducer)
   (stmt)
   (pat)))

(define (toif t tst brs)
  (define-values (tb fb)
    (if (pat-true? (expr-branch-pat (car brs)))
        (values (car brs) (cadr brs))
        (values (cadr brs) (car brs))))
  (expr-if t tst (expr-branch-body tb) (expr-branch-body fb)))

(define (extract-pair t tst brs)
  (unless (and (eq? (length brs) 1)
               (eq? (car (typeof tst)) 'pair)
               (pat-pair? (expr-branch-pat (car brs)))
               (pat-var? (pat-pair-a (expr-branch-pat (car brs))))
               (pat-var? (pat-pair-b (expr-branch-pat (car brs)))))
    (error "matching over pair with multiple branches or complex pattern." brs))

  (define car-type (cadr (typeof tst)))
  (define cdr-type (caddr (typeof tst)))
  (define car-bind (expr-branch-body (car brs)))
  (define cdr-bind (expr-bind-body car-bind))
  (define body (expr-bind-body cdr-bind))
  (define car-var (expr-bind-var car-bind))
  (define cdr-var (expr-bind-var cdr-bind))
  (dprintf #t "setting type of ~a to ~a\n" (print-expr car-var) car-type)
  (set-expr-var-type! car-var car-type)
  (dprintf #t "setting type of ~a to ~a\n" (print-expr cdr-var) cdr-type)
  (set-expr-var-type! cdr-var cdr-type)

  (dprintf #t "match-pair: at: ~a, bt: ~a\n"
          car-type cdr-type)
  (expr-lets (list car-type cdr-type)
            (list car-var cdr-var)
            (list (expr-app car-type (expr-intrf 'car) (list tst))
                  (expr-app cdr-type (expr-intrf 'cdr) (list tst)))
            body))

(define (sl e env)
  (match e
    [(expr-let t v val body)
     (if (expr-var? val)
         (begin
           (dprintf #t "replacing: ~a with ~a\n" (print-expr v) (print-expr val))
           (sl body (hash-set env v val)))
         (expr-let t v (sl val env) (sl body env)))]
    [(expr-lets ts vars vals body)
     (define-values (nts nvars nvals ne)
       (for/fold ([nts '()] [nvars '()] [nvals '()] [e env])
                 ([t ts] [var vars] [val vals])
         (if (expr-var? val)
             (begin
               (dprintf #t "replacing: ~a with ~a\n"
                        (print-expr var) (print-expr val))
               (values nts nvars nvals (hash-set e var val)))
             (values (cons t nts) (cons var nvars) (cons (sl val e) nvals) e))))
     (if (empty? nvars)
         (sl body ne)
         (expr-lets nts nvars nvals (sl body ne)))]
    [v #:when (hash-has-key? env v)
       (dprintf #t "\treplaced: ~a with ~a\n"
               (print-expr v) (print-expr (hash-ref env v)))
       (hash-ref env v)]
    [(stmt-elets vars vals bstmt)
     (define-values (nvars nvals ne)
       (for/fold ([nvars '()] [nvals '()] [e env])
                 ([var vars] [val vals])
         (dprintf (expr-var? val)
                  "replacing: ~a with ~a\n" (print-expr var) (print-expr val))
         (if (expr-var? val)
             (values nvars nvals (hash-set e var val))
             (values (cons var nvars) (cons (sl val e) nvals) e))))
     (if (empty? nvars)
         (sl bstmt ne)
         (stmt-elets nvars nvals (sl bstmt ne)))]
    [(? expr?)
     (define fsl (curryr sl env))
     (map-expr fsl identity fsl identity e)]
    [(? stmt?)
     (define fsl (curryr sl env))
     (map-stmt fsl identity fsl identity e)]))

(define (simplify-lets e)
  (sl e (make-immutable-hash)))

(define (remove-pairs e)
  (define vars (mutable-set))
  (define pass
    (create-rpass
     (expr [(expr-app t (expr-intrf s) rands)
            (match s
              ['car
               (define var-sym (expr-var-sym (car rands)))
               (define nv (expr-var t (symbol-append var-sym 'a) '_))
               (if (string-prefix? (symbol->string var-sym) "bk")
                   nv
                   (expr-app t (expr-intrf s) rands))]
              ['cdr
               (define var-sym (expr-var-sym (car rands)))
               (define nv (expr-var t (symbol-append var-sym 'b) '_))
               (if (string-prefix? (symbol->string var-sym) "bk")
                   nv
                   (expr-app t (expr-intrf s) rands))]
              [else  (expr-app t (expr-intrf s) rands)])])
     (reducer)
     (stmt)
     (pat)))
  (pass e))

(define mbind->let
  (create-rpass
   (expr [(expr-app t (expr-intrf 'mbind)
                    (list val (expr-bind (expr-var vt var org-sym) body)))
          (dprintf #t "mbind ~a: ~a\n" var (typeof val))
          (expr-let t (expr-var (typeof val) var org-sym) val body)])
   (reducer)
   (stmt)
   (pat)))

(define (filter-index pred? lst)
  (for/list ([i (in-range (length lst))]
             [v lst]
             #:when (pred? i))
    v))
(define (make-switch t lst v)
  (for/fold ([b (expr-val 'nat 0)])
            ([e lst]
             [i (in-range (length lst))])
    (expr-if t (expr-app 'nat (expr-intrf '==) (list v (expr-val 'nat i))) e b)))

(define macro-functions
  (create-rpass
   (expr [(expr-app t (expr-intrf 'empty) '())
          (expr-app t (expr-intrf 'empty) (list (expr-val 'nat 0)))]
         [(expr-app t (expr-intrf 'dirac) (list val))
          val]
         [(expr-app t (expr-intrf 'pose) (list arg1 arg2))
          arg2]
         [(expr-app t (expr-intrf 'superpose) args)
          (define get-odds (curry filter-index odd?))
          (define get-evens (curry filter-index even?))
          (define var (expr-var 'nat (gensym^ 's) '_))
          (expr-let t
                    var
                    (expr-app 'nat (expr-intrf 'superpose-categorical)
                              (get-evens args))
                    (make-switch t (get-odds args) var))])
   (reducer)
   (stmt)
   (pat)))

(define (remove-array-literals e)
  (define pass
    (create-rpass
     (expr [(expr-app ta (expr-intrf 'index)
                      (list (expr-app t (expr-intrf 'array-literal) aargs) iarg))
            (define ab
              (expr-app ta (expr-intrf 'index)
                        (list (expr-app t (expr-intrf 'array-literal) aargs) iarg)))
            (if (< (length aargs) 5) (check-if-remove aargs iarg ab) ab)])
     (reducer)
     (stmt)
     (pat)))
  (pass e))

(define (check-if-remove arr-vals indexer orig-b)
  (if (complex? indexer)
      orig-b
      (match indexer
        [(expr-if t chk (expr-val 'nat vthn) (expr-val 'nat vels))
         (expr-if t chk (list-ref arr-vals vthn) (list-ref arr-vals vels))]
        [else orig-b])))

(define (cleanup e)
  (define (merge-stmt-block s)
    (match s
      [(stmt-block stmts)
       (define ns
         (append-map
          (λ (s)
            (match s
              [(stmt-block ss) ss]
              [(stmt-void) '()]
              [else (list s)]))
          stmts))
       (if (eq? (length ns) 1)
           (car ns)
           (stmt-block ns))]
      [else s]))
  (define pass
    (create-rpass
     (expr
      [(expr-lets _ '() _ b) b]
      [(expr-block t (stmt-void) e) e]
      [(expr-block t (stmt-block '()) e) e])
     (reducer)
     (stmt
      ;; [(stmt-lets '() s) s]
      ;; [(stmt-lets vars s)
      ;;  #:when (stmt-block? s)
      ;;  (define vs (list->mutable-set vars))
      ;;  (define ns
      ;;    (append-map
      ;;     (λ (s)
      ;;       (match s
      ;;         [(stmt-lets vars s) (map (curry set-add! vs) vars) (list s)]
      ;;         [(stmt-block ss) ss]
      ;;         [else (list s)]))
      ;;     (stmt-block-stmts s)))
      ;;  (stmt-lets (set->list vs) (merge-stmt-block (stmt-block ns)))]
      [(stmt-block '()) (stmt-void)]
      [(stmt-block stmts) (merge-stmt-block (stmt-block stmts))])
     (pat)))
  (pass e))
