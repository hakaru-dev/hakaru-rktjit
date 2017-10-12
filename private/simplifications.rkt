#lang racket
(require "ast.rkt"
         "utils.rkt")

(provide simplify-match
         simplify-lets
         mbind->let
         macro-functions
         remove-pairs)

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
  (define car-var (expr-bind-var car-bind))
  (define cdr-var (expr-bind-var cdr-bind))
  (printf "match-pair: \n\tbr: ~a\n\tat: ~a, bt: ~a\n"
          (pe (car brs))
          car-type cdr-type)
  (expr-let t car-var (expr-app car-type (expr-intrf 'car) (list tst))
            (expr-let t cdr-var (expr-app cdr-type (expr-intrf 'cdr) (list tst))
                      (expr-bind-body cdr-bind))))

(define (sl e env)
  (match e
    [(expr-let t v val body)
     (if (expr-var? val)
         (begin
           (printf "replacing: ~a with ~a\n" (print-expr v) (print-expr val))
           (sl body (hash-set env v val)))
         (expr-let t v (sl val env) (sl body env)))]
    [v #:when (hash-has-key? env v)
       (printf "\treplaced: ~a with ~a\n" (print-expr v) (print-expr (hash-ref env v)))
       (hash-ref env v)]
    [(stmt-elets vars vals bstmt)
     (define-values (nvars nvals ne)
       (for/fold ([nvars '()] [nvals '()] [e env])
                 ([var vars] [val vals])
         (if (expr-var? val)
             (begin
               (printf "replacing: ~a with ~a\n" (print-expr var) (print-expr val))
               (values nvars nvals (hash-set e var val)))
             (values (cons var nvars) (cons (sl val e) nvals) e))))
            (stmt-elets nvars nvals (sl bstmt ne))]
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
   (expr [(expr-app t (expr-intrf 'mbind) (list val (expr-bind (expr-var vt var org-sym) body)))
          (printf "mbind ~a: ~a\n" var (typeof val))
          (expr-let t (expr-var (typeof val) var org-sym) val body)])
   (reducer)
   (stmt)
   (pat)))

(define macro-functions
  (create-rpass
   (expr [(expr-app t (expr-intrf 'empty) '())
          (expr-app t (expr-intrf 'empty) (list (expr-val 'nat 0)))]
         [(expr-app t (expr-intrf 'dirac) (list val))
          val]
         [(expr-app t (expr-intrf 'pose) (list arg1 arg2))
          arg2])
   (reducer)
   (stmt)
   (pat)))


