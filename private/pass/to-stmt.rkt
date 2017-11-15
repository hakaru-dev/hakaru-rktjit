#lang racket

(require "ast.rkt"
         "utils.rkt")
(provide to-stmt)

(define debug-to-stmt (make-parameter #f))
(define dps (debug-printf debug-to-stmt))


(define (higher-order-f? ret-type)
  (match ret-type [`(,t ... -> ,t1) #t] [else #f]))

;; Converts body of functions to statements, if expressions
(define (get-type t index group-size)
  (dps "asking for type with index: ~a, ~a\n" t index)
  (if (zero? group-size) t
      (match t
        [`(array ,at ,_) at])))
(define (wrap-curry stmt carg ret-value-pairs)
  (for/fold ([s stmt])
            ([rvp ret-value-pairs])
    (set-expr-var-type! carg (typeof (cdr rvp)))
    (dps "cdr rvp: ~a\n" (pretty-format (second (typeof (cdr rvp)))))
    (dps "car rvp: ~a\n"  (pretty-format (map (curry map pe) (car rvp))))
    (for/fold ([st s])
              ([g (car rvp)]
               [t (second (typeof (cdr rvp)))]
               [si (in-range (length (car rvp)))])
      (dps "okey check here: g: ~a, t: ~a\n" (map pe g) t)
      (define sti (expr-app t (expr-intrf (symbol-append 'struct-index. si)) (list carg)))
      (define stis (expr-var t (gensym^ 'sti) '()))
      (define tvl (map (λ (gi ii)
                         (list (get-type t ii (length g))
                               gi
                               (if (equal? (length g) 1)
                                   stis
                                   (expr-app (get-type t ii (length g))
                                             (expr-intrf 'index)
                                             (list stis (expr-val 'nat ii))))))
                       g (build-list (length g) identity)))

      (dps "tvl: ~a\n" tvl)
      (stmt-expr
       (stmt-void)
       (expr-lets
        (cons t (map first tvl))
        (cons stis (map second tvl))
        (cons sti (map third tvl))
        st (expr-val 'nat 0))))))

    ;; (define tvl (for/list ([i (in-range (length (car rvp)))])
    ;;               (list '? g (expr-app t (expr-intrf 'struct-index) (list (expr-val 'nat i) carg)))))
    ;; (stmt-expr (stmt-void)
    ;;            (expr-lets (map first tvl) (map second tvl) (map third tvl) s (expr-val 'nat 0)))



(define (expand-function prog curry-args)
  (match prog
    [(expr-fun name args ret-type body)
     (dps "expanding function: ~a, ret-type: ~a\n" name ret-type)
     (define carg (expr-var '? (gensym^ 'carg) '()))
     (define dr (takef args (λ (a) (or (expr-val? a) (not (equal? (expr-var-sym a) '$))))))
     (define new-args (if (> (length curry-args) 0) (cons carg dr) dr))
     (if (higher-order-f? ret-type)
         (begin
           (let-values ([(stmt nfrp) (stmt-hl-body body args)])
             (match-define (cons next-func ret-value-pair) nfrp)
             (cons (expr-fun name new-args (typeof (cdr ret-value-pair)) (wrap-curry stmt carg curry-args))
                   (expand-function next-func (cons ret-value-pair curry-args)))))
         (list
          (expr-fun 'prog new-args ret-type
                    (wrap-curry (expr->stmt body (λ (e) (stmt-return e)))
                                carg curry-args))))]))

(define (figure-out-return args)
  (define cargs (remove-duplicates (remove (expr-var '$ '$ '()) args equal?) equal?))
  (define grouped-args (group-by typeof cargs))
  (dps "grouped-args: ~a\n" (map (curry map pe) grouped-args))
  (define (create-struct grouped-args)
    (define rands (append-map (λ (g) (if (equal? (length g) 1) g
                                         (list (expr-app `(array ,(typeof (car g)) (size . ,(length g)))
                                                         (expr-intrf 'const-size-array-literal)
                                                         g))))
                              grouped-args))
    (expr-app `(struct-type ,(map typeof rands)) (expr-intrf 'struct-literal) rands))
  (cons grouped-args (create-struct grouped-args)))

(define (stmt-hl-body body args)
  (match body
    [(expr-lets types vars vals s body)
     (define-values (nbody bargs) (stmt-hl-body body (append vars args)))
     (values (stmt-expr (stmt-void) (expr-lets types vars vals (stmt-block (list s nbody)) (expr-val 'nat 0)))
             bargs)]
    [(expr-fun fname fargs fret-type fbody)
     (define ret-value (figure-out-return args))
     (values (stmt-return (cdr ret-value))
             (cons body ret-value))]
    [else (printf "in stmt-hl-body args: ~a, body: \n~a\n" (map pe args) (pretty-format body))
          (error 'stop)]))


(define (to-stmt st)
  ;; (define (mod-stmt m)
  ;;   (define (fn-stmt f)
  ;;     (define (expr->ret-stmt e)
  ;;       (expr->stmt e (λ (e) (stmt-return e))))
  ;;     (match f
  ;;       [(expr-fun name args ret-type body)
  ;;        (if (expr? body) (expr-fun name args ret-type (expr->ret-stmt body)) f)]))
  ;;   (match m
  ;;     [(expr-mod main fns)
  ;;      (expr-mod (fn-stmt main)
  ;;                (map (λ (p) (cons (car p) (fn-stmt (cdr p)))) fns))]))
  (match st
    [(state prg info os)
     (define funs (expand-function prg '()))
     (run-next funs info st)]))
