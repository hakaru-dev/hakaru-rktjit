#lang racket

(provide (all-defined-out))

(require racket/generic)
(define map-symbol (λ (e r s p e^) e^))
(define mapr-symbol(λ (e r s p e^) e^))

;;GRAMMAR-INFO
#|
(define-grammar hakaru
  (expr
   (mod (main fns) [expr (* expr)])
   (fun (args ret-type body) [(* expr) symbol expr])
   (let (type var val body) [symbol expr expr expr])
   (lets (types vars vals body) [symbol (* expr) (* expr) expr])
   (var (type sym orig) [symbol symbol symbol])
   (arr (type index size body) [symbol expr expr expr])
   (sum (type index start end body) [symbol expr expr expr expr])
   (prd (type index start end body) [symbol expr expr expr expr])
   (bucket (type start end reducer) [symbol expr expr reducer])
   (branch (pat body) (pat expr))
   (match (type tst branches) [symbol expr (* expr)])
   (bind (var body) (expr expr))
   (if (type tst thn els) (symbol expr expr expr))
   (app (type rator rands) (symbol expr (* expr)))
   (val (type v) (symbol symbol))
   (intr (sym) (symbol))
   (intrf (sym) (symbol))
   (block (type stmt body) (symbol stmt expr)))
  (reducer
   (split (e a b) [expr reducer reducer])
   (fanout (a b) [reducer reducer])
   (add (e) [expr])
   (nop () ())
   (index (n i a) (expr expr reducer)))
  (stmt
   (if (tst thn els) (expr stmt stmt))
   (for (i start end body) (expr expr expr stmt))
   (block (stmts) ((* stmt)))
   (assign (var val) (expr expr))
   (void () ()))
  (pat
   (true () ())
   (false () ())
   (pair (a b) [pat pat])
   (var () ())
   (ident () ())))
|#

;AG
(begin
   (begin
     (struct expr ())
     (define-generics
      exprg
      (map-expr f-expr f-reducer f-stmt f-pat exprg)
      (mapr-expr f-expr f-reducer f-stmt f-pat exprg))
     (struct
      expr-mod
      expr
      (main fns)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-mod main fns) (f-expr e^))
         (expr-mod
          (super-map-expr f-expr f-reducer f-stmt f-pat main)
          (map (curry super-map-expr f-expr f-reducer f-stmt f-pat) fns)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-mod main fns) e^)
         (f-expr
          (expr-mod
           (super-mapr-expr f-expr f-reducer f-stmt f-pat main)
           (map (curry super-mapr-expr f-expr f-reducer f-stmt f-pat) fns))))))
     (struct
      expr-fun
      expr
      (args ret-type body)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-fun args ret-type body) (f-expr e^))
         (expr-fun
          (map (curry super-map-expr f-expr f-reducer f-stmt f-pat) args)
          (map-symbol f-expr f-reducer f-stmt f-pat ret-type)
          (super-map-expr f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-fun args ret-type body) e^)
         (f-expr
          (expr-fun
           (map (curry super-mapr-expr f-expr f-reducer f-stmt f-pat) args)
           (mapr-symbol f-expr f-reducer f-stmt f-pat ret-type)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat body))))))
     (struct
      expr-let
      expr
      (type var val body)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-let type var val body) (f-expr e^))
         (expr-let
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (super-map-expr f-expr f-reducer f-stmt f-pat var)
          (super-map-expr f-expr f-reducer f-stmt f-pat val)
          (super-map-expr f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-let type var val body) e^)
         (f-expr
          (expr-let
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat var)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat val)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat body))))))
     (struct
      expr-lets
      expr
      (types vars vals body)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-lets types vars vals body) (f-expr e^))
         (expr-lets
          (map-symbol f-expr f-reducer f-stmt f-pat types)
          (map (curry super-map-expr f-expr f-reducer f-stmt f-pat) vars)
          (map (curry super-map-expr f-expr f-reducer f-stmt f-pat) vals)
          (super-map-expr f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-lets types vars vals body) e^)
         (f-expr
          (expr-lets
           (mapr-symbol f-expr f-reducer f-stmt f-pat types)
           (map (curry super-mapr-expr f-expr f-reducer f-stmt f-pat) vars)
           (map (curry super-mapr-expr f-expr f-reducer f-stmt f-pat) vals)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat body))))))
     (struct
      expr-var
      expr
      (type sym orig)
      #:methods
      gen:equal+hash
      ((define (equal-proc v1 v2 _)
         (equal? (expr-var-sym v1) (expr-var-sym v2)))
       (define (hash-proc v _) (equal-hash-code (expr-var-sym v)))
       (define (hash2-proc v _) (equal-secondary-hash-code (expr-var-sym v))))
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-var type sym orig) (f-expr e^))
         (expr-var
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (map-symbol f-expr f-reducer f-stmt f-pat sym)
          (map-symbol f-expr f-reducer f-stmt f-pat orig)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-var type sym orig) e^)
         (f-expr
          (expr-var
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (mapr-symbol f-expr f-reducer f-stmt f-pat sym)
           (mapr-symbol f-expr f-reducer f-stmt f-pat orig))))))
     (struct
      expr-arr
      expr
      (type index size body)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-arr type index size body) (f-expr e^))
         (expr-arr
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (super-map-expr f-expr f-reducer f-stmt f-pat index)
          (super-map-expr f-expr f-reducer f-stmt f-pat size)
          (super-map-expr f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-arr type index size body) e^)
         (f-expr
          (expr-arr
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat index)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat size)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat body))))))
     (struct
      expr-sum
      expr
      (type index start end body)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-sum type index start end body) (f-expr e^))
         (expr-sum
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (super-map-expr f-expr f-reducer f-stmt f-pat index)
          (super-map-expr f-expr f-reducer f-stmt f-pat start)
          (super-map-expr f-expr f-reducer f-stmt f-pat end)
          (super-map-expr f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-sum type index start end body) e^)
         (f-expr
          (expr-sum
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat index)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat start)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat end)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat body))))))
     (struct
      expr-prd
      expr
      (type index start end body)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-prd type index start end body) (f-expr e^))
         (expr-prd
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (super-map-expr f-expr f-reducer f-stmt f-pat index)
          (super-map-expr f-expr f-reducer f-stmt f-pat start)
          (super-map-expr f-expr f-reducer f-stmt f-pat end)
          (super-map-expr f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-prd type index start end body) e^)
         (f-expr
          (expr-prd
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat index)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat start)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat end)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat body))))))
     (struct
      expr-bucket
      expr
      (type start end reducer)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-bucket type start end reducer) (f-expr e^))
         (expr-bucket
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (super-map-expr f-expr f-reducer f-stmt f-pat start)
          (super-map-expr f-expr f-reducer f-stmt f-pat end)
          (map-reducer f-expr f-reducer f-stmt f-pat reducer)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-bucket type start end reducer) e^)
         (f-expr
          (expr-bucket
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat start)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat end)
           (mapr-reducer f-expr f-reducer f-stmt f-pat reducer))))))
     (struct
      expr-branch
      expr
      (pat body)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-branch pat body) (f-expr e^))
         (expr-branch
          (map-pat f-expr f-reducer f-stmt f-pat pat)
          (super-map-expr f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-branch pat body) e^)
         (f-expr
          (expr-branch
           (mapr-pat f-expr f-reducer f-stmt f-pat pat)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat body))))))
     (struct
      expr-match
      expr
      (type tst branches)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-match type tst branches) (f-expr e^))
         (expr-match
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (super-map-expr f-expr f-reducer f-stmt f-pat tst)
          (map (curry super-map-expr f-expr f-reducer f-stmt f-pat) branches)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-match type tst branches) e^)
         (f-expr
          (expr-match
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat tst)
           (map
            (curry super-mapr-expr f-expr f-reducer f-stmt f-pat)
            branches))))))
     (struct
      expr-bind
      expr
      (var body)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-bind var body) (f-expr e^))
         (expr-bind
          (super-map-expr f-expr f-reducer f-stmt f-pat var)
          (super-map-expr f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-bind var body) e^)
         (f-expr
          (expr-bind
           (super-mapr-expr f-expr f-reducer f-stmt f-pat var)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat body))))))
     (struct
      expr-if
      expr
      (type tst thn els)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-if type tst thn els) (f-expr e^))
         (expr-if
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (super-map-expr f-expr f-reducer f-stmt f-pat tst)
          (super-map-expr f-expr f-reducer f-stmt f-pat thn)
          (super-map-expr f-expr f-reducer f-stmt f-pat els)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-if type tst thn els) e^)
         (f-expr
          (expr-if
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat tst)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat thn)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat els))))))
     (struct
      expr-app
      expr
      (type rator rands)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-app type rator rands) (f-expr e^))
         (expr-app
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (super-map-expr f-expr f-reducer f-stmt f-pat rator)
          (map (curry super-map-expr f-expr f-reducer f-stmt f-pat) rands)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-app type rator rands) e^)
         (f-expr
          (expr-app
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat rator)
           (map
            (curry super-mapr-expr f-expr f-reducer f-stmt f-pat)
            rands))))))
     (struct
      expr-val
      expr
      (type v)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-val type v) (f-expr e^))
         (expr-val
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (map-symbol f-expr f-reducer f-stmt f-pat v)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-val type v) e^)
         (f-expr
          (expr-val
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (mapr-symbol f-expr f-reducer f-stmt f-pat v))))))
     (struct
      expr-intr
      expr
      (sym)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-intr sym) (f-expr e^))
         (expr-intr (map-symbol f-expr f-reducer f-stmt f-pat sym)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-intr sym) e^)
         (f-expr
          (expr-intr (mapr-symbol f-expr f-reducer f-stmt f-pat sym))))))
     (struct
      expr-intrf
      expr
      (sym)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-intrf sym) (f-expr e^))
         (expr-intrf (map-symbol f-expr f-reducer f-stmt f-pat sym)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-intrf sym) e^)
         (f-expr
          (expr-intrf (mapr-symbol f-expr f-reducer f-stmt f-pat sym))))))
     (struct
      expr-block
      expr
      (type stmt body)
      #:methods
      gen:exprg
      ((define/generic super-map-expr map-expr)
       (define (map-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-block type stmt body) (f-expr e^))
         (expr-block
          (map-symbol f-expr f-reducer f-stmt f-pat type)
          (map-stmt f-expr f-reducer f-stmt f-pat stmt)
          (super-map-expr f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-expr mapr-expr)
       (define (mapr-expr f-expr f-reducer f-stmt f-pat e^)
         (match-define (expr-block type stmt body) e^)
         (f-expr
          (expr-block
           (mapr-symbol f-expr f-reducer f-stmt f-pat type)
           (mapr-stmt f-expr f-reducer f-stmt f-pat stmt)
           (super-mapr-expr f-expr f-reducer f-stmt f-pat body)))))))
   (begin
     (struct reducer ())
     (define-generics
      reducerg
      (map-reducer f-expr f-reducer f-stmt f-pat reducerg)
      (mapr-reducer f-expr f-reducer f-stmt f-pat reducerg))
     (struct
      reducer-split
      reducer
      (e a b)
      #:methods
      gen:reducerg
      ((define/generic super-map-reducer map-reducer)
       (define (map-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-split e a b) (f-reducer e^))
         (reducer-split
          (map-expr f-expr f-reducer f-stmt f-pat e)
          (super-map-reducer f-expr f-reducer f-stmt f-pat a)
          (super-map-reducer f-expr f-reducer f-stmt f-pat b)))
       (define/generic super-mapr-reducer mapr-reducer)
       (define (mapr-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-split e a b) e^)
         (f-reducer
          (reducer-split
           (mapr-expr f-expr f-reducer f-stmt f-pat e)
           (super-mapr-reducer f-expr f-reducer f-stmt f-pat a)
           (super-mapr-reducer f-expr f-reducer f-stmt f-pat b))))))
     (struct
      reducer-fanout
      reducer
      (a b)
      #:methods
      gen:reducerg
      ((define/generic super-map-reducer map-reducer)
       (define (map-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-fanout a b) (f-reducer e^))
         (reducer-fanout
          (super-map-reducer f-expr f-reducer f-stmt f-pat a)
          (super-map-reducer f-expr f-reducer f-stmt f-pat b)))
       (define/generic super-mapr-reducer mapr-reducer)
       (define (mapr-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-fanout a b) e^)
         (f-reducer
          (reducer-fanout
           (super-mapr-reducer f-expr f-reducer f-stmt f-pat a)
           (super-mapr-reducer f-expr f-reducer f-stmt f-pat b))))))
     (struct
      reducer-add
      reducer
      (e)
      #:methods
      gen:reducerg
      ((define/generic super-map-reducer map-reducer)
       (define (map-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-add e) (f-reducer e^))
         (reducer-add (map-expr f-expr f-reducer f-stmt f-pat e)))
       (define/generic super-mapr-reducer mapr-reducer)
       (define (mapr-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-add e) e^)
         (f-reducer
          (reducer-add (mapr-expr f-expr f-reducer f-stmt f-pat e))))))
     (struct
      reducer-nop
      reducer
      ()
      #:methods
      gen:reducerg
      ((define/generic super-map-reducer map-reducer)
       (define (map-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-nop) (f-reducer e^))
         (reducer-nop))
       (define/generic super-mapr-reducer mapr-reducer)
       (define (mapr-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-nop) e^)
         (f-reducer (reducer-nop)))))
     (struct
      reducer-index
      reducer
      (n i a)
      #:methods
      gen:reducerg
      ((define/generic super-map-reducer map-reducer)
       (define (map-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-index n i a) (f-reducer e^))
         (reducer-index
          (map-expr f-expr f-reducer f-stmt f-pat n)
          (map-expr f-expr f-reducer f-stmt f-pat i)
          (super-map-reducer f-expr f-reducer f-stmt f-pat a)))
       (define/generic super-mapr-reducer mapr-reducer)
       (define (mapr-reducer f-expr f-reducer f-stmt f-pat e^)
         (match-define (reducer-index n i a) e^)
         (f-reducer
          (reducer-index
           (mapr-expr f-expr f-reducer f-stmt f-pat n)
           (mapr-expr f-expr f-reducer f-stmt f-pat i)
           (super-mapr-reducer f-expr f-reducer f-stmt f-pat a)))))))
   (begin
     (struct stmt ())
     (define-generics
      stmtg
      (map-stmt f-expr f-reducer f-stmt f-pat stmtg)
      (mapr-stmt f-expr f-reducer f-stmt f-pat stmtg))
     (struct
      stmt-if
      stmt
      (tst thn els)
      #:methods
      gen:stmtg
      ((define/generic super-map-stmt map-stmt)
       (define (map-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-if tst thn els) (f-stmt e^))
         (stmt-if
          (map-expr f-expr f-reducer f-stmt f-pat tst)
          (super-map-stmt f-expr f-reducer f-stmt f-pat thn)
          (super-map-stmt f-expr f-reducer f-stmt f-pat els)))
       (define/generic super-mapr-stmt mapr-stmt)
       (define (mapr-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-if tst thn els) e^)
         (f-stmt
          (stmt-if
           (mapr-expr f-expr f-reducer f-stmt f-pat tst)
           (super-mapr-stmt f-expr f-reducer f-stmt f-pat thn)
           (super-mapr-stmt f-expr f-reducer f-stmt f-pat els))))))
     (struct
      stmt-for
      stmt
      (i start end body)
      #:methods
      gen:stmtg
      ((define/generic super-map-stmt map-stmt)
       (define (map-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-for i start end body) (f-stmt e^))
         (stmt-for
          (map-expr f-expr f-reducer f-stmt f-pat i)
          (map-expr f-expr f-reducer f-stmt f-pat start)
          (map-expr f-expr f-reducer f-stmt f-pat end)
          (super-map-stmt f-expr f-reducer f-stmt f-pat body)))
       (define/generic super-mapr-stmt mapr-stmt)
       (define (mapr-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-for i start end body) e^)
         (f-stmt
          (stmt-for
           (mapr-expr f-expr f-reducer f-stmt f-pat i)
           (mapr-expr f-expr f-reducer f-stmt f-pat start)
           (mapr-expr f-expr f-reducer f-stmt f-pat end)
           (super-mapr-stmt f-expr f-reducer f-stmt f-pat body))))))
     (struct
      stmt-block
      stmt
      (stmts)
      #:methods
      gen:stmtg
      ((define/generic super-map-stmt map-stmt)
       (define (map-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-block stmts) (f-stmt e^))
         (stmt-block
          (map (curry super-map-stmt f-expr f-reducer f-stmt f-pat) stmts)))
       (define/generic super-mapr-stmt mapr-stmt)
       (define (mapr-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-block stmts) e^)
         (f-stmt
          (stmt-block
           (map
            (curry super-mapr-stmt f-expr f-reducer f-stmt f-pat)
            stmts))))))
     (struct
      stmt-assign
      stmt
      (var val)
      #:methods
      gen:stmtg
      ((define/generic super-map-stmt map-stmt)
       (define (map-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-assign var val) (f-stmt e^))
         (stmt-assign
          (map-expr f-expr f-reducer f-stmt f-pat var)
          (map-expr f-expr f-reducer f-stmt f-pat val)))
       (define/generic super-mapr-stmt mapr-stmt)
       (define (mapr-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-assign var val) e^)
         (f-stmt
          (stmt-assign
           (mapr-expr f-expr f-reducer f-stmt f-pat var)
           (mapr-expr f-expr f-reducer f-stmt f-pat val))))))
     (struct
      stmt-void
      stmt
      ()
      #:methods
      gen:stmtg
      ((define/generic super-map-stmt map-stmt)
       (define (map-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-void) (f-stmt e^))
         (stmt-void))
       (define/generic super-mapr-stmt mapr-stmt)
       (define (mapr-stmt f-expr f-reducer f-stmt f-pat e^)
         (match-define (stmt-void) e^)
         (f-stmt (stmt-void))))))
   (begin
     (struct pat ())
     (define-generics
      patg
      (map-pat f-expr f-reducer f-stmt f-pat patg)
      (mapr-pat f-expr f-reducer f-stmt f-pat patg))
     (struct
      pat-true
      pat
      ()
      #:methods
      gen:patg
      ((define/generic super-map-pat map-pat)
       (define (map-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-true) (f-pat e^))
         (pat-true))
       (define/generic super-mapr-pat mapr-pat)
       (define (mapr-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-true) e^)
         (f-pat (pat-true)))))
     (struct
      pat-false
      pat
      ()
      #:methods
      gen:patg
      ((define/generic super-map-pat map-pat)
       (define (map-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-false) (f-pat e^))
         (pat-false))
       (define/generic super-mapr-pat mapr-pat)
       (define (mapr-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-false) e^)
         (f-pat (pat-false)))))
     (struct
      pat-pair
      pat
      (a b)
      #:methods
      gen:patg
      ((define/generic super-map-pat map-pat)
       (define (map-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-pair a b) (f-pat e^))
         (pat-pair
          (super-map-pat f-expr f-reducer f-stmt f-pat a)
          (super-map-pat f-expr f-reducer f-stmt f-pat b)))
       (define/generic super-mapr-pat mapr-pat)
       (define (mapr-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-pair a b) e^)
         (f-pat
          (pat-pair
           (super-mapr-pat f-expr f-reducer f-stmt f-pat a)
           (super-mapr-pat f-expr f-reducer f-stmt f-pat b))))))
     (struct
      pat-var
      pat
      ()
      #:methods
      gen:patg
      ((define/generic super-map-pat map-pat)
       (define (map-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-var) (f-pat e^))
         (pat-var))
       (define/generic super-mapr-pat mapr-pat)
       (define (mapr-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-var) e^)
         (f-pat (pat-var)))))
     (struct
      pat-ident
      pat
      ()
      #:methods
      gen:patg
      ((define/generic super-map-pat map-pat)
       (define (map-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-ident) (f-pat e^))
         (pat-ident))
       (define/generic super-mapr-pat mapr-pat)
       (define (mapr-pat f-expr f-reducer f-stmt f-pat e^)
         (match-define (pat-ident) e^)
         (f-pat (pat-ident)))))))

(define-syntax (create-pass stx)
  (syntax-case stx (expr reducer stmt pat)
    ((_
      (expr mat-expr ...)
      (reducer mat-reducer ...)
      (stmt mat-stmt ...)
      (pat mat-pat ...))
     #`(letrec ((f-expr (λ (e) (match e mat-expr ... (else e))))
                (f-reducer (λ (e) (match e mat-reducer ... (else e))))
                (f-stmt (λ (e) (match e mat-stmt ... (else e))))
                (f-pat (λ (e) (match e mat-pat ... (else e)))))
         (λ (e) (map-expr f-expr f-reducer f-stmt f-pat e))))))
(define-syntax (create-rpass stx)
  (syntax-case stx (expr reducer stmt pat)
    ((_
      (expr mat-expr ...)
      (reducer mat-reducer ...)
      (stmt mat-stmt ...)
      (pat mat-pat ...))
     #`(letrec ((f-expr (λ (e) (match e mat-expr ... (else e))))
                (f-reducer (λ (e) (match e mat-reducer ... (else e))))
                (f-stmt (λ (e) (match e mat-stmt ... (else e))))
                (f-pat (λ (e) (match e mat-pat ... (else e)))))
         (λ (e) (mapr-expr f-expr f-reducer f-stmt f-pat e))))))
;AG-END

(define internal-ops
  (apply set '(size index recip nat2prob prob2real + * == and < or not
                    categorical)))
(define sum-prod-loops (set 'summate 'product))
(define internal-loop-ops
  (set 'summate 'product 'array))

(define (typeof ast)
  (match ast
    [(expr-fun args ret-type body) 'fn]
    [(expr-if t tst thn els) t]
    [(expr-app t rt rds) t]
    [(expr-let t var val b) t]
    [(expr-sum t i start end b) t]
    [(expr-prd t i start end b) t]
    [(expr-arr t i end b) t]
    [(expr-match t tst brs) t]
    [(expr-bucket t _ _ _) t]
    [(expr-val t v) t]
    [(expr-intr s) '*]
    [(expr-intrf s) '!]
    [(expr-var t s o) t]
    [(expr-bucket t s e b) t]))

(define (pe e)
  (match e
    [(expr-mod main fns)
     `((main ,(pe main))
       ,@(for/list [(fn fns)]
           `(,(car fn) ,(pe (cdr fn)))))]
    [(expr-fun args ret-type body)
     `(function ,(map pe args) ,(pe body))]
    [(expr-var type sym orig) sym #;(string->symbol (format "~a|~a" sym orig))]
    [(expr-arr type index size body)
     `(array ,(pe index) ,(pe size) ,(pe body))]
    [(expr-sum type index start end body)
     `(summate (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-prd type index start end body)
     `(product (,(pe index) ,(pe start) ,(pe end)) ,(pe body))]
    [(expr-bucket type start end reducer)
     `(bucket ,(pe start) ,(pe end) ,(pr reducer))]
    [(expr-bind var body)
     `(/ ,(pe var) -> ,(pe body))]
    [(expr-match type tst branches)
     `(match ,(pe tst) ,@(map pe branches))]
    [(expr-branch pat body)
     `[,(pp pat) ,(pe body)]]
    [(expr-if type tst thn els)
     `(if ,(pe tst) ,(pe thn) ,(pe els))]
    [(expr-app type rator rands)
     `(,(pe rator) ,@(map pe rands))]
    [(expr-let type var val body)
     `(let (,(pe var) ,(pe val)) ,(pe body))]
    [(expr-lets types vars vals body)
     `(let (,@(for/list ( [var vars] [val vals])
                `(,(pe var) ,(pe val))))
        ,(pe body))]
    [(expr-block t stmt e)
     `(expr-block ,(ps stmt) ,(pe e))]
    [(expr-intr s) s]
    [(expr-intrf s) s]
    [(expr-val t v) v]
    [else `(? ,e)]))
(define print-expr pe)
(define display-expr (compose pretty-display print-expr))
(define (pr red)
  (match red
    [(reducer-split e a b) `(split ,(pe e) ,(pr a) ,(pr b))]
    [(reducer-fanout a b) `(fanout ,(pr a) ,(pr b))]
    [(reducer-add i) `(add ,(pe i))]
    [(reducer-nop) `(nop)]
    [(reducer-index i e b) `(index ,(pe i) ,(pe e) ,(pr b))]))
(define print-reducer pr)
(define display-reducer (compose pretty-display print-reducer))
(define (pp pat)
  (match pat
    [(pat-var) 'var]
    [(pat-true) 'true]
    [(pat-false) 'false]
    [(pat-pair a b) `(pair ,(pp a) ,(pp b))]
    [(pat-ident) 'rec]))
(define print-pattern pp)
(define display-pattern (compose pretty-display print-pattern))
(define (ps stmt)
  (match stmt
    [(stmt-if tst thn els) `(if-stmt ,(pe tst) ,(ps thn) ,(ps els))]
    [(stmt-for i start end body) `(for-stmt (,(pe i) ,(pe start) ,(pe end)) ,(ps body))]
    [(stmt-block stmts) `(block-stmt ,@(map ps stmts))]
    [(stmt-assign var val) `(set! ,(pe var) ,(pe val))]
    [(stmt-void) '<void>]))
(define print-stmt ps)
(define display-stmt (compose pretty-display print-stmt))

(define-syntax (expr/pass stx)
  (syntax-case stx ()
      [(_ mps ...)
       #'(λ (e)
           (letrec
               ((f (λ (e)
                     (define ne (match e mps ... [else e]))
                     (fill-expr-pass f ne))))
             (f e)))]))

(define-syntax (fill-expr-pass stx)
  (syntax-case stx ()
    [(_ f e mps ...)
     #'(match e
         mps ...
         [(expr-mod main fns)
          (expr-mod (f main) (map f fns))]
         [(expr-fun args ret-type body)
          (expr-fun args ret-type (f body))]
         [(expr-let type var val body)
          (expr-let type var (f val) (f body))]
         [(expr-lets types vars vals body)
          (expr-lets types vars (map f vals) (f body))]
         [(expr-arr t i s b)
          (expr-arr t (f i) (f s) (f b))]
         [(expr-sum t i s e b)
          (expr-sum t (f i) (f s) (f e) (f b))]
         [(expr-prd t i s e b)
          (expr-prd t (f i) (f s) (f e) (f b))]
         [(expr-bucket t s e r)
          (expr-bucket t (f s) (f e) r)]
         [(expr-branch pat body)
          (expr-branch pat (f body))]
         [(expr-match t tst brs)
          (expr-match t (f tst) (map f brs))]
         [(expr-bind v b)
          (expr-bind v (f b))]
         [(expr-if t tst thn els)
          (expr-if t (f tst) (f thn) (f els))]
         [(expr-app t ra rs)
          (expr-app t (f ra) (map f rs))]
         [(expr-block t stmt e)
          (expr-block t (f stmt) (f e))]
         [(stmt-if tst thn els)
          (stmt-if (f tst) (f thn) (f els))]
         [(stmt-for i start end body)
          (stmt-for i (f start) (f end) (f body))]
         [(stmt-block stmts)
          (stmt-block (map f stmts))]
         [(stmt-assign var val)
          (stmt-assign var (f val))]
         [else e])]))
