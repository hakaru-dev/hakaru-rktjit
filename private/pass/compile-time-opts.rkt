#lang racket

(require "ast.rkt"
         "utils.rkt"
         ffi/unsafe
         sham/private/llvm/ffi/all)
;; TODO add in sham creating constant llvm values.

(provide compile-opts)

;; these functions assume we are doing stuff for known sized array
(define (get-llvm-type type)
  (match type
    [`(array ,type (size . ,len))
     (define etype (get-llvm-type type))
     (LLVMArrayType etype len)]
    ['nat (LLVMInt64Type)]
    ['int (LLVMInt64Type)]
    ['real (LLVMDoubleType)]
    ['prob (LLVMDoubleType)]
    [`(nat ,_ ...) (LLVMInt64Type)]
    [`(int ,_ ...) (LLVMInt64Type)]
    [`(real ,_ ...) (LLVMDoubleType)]
    [`(prob ,_ ...) (LLVMDoubleType)]))
(define (get-racket-type type)
  (match (second type)
    ['nat _uint64]
    ['int _uint64]
    ['real _double]
    ['prob _double]
    [`(nat ,_ ...) _uint64]
    [`(int ,_ ...) _uint64]
    [`(real ,_ ...) _double]
    [`(prob ,_ ...) _double]
    [else _pointer]))

(define (get-llvm-ptr rptr ltype)
  (LLVMConstIntToPtr (LLVMConstInt (LLVMInt64Type) (cast rptr _pointer _uintptr) #f) ltype))

(define (get-cltype type)
  (values (get-racket-type type)
          (LLVMPointerType (get-llvm-type type) 0)))
(define (llvm-ptr-list l type)
  (define-values (ctype ltype) (get-cltype type))
  (get-llvm-ptr (list->cblock l ctype) ltype))

(define (llvm-ptr-empty-list type)
  (define (get-list-size type)
    (match type
      [`(array ,etype (size . ,v)) (* v (get-list-size etype))]
      [else 1]))
  (define-values (ctype ltype) (get-cltype type))
  (get-llvm-ptr (malloc ctype (get-list-size type)) ltype))

(define (get-constant arg)
  (match-define (expr-var atype asym ainfo) arg)
  (define var (expr-var atype (gensym^ (symbol-append 'constant- asym)) ainfo))
  (expr-cvar var (if (not (pair? (second atype)))
                     (llvm-ptr-list (assocv ainfo 'value) atype)
                     #f)))

(define (can-be-constant-let? triple)
  ;; todo do recursion after first step
  (match-define (list type var val) triple)
  (and (array-type? type)
       (constant-size-array? type)
       (expr-app? val)
       (equal? 'empty (expr-intrf-sym (expr-app-rator val)))
       (or (not (array-type? (array-element-type type)))
           (constant-size-array? (array-element-type type)))))
(define (get-constant-lets triple)
  (match-define (list type var val) triple)
  (define cvar (expr-var type (gensym^ (symbol-append 'constant- (expr-var-sym var))) (expr-var-info var)))
  (define cval (expr-val  `(pointer ,type) 0))
  (expr-cvar cvar (llvm-ptr-empty-list type)))

(define (is-constant? info)
  (member 'constant (get-info-attrs info)))

(define (compile-opts st)
  (define constants (list-box))
  (define pass
    (create-rpass
     (expr [(expr-lets types vars vals sbody ebody)
            (define tvv (map list types vars vals))
            (define-values (can cant) (partition can-be-constant-let? tvv))
            ;; (printf "let cans: ~a\n" (map (compose pe second) can))
            (define gls (map get-constant-lets can))
            (set-box! constants (append (unbox constants) gls))
            (expr-lets (append (map first cant) (map first can))
                       (append (map second cant) (map second can))
                       (append (map third cant) gls)
                       sbody ebody)])
     (reducer) (stmt) (pat)))
  (define (optimize fun info)
    (match fun
      [(expr-fun name args ret-type body)
       (define-values (can cant)
         (partition (compose is-constant? get-arg-info expr-var-info)
                    args))
       (printf "arg cans:~a\n" (map pe can))
       (define gl (map get-constant can))
       (set-box! constants (append (unbox constants) gl))
       (expr-fun name cant ret-type
                 (stmt-expr
                  (stmt-void)
                  (expr-lets (map typeof can) can gl
                             (pass body) (expr-val 'nat 0))))]))
  (match st
    [(state prgs info os)
     (define funs (map (curryr optimize info) prgs))
     (run-next funs info st)]))
