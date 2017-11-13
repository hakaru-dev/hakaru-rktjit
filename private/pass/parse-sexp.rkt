#lang racket

(require "ast.rkt")
(require "utils.rkt")

(provide parse-sexp)

(define (sa e env)
  (match e
    [`(fn ,args ,ret-type ,body)
     (define aes (for/list [(arg args)]
                   (expr-var (cadr arg) (car arg) (car arg))))
     (define arg-env
       (for/fold [(env env)]
                 [(arg args)
                  (ae aes)]
         (hash-set env (car arg) ae)))
     (expr-mod
      (expr-fun aes ret-type
                (sa body arg-env))
      '())]
    [`((let (,var ,type ,val) ,body) : ,t)
     (define ve (expr-var type var var))
     (define vale (sa val env))
     (expr-lets (list type) (list ve) (list vale) (stmt-void) (sa body (hash-set env var ve)))]
    [`((summate (,index ,start ,end) ,body) : ,type)
     (define ie (expr-var 'nat (gensym^ 'si) index))
     (expr-sum type ie (sa start env) (sa end env)
               (sa body (hash-set env index ie)))]
    [`((product (,index ,start ,end) ,body) : ,type)
     (define ie (expr-var 'nat (gensym^ 'pi) index))
     (expr-prd type ie (sa start env) (sa end env)
               (sa body (hash-set env index ie)))]
    [`((array (,index ,size) ,body) : ,type)
     (define ie (expr-var 'nat (gensym^ 'ai) index))
     (expr-arr type  ie (sa size env) (sa body (hash-set env index ie)))]
    [`((bucket ,start ,end ,reducer) : ,type)
     (expr-bucket type (sa start env) (sa end env) (sr reducer env))]
    [`((match ,tst ,brnchs ...) : ,type)
     (expr-match type (sa tst env) (map (curryr sb env) brnchs))]
    [`(bind ,v ,e)
     (define ve (expr-var 'bind (gensym^ 'bi) v))
     (expr-bind ve (sa e (hash-set env v ve)))]
    [`((superpose (,p ,m) ...) : ,type)
     (define ms (map (curryr sa env) m))
     (define ps (map (curryr sa env) p))
     (expr-app type (expr-intrf 'superpose) (apply append (map list ps ms)))]
    [`((datum ,k ,v) : ,typ)
     (dtprintf "datum: k ~a, v ~a, typ: ~a\n" k v typ)
     (define (get-datum kind val type)
       (match kind
         ['pair
          (match val
            [`(inl (et (konst ,v1) (et (konst ,v2) done)))
             (expr-app typ (expr-intrf 'cons) (list (sa v1 env) (sa v2 env)))])]
         ['true (expr-val typ 1)]
         ['false (expr-val typ 0)]))
     (get-datum k v typ)]
    [`((prob_ ,v1 % ,v2) : prob)
     (expr-app 'prob (expr-intrf 'real2prob)
               (list (expr-val 'real (exact->inexact
                                      (/ (get-value v1) (get-value v2))))))]
    [`((real_ ,v1 % ,v2) : real)
     (expr-val 'real (exact->inexact (/ (get-value v1) (get-value v2))))]
    [`((nat_ ,v) : nat)
     #:when (and (number? v))
     (expr-val 'nat v)]
    [`((,rator ,rands ...) : ,type)
     (define randse (map (curryr sa env) rands))
     (expr-app type (expr-intrf rator) randse)]
    [`(,s : ,type) #:when (and (symbol? s)
                               (hash-has-key? env s))
                   (define orig (hash-ref env s))
                   (if (equal? (expr-var-type orig) 'bind)
                       (expr-var type (expr-var-sym orig) s)
                       (hash-ref env s))]
    [`(,s : ,type) #:when (number? s)
                   (expr-val type s)]
    [`(,s : ,type)
     #:when (and (symbol? s) (hash-has-key? env e))
     (hash-ref env s)]))
    ;; [else (error (format "match: no matching clause found for ~a" e))]
(define (get-value v)
  (match v
    [(? number?) v]
    [`(,n) #:when (number? n) n]))

(define (sr r env)
  (match r
    [`(r_split ,e ,ra ,rb)
     (reducer-split (sa e env) (sr ra env) (sr rb env))]
    [`(r_fanout ,ra ,rb)
     (reducer-fanout (sr ra env) (sr rb env))]
    [`(r_add ,i)
     (reducer-add (sa i env))]
    [`r_nop
     (reducer-nop)]
    [`(r_index ,i ,e ,rb)
     (reducer-index (sa i env) (sa e env) (sr rb env))]
    [else (error "unknown reducer " r)]))

(define (sb b env)
  (match b
    [`(,pat ,e)
     (expr-branch (sp pat env) (sa e env))]
    [else (error "unknown match branch format " b)]))

(define (sp pat env)
  (define (pf f)
    (match f
      [`(pf_konst var) (pat-var)]
      [`(pf_ident) (pat-ident)]))
  (define (ps s)
    (match s
      [`(ps_et ,f ,s) (cons (pf f) (ps s))]
      [`(ps_done) '()]))
  (define (pc c)
    (match c
      [`(pc_inr ,c) (pc c)]
      [`(pc_inl ,s) (ps s)]))
  (match pat
    [`(pdatum true ,_)  (pat-true)]
    [`(pdatum false ,_) (pat-false)]
    [`(pdatum pair ,c)  (match-define (list a b) (pc c)) (pat-pair a b)]))

;;S-expression to ast struct
(define (parse-sexp state)
  (sa expr (make-immutable-hash)))

(module+ test
  (print-expr
   (parse-sexp
    '(fn
         ((topic_prior (array prob))
          (word_prior (array prob))
          (z (array nat))
          (w (array nat))
          (doc (array nat))
          (docUpdate nat))
       (measure nat)
       ((categorical
         ((array
           (zNew丏 ((size (topic_prior : (array prob))) : nat))
           ((*
             ((product
               (i (0 : nat) ((size (topic_prior : (array prob))) : nat))
               ((product
                 (i丣 (0 : nat) ((size (word_prior : (array prob))) : nat))
                 ((product
                   (j
                    (0 : nat)
                    ((let (summary
                           (pair (pair (array nat) unit) unit)
                           ((bucket
                             (0 : nat)
                             ((size (w : (array nat))) : nat)
                             (r_fanout
                              (r_split
                               (bind
                                i丙
                                ((== (docUpdate : nat) ((index (doc : (array nat)) (i丙 : nat)) : nat))
                                 :
                                 bool))
                               (r_index
                                ((size (word_prior : (array prob))) : nat)
                                (bind i丙 ((index (w : (array nat)) (i丙 : nat)) : nat))
                                (r_add (bind i丙 (bind i丣 (1 : nat)))))
                               r_nop)
                              r_nop))
                            :
                            (pair (pair (array nat) unit) unit)))
                       ((match
                            ((== (i : nat) (zNew丏 : nat)) : bool)
                          ((datum true (pc_inl (ps_done)))
                           ((index
                             ((match
                                  ((match
                                       (summary : (pair (pair (array nat) unit) unit))
                                     ((datum
                                       pair
                                       (pc_inl (ps_et (pf_konst var) (ps_et (pf_konst var) (ps_done)))))
                                      (bind y (bind z y))))
                                   :
                                   (pair (array nat) unit))
                                ((datum
                                  pair
                                  (pc_inl (ps_et (pf_konst var) (ps_et (pf_konst var) (ps_done)))))
                                 (bind y (bind z y))))
                              :
                              (array nat))
                             (i丣 : nat))
                            :
                            nat))
                          ((datum false (pc_inr (pc_inl (ps_done)))) (0 : nat)))
                        :
                        nat))
                     :
                     nat))
                   ((+
                     ((nat2prob
                       ((let (summary
                              (pair unit (array (array nat)))
                              ((bucket
                                (0 : nat)
                                ((size (w : (array nat))) : nat)
                                (r_split
                                 (bind
                                  i丙
                                  ((== ((index (doc : (array nat)) (i丙 : nat)) : nat) (docUpdate : nat))
                                   :
                                   bool))
                                 r_nop
                                 (r_index
                                  ((size (word_prior : (array prob))) : nat)
                                  (bind i丙 ((index (w : (array nat)) (i丙 : nat)) : nat))
                                  (r_index
                                   (bind i丣 ((size (topic_prior : (array prob))) : nat))
                                   (bind
                                    i丙
                                    (bind
                                     i丣
                                     ((index
                                       (z : (array nat))
                                       ((index (doc : (array nat)) (i丙 : nat)) : nat))
                                      :
                                      nat)))
                                   (r_add (bind i丙 (bind i (bind i丣 (1 : nat)))))))))
                               :
                               (pair unit (array (array nat)))))
                          ((index
                            ((index
                              ((match
                                   (summary : (pair unit (array (array nat))))
                                 ((datum
                                   pair
                                   (pc_inl (ps_et (pf_konst var) (ps_et (pf_konst var) (ps_done)))))
                                  (bind y (bind z z))))
                               :
                               (array (array nat)))
                              (i丣 : nat))
                             :
                             (array nat))
                            (i : nat))
                           :
                           nat))
                        :
                        nat))
                      :
                      prob)
                     ((nat2prob (j : nat)) : prob)
                     ((index (word_prior : (array prob)) (i丣 : nat)) : prob))
                    :
                    prob))
                  :
                  prob))
                :
                prob))
              :
              prob)
             ((+
               ((let (summary
                      (pair unit (array nat))
                      ((bucket
                        (0 : nat)
                        ((size (z : (array nat))) : nat)
                        (r_split
                         (bind i丙 ((== (i丙 : nat) (docUpdate : nat)) : bool))
                         r_nop
                         (r_index
                          ((size (topic_prior : (array prob))) : nat)
                          (bind i丙 ((index (z : (array nat)) (i丙 : nat)) : nat))
                          (r_add (bind i丙 (bind zNew丏 (1 : nat)))))))
                       :
                       (pair unit (array nat))))
                  ((nat2prob
                    ((index
                      ((match
                           (summary : (pair unit (array nat)))
                         ((datum pair (pc_inl (ps_et (pf_konst var) (ps_et (pf_konst var) (ps_done)))))
                          (bind y (bind z z))))
                       :
                       (array nat))
                      (zNew丏 : nat))
                     :
                     nat))
                   :
                   prob))
                :
                prob)
               ((index (topic_prior : (array prob)) (zNew丏 : nat)) : prob))
              :
              prob)
             ((recip
               ((+
                 ((nat2prob
                   ((summate
                     (i丙 (0 : nat) ((size (z : (array nat))) : nat))
                     ((match
                          ((== (i丙 : nat) (docUpdate : nat)) : bool)
                        ((datum true (pc_inl (ps_done))) (0 : nat))
                        ((datum false (pc_inr (pc_inl (ps_done))))
                         ((match
                              ((< ((index (z : (array nat)) (i丙 : nat)) : nat) (0 : nat)) : bool)
                            ((datum true (pc_inl (ps_done))) (0 : nat))
                            ((datum false (pc_inr (pc_inl (ps_done)))) (1 : nat)))
                          :
                          nat)))
                      :
                      nat))
                    :
                    nat))
                  :
                  prob)
                 ((summate
                   (i丙 (0 : nat) ((size (topic_prior : (array prob))) : nat))
                   ((index (topic_prior : (array prob)) (i丙 : nat)) : prob))
                  :
                  prob))
                :
                prob))
              :
              prob)
             ((recip
               ((product
                 (i (0 : nat) ((size (topic_prior : (array prob))) : nat))
                 ((product
                   (i丣
                    (0 : nat)
                    ((let (summary
                           (pair (pair unit (pair nat unit)) unit)
                           ((bucket
                             (0 : nat)
                             ((size (w : (array nat))) : nat)
                             (r_fanout
                              (r_split
                               (bind
                                i丙
                                ((< ((index (w : (array nat)) (i丙 : nat)) : nat) (0 : nat)) : bool))
                               r_nop
                               (r_split
                                (bind
                                 i丙
                                 ((== (docUpdate : nat) ((index (doc : (array nat)) (i丙 : nat)) : nat))
                                  :
                                  bool))
                                (r_add (bind i丙 (1 : nat)))
                                r_nop))
                              r_nop))
                            :
                            (pair (pair unit (pair nat unit)) unit)))
                       ((match
                            ((== (i : nat) (zNew丏 : nat)) : bool)
                          ((datum true (pc_inl (ps_done)))
                           ((match
                                ((match
                                     ((match
                                          (summary : (pair (pair unit (pair nat unit)) unit))
                                        ((datum
                                          pair
                                          (pc_inl (ps_et (pf_konst var) (ps_et (pf_konst var) (ps_done)))))
                                         (bind y (bind z y))))
                                      :
                                      (pair unit (pair nat unit)))
                                   ((datum
                                     pair
                                     (pc_inl (ps_et (pf_konst var) (ps_et (pf_konst var) (ps_done)))))
                                    (bind y (bind z z))))
                                 :
                                 (pair nat unit))
                              ((datum pair (pc_inl (ps_et (pf_konst var) (ps_et (pf_konst var) (ps_done)))))
                               (bind y (bind z y))))
                            :
                            nat))
                          ((datum false (pc_inr (pc_inl (ps_done)))) (0 : nat)))
                        :
                        nat))
                     :
                     nat))
                   ((+
                     ((nat2prob
                       ((let (summary
                              (pair unit (pair unit (array nat)))
                              ((bucket
                                (0 : nat)
                                ((size (w : (array nat))) : nat)
                                (r_split
                                 (bind
                                  i丙
                                  ((< ((index (w : (array nat)) (i丙 : nat)) : nat) (0 : nat)) : bool))
                                 r_nop
                                 (r_split
                                  (bind
                                   i丙
                                   ((== ((index (doc : (array nat)) (i丙 : nat)) : nat) (docUpdate : nat))
                                    :
                                    bool))
                                  r_nop
                                  (r_index
                                   ((size (topic_prior : (array prob))) : nat)
                                   (bind
                                    i丙
                                    ((index
                                      (z : (array nat))
                                      ((index (doc : (array nat)) (i丙 : nat)) : nat))
                                     :
                                     nat))
                                   (r_add (bind i丙 (bind i (1 : nat))))))))
                               :
                               (pair unit (pair unit (array nat)))))
                          ((index
                            ((match
                                 ((match
                                      (summary : (pair unit (pair unit (array nat))))
                                    ((datum
                                      pair
                                      (pc_inl (ps_et (pf_konst var) (ps_et (pf_konst var) (ps_done)))))
                                     (bind y (bind z z))))
                                  :
                                  (pair unit (array nat)))
                               ((datum
                                 pair
                                 (pc_inl (ps_et (pf_konst var) (ps_et (pf_konst var) (ps_done)))))
                                (bind y (bind z z))))
                             :
                             (array nat))
                            (i : nat))
                           :
                           nat))
                        :
                        nat))
                      :
                      prob)
                     ((nat2prob (i丣 : nat)) : prob)
                     ((summate
                       (i丙 (0 : nat) ((size (word_prior : (array prob))) : nat))
                       ((index (word_prior : (array prob)) (i丙 : nat)) : prob))
                      :
                      prob))
                    :
                    prob))
                  :
                  prob))
                :
                prob))
              :
              prob))
            :
            prob))
          :
          (array prob)))
        :
        (measure nat))))))
  ;; (define ps
  ;;   (parse-sexp
  ;;    '(fn
  ;;         ((topic_prior (array prob))
  ;;          (word_prior (array prob))
  ;;          (z (array nat))
  ;;          (w (array nat))
  ;;          (doc (array nat))
  ;;          (docUpdate nat))
  ;;       (array prob)
  ;;       ((let (topic_prior_size  nat ((size (topic_prior : (array prob))) : nat))
  ;;          (topic_prior : (array prob)))
  ;;        :
  ;;        (array prob)))))
