#lang racket

(require ffi/unsafe)
(require sham/jit
         sham/ast)
(require "jit-utils.rkt"
         "utils.rkt"
         "basic-defines.rkt"
         "jit.rkt")
(define ast
  (sexp->sham-ast
   `(module (passes AlwaysInliner) (define-type nat i32)
      (define-type int i32)
      (define-type real f64)
      (define-type prob f64)
      (define-type nat* (* nat))
      (define-type nat** (* nat*))
      (define-type nat*** (* nat**))
      (define-type real* (* real))
      (define-type real** (* real*))
      (define-type real*** (* real**))
      (define-type prob* (* prob))
      (define-type prob** (* prob*))
      (define-type prob*** (* prob**))
      (define-type array<real> (struct ((size : i32) (data : real*))))
      (define-type array<prob> (struct ((size : i32) (data : prob*))))
      (define-type array<nat> (struct ((size : i32) (data : nat*))))
      (define-type array<nat>* (* array<nat>))
      (define-type array<real>* (* array<real>))
      (define-type array<prob>* (* array<prob>))
      (define-type array<nat>** (* array<nat>*))
      (define-type array<real>** (* array<real>*))
      (define-type array<prob>** (* array<prob>*))
      (define-type array<array<nat>*> (struct ((size : i32) (data : array<nat>**))))
      (define-type array<array<real>*> (struct ((size : i32) (data : array<real>**))))
      (define-type array<array<prob>*> (struct ((size : i32) (data : array<prob>**))))
      (define-type array<array<nat>*>* (* array<array<nat>*>))
      (define-type array<array<real>*>* (* array<array<real>*>))
      (define-type array<array<prob>*>* (* array<array<prob>*>))
      (define-function (passes)
        (attrs AlwaysInline)
        (nat2prob (v : nat) : prob)
        (return (ui->fp v (%type real))))
      (define-function (passes)
        (attrs AlwaysInline)
        (prob2real (v : prob) : real)
        (return ((llvm.exp.f64 :-> prob) v)))
      (define-function (passes)
        (attrs AlwaysInline)
        (real2prob (v : real) : prob)
        (return ((llvm.log.f64 :-> prob) v)))
      (define-function (passes)
        (attrs AlwaysInline)
        (recip-nat (v : nat) : real)
        (return (fdiv (%float 1.0 real) (ui->fp v (%type real)))))
      (define-function (passes)
        (attrs AlwaysInline)
        (recip-real (v : real) : real)
        (return (fdiv (%float 1.0 real) v)))
      (define-function (passes)
        (attrs AlwaysInline)
        (recip-prob (v : real) : real)
        (return (fmul (%float -1.0 real) v)))
      (define-function (passes)
        (attrs AlwaysInline)
        (add-2-nat (v1 : nat) (v2 : nat) : nat)
        (return (add-nuw v1 v2)))
      (define-function (passes)
        (attrs AlwaysInline)
        (add-2-real (v1 : real) (v2 : real) : real)
        (return (fadd v1 v2)))
      (define-function (passes)
        (attrs AlwaysInline)
        (add-3-real (v1 : real) (v2 : real) (v3 : real) : real)
        (return (fadd (fadd v1 v2) v3)))
      (define-function (passes)
        (attrs AlwaysInline)
        (add-2-prob (v1 : prob) (v2 : prob) : prob)
        (return (real2prob (add-2-real (prob2real v1) (prob2real v2)))))
      (define-function (passes)
        (attrs AlwaysInline)
        (add-3-prob (v1 : prob) (v2 : prob) (v3 : prob) : prob)
        (return (real2prob (add-3-real (prob2real v1) (prob2real v2) (prob2real v3)))))
      (define-function (passes)
        (attrs AlwaysInline)
        (mul-2-nat (v1 : nat) (v2 : nat) : nat)
        (return (mul-nuw v1 v2)))
      (define-function (passes)
        (attrs AlwaysInline)
        (mul-2-real (v1 : real) (v2 : real) : real)
        (return (fmul v1 v2)))
      (define-function (passes)
        (attrs AlwaysInline)
        (mul-2-prob (v1 : prob) (v2 : prob) : prob)
        (return (fadd v1 v2)))
      (define-function (passes)
        (attrs AlwaysInline)
        (mul-4-prob (v1 : prob) (v2 : prob) (v3 : prob) (v4 : prob) : prob)
        (return (fadd v4 (fadd v3 (fadd v1 v2)))))
      (define-function (passes)
        (attrs AlwaysInline)
        (make-array<nat> (size : i32) (data : (* nat)) : (* (struct ((size : i32) (data : (* nat))))))
        (let ((ap
               :
               (* (struct ((size : i32) (data : (* nat)))))
               (malloc (%type (struct ((size : i32) (data : (* nat)))))))
              (ap-size* : (* i32) (%gep ap (%uint 0 nat) (%uint 0 nat)))
              (ap-data* : (* (* nat)) (%gep ap (%uint 0 nat) (%uint 1 nat))))
          (block (expr (store! size ap-size*)) (expr (store! data ap-data*)) (return ap))))
      (define-function (passes)
        (attrs AlwaysInline)
        (get-ptr-array<nat> (ap : (* (struct ((size : i32) (data : (* nat)))))) : (* nat))
        (let ((atp : (* (* nat)) (%gep ap (%uint 0 nat) (%uint 1 nat)))) (return (load atp))))
      (define-function (passes)
        (attrs AlwaysInline)
        (empty-array<nat> (size : i32) : (* (struct ((size : i32) (data : (* nat))))))
        (let ((ap
               :
               (* (struct ((size : i32) (data : (* nat)))))
               (malloc (%type (struct ((size : i32) (data : (* nat)))))))
              (data : (* nat) (arr-malloc (%type nat) size))
              (datap : (* (* nat)) (%gep ap (%uint 0 nat) (%uint 1 nat)))
              (sizep : (* i32) (%gep ap (%uint 0 nat) (%uint 0 nat))))
          (block (expr (store! size sizep)) (expr (store! data datap)) (return ap))))
      (define-function (passes)
        (attrs AlwaysInline)
        (size-array<nat> (array-ptr : (* (struct ((size : i32) (data : (* nat)))))) : i32)
        (return (load (%gep array-ptr (%uint 0 nat) (%uint 0 nat)))))
      (define-function (passes)
        (attrs AlwaysInline)
        (index-array<nat>
         (array-ptr : (* (struct ((size : i32) (data : (* nat))))))
         (index : i32)
         :
         nat)
        (return (load (%gep (load (%gep array-ptr (%uint 0 nat) (%uint 1 nat))) index))))
      (define-function (passes)
        (attrs AlwaysInline)
        (set-index-in-array<nat>
         (array-ptr : (* (struct ((size : i32) (data : (* nat))))))
         (index : i32)
         (v : nat)
         :
         void)
        (block
         (expr (store! v (%gep (load (%gep array-ptr (%uint 0 nat) (%uint 1 nat))) index)))
         (return-void)))
      (define-function (passes)
        (attrs AlwaysInline)
        (empty-zero-array<nat> : (* (struct ((size : i32) (data : (* nat))))))
        (return (empty-array<nat> (%uint 0 nat))))
      (define-function (passes)
        (attrs AlwaysInline)
        (make-array<real>
         (size : i32)
         (data : (* real))
         :
         (* (struct ((size : i32) (data : (* real))))))
        (let ((ap
               :
               (* (struct ((size : i32) (data : (* real)))))
               (malloc (%type (struct ((size : i32) (data : (* real)))))))
              (ap-size* : (* i32) (%gep ap (%uint 0 nat) (%uint 0 nat)))
              (ap-data* : (* (* real)) (%gep ap (%uint 0 nat) (%uint 1 nat))))
          (block (expr (store! size ap-size*)) (expr (store! data ap-data*)) (return ap))))
      (define-function (passes)
        (attrs AlwaysInline)
        (get-ptr-array<real> (ap : (* (struct ((size : i32) (data : (* real)))))) : (* real))
        (let ((atp : (* (* real)) (%gep ap (%uint 0 nat) (%uint 1 nat)))) (return (load atp))))
      (define-function (passes)
        (attrs AlwaysInline)
        (empty-array<real> (size : i32) : (* (struct ((size : i32) (data : (* real))))))
        (let ((ap
               :
               (* (struct ((size : i32) (data : (* real)))))
               (malloc (%type (struct ((size : i32) (data : (* real)))))))
              (data : (* real) (arr-malloc (%type real) size))
              (datap : (* (* real)) (%gep ap (%uint 0 nat) (%uint 1 nat)))
              (sizep : (* i32) (%gep ap (%uint 0 nat) (%uint 0 nat))))
          (block (expr (store! size sizep)) (expr (store! data datap)) (return ap))))
      (define-function (passes)
        (attrs AlwaysInline)
        (size-array<real> (array-ptr : (* (struct ((size : i32) (data : (* real)))))) : i32)
        (return (load (%gep array-ptr (%uint 0 nat) (%uint 0 nat)))))
      (define-function (passes)
        (attrs AlwaysInline)
        (index-array<real>
         (array-ptr : (* (struct ((size : i32) (data : (* real))))))
         (index : i32)
         :
         real)
        (return (load (%gep (load (%gep array-ptr (%uint 0 nat) (%uint 1 nat))) index))))
      (define-function (passes)
        (attrs AlwaysInline)
        (set-index-in-array<real>
         (array-ptr : (* (struct ((size : i32) (data : (* real))))))
         (index : i32)
         (v : real)
         :
         void)
        (block
         (expr (store! v (%gep (load (%gep array-ptr (%uint 0 nat) (%uint 1 nat))) index)))
         (return-void)))
      (define-function (passes)
        (attrs AlwaysInline)
        (empty-zero-array<real> : (* (struct ((size : i32) (data : (* real))))))
        (return (empty-array<real> (%uint 0 nat))))
      (define-function (passes)
        (attrs AlwaysInline)
        (make-array<prob>
         (size : i32)
         (data : (* prob))
         :
         (* (struct ((size : i32) (data : (* prob))))))
        (let ((ap
               :
               (* (struct ((size : i32) (data : (* prob)))))
               (malloc (%type (struct ((size : i32) (data : (* prob)))))))
              (ap-size* : (* i32) (%gep ap (%uint 0 nat) (%uint 0 nat)))
              (ap-data* : (* (* prob)) (%gep ap (%uint 0 nat) (%uint 1 nat))))
          (block (expr (store! size ap-size*)) (expr (store! data ap-data*)) (return ap))))
      (define-function (passes)
        (attrs AlwaysInline)
        (get-ptr-array<prob> (ap : (* (struct ((size : i32) (data : (* prob)))))) : (* prob))
        (let ((atp : (* (* prob)) (%gep ap (%uint 0 nat) (%uint 1 nat)))) (return (load atp))))
      (define-function (passes)
        (attrs AlwaysInline)
        (empty-array<prob> (size : i32) : (* (struct ((size : i32) (data : (* prob))))))
        (let ((ap
               :
               (* (struct ((size : i32) (data : (* prob)))))
               (malloc (%type (struct ((size : i32) (data : (* prob)))))))
              (data : (* prob) (arr-malloc (%type prob) size))
              (datap : (* (* prob)) (%gep ap (%uint 0 nat) (%uint 1 nat)))
              (sizep : (* i32) (%gep ap (%uint 0 nat) (%uint 0 nat))))
          (block (expr (store! size sizep)) (expr (store! data datap)) (return ap))))
      (define-function (passes)
        (attrs AlwaysInline)
        (size-array<prob> (array-ptr : (* (struct ((size : i32) (data : (* prob)))))) : i32)
        (return (load (%gep array-ptr (%uint 0 nat) (%uint 0 nat)))))
      (define-function (passes)
        (attrs AlwaysInline)
        (index-array<prob>
         (array-ptr : (* (struct ((size : i32) (data : (* prob))))))
         (index : i32)
         :
         prob)
        (return (load (%gep (load (%gep array-ptr (%uint 0 nat) (%uint 1 nat))) index))))
      (define-function (passes)
        (attrs AlwaysInline)
        (set-index-in-array<prob>
         (array-ptr : (* (struct ((size : i32) (data : (* prob))))))
         (index : i32)
         (v : prob)
         :
         void)
        (block
         (expr (store! v (%gep (load (%gep array-ptr (%uint 0 nat) (%uint 1 nat))) index)))
         (return-void)))
      (define-function (passes)
        (attrs AlwaysInline)
        (empty-zero-array<prob> : (* (struct ((size : i32) (data : (* prob))))))
        (return (empty-array<prob> (%uint 0 nat))))
      (define-function (passes)
        (attrs AlwaysInline)
        (make-array<array<nat>*>
         (size : i32)
         (data : (* array<nat>*))
         :
         (* (struct ((size : i32) (data : (* array<nat>*))))))
        (let ((ap
               :
               (* (struct ((size : i32) (data : (* array<nat>*)))))
               (malloc (%type (struct ((size : i32) (data : (* array<nat>*)))))))
              (ap-size* : (* i32) (%gep ap (%uint 0 nat) (%uint 0 nat)))
              (ap-data* : (* (* array<nat>*)) (%gep ap (%uint 0 nat) (%uint 1 nat))))
          (block (expr (store! size ap-size*)) (expr (store! data ap-data*)) (return ap))))
      (define-function (passes)
        (attrs AlwaysInline)
        (get-ptr-array<array<nat>*>
         (ap : (* (struct ((size : i32) (data : (* array<nat>*))))))
         :
         (* array<nat>*))
        (let ((atp : (* (* array<nat>*)) (%gep ap (%uint 0 nat) (%uint 1 nat)))) (return (load atp))))
      (define-function (passes)
        (attrs AlwaysInline)
        (empty-array<array<nat>*> (size : i32) : (* (struct ((size : i32) (data : (* array<nat>*))))))
        (let ((ap
               :
               (* (struct ((size : i32) (data : (* array<nat>*)))))
               (malloc (%type (struct ((size : i32) (data : (* array<nat>*)))))))
              (data : (* array<nat>*) (arr-malloc (%type array<nat>*) size))
              (datap : (* (* array<nat>*)) (%gep ap (%uint 0 nat) (%uint 1 nat)))
              (sizep : (* i32) (%gep ap (%uint 0 nat) (%uint 0 nat))))
          (block (expr (store! size sizep)) (expr (store! data datap)) (return ap))))
      (define-function (passes)
        (attrs AlwaysInline)
        (size-array<array<nat>*>
         (array-ptr : (* (struct ((size : i32) (data : (* array<nat>*))))))
         :
         i32)
        (return (load (%gep array-ptr (%uint 0 nat) (%uint 0 nat)))))
      (define-function (passes)
        (attrs AlwaysInline)
        (index-array<array<nat>*>
         (array-ptr : (* (struct ((size : i32) (data : (* array<nat>*))))))
         (index : i32)
         :
         array<nat>*)
        (return (load (%gep (load (%gep array-ptr (%uint 0 nat) (%uint 1 nat))) index))))
      (define-function (passes)
        (attrs AlwaysInline)
        (set-index-in-array<array<nat>*>
         (array-ptr : (* (struct ((size : i32) (data : (* array<nat>*))))))
         (index : i32)
         (v : array<nat>*)
         :
         void)
        (block
         (expr (store! v (%gep (load (%gep array-ptr (%uint 0 nat) (%uint 1 nat))) index)))
         (return-void)))
      (define-function (passes)
        (attrs AlwaysInline)
        (empty-zero-array<array<nat>*> : (* (struct ((size : i32) (data : (* array<nat>*))))))
        (return (empty-array<array<nat>*> (%uint 0 nat))))



      (define-function (passes)
        (attrs AlwaysInline)
        (main
         (topic_prior : array<prob>*)
         (word_prior : array<prob>*)
         (z : array<nat>*)
         (w : array<nat>*)
         (doc : array<nat>*)
         (docUpdate : nat)
         :
         array<prob>*)
        (if (icmp-ult docUpdate (size-array<nat> z))
            (let ((ar2 : array<prob>* void))
              (block (set! ar2 (af1 w docUpdate word_prior doc z topic_prior)) (return ar2)))
            (return (empty-array<prob>))))

      (define-function (passes)
        (attrs AlwaysInline)
        (af1
         (w : array<nat>*)
         (docUpdate : nat)
         (word_prior : array<prob>*)
         (doc : array<nat>*)
         (z : array<nat>*)
         (topic_prior : array<prob>*)
         :
         array<prob>*)
        (let ((ar3 : array<prob>* void))
          (block
           (set! ar3 (empty-array<prob> (size-array<prob> topic_prior)))
           (block
            (let ((ai1 : nat (%uint 0 nat)) (end1 : nat (size-array<prob> topic_prior)))
              (while (icmp-ult ai1 end1)
                (block
                 (let ((pr9 : prob void)
                       (bk8b : array<nat>* void)
                       (sm4 : nat void)
                       (sm5 : prob void))
                   (block
                    (set! pr9 (pf2 ai1 w docUpdate word_prior doc z topic_prior))
                    (let ((arri1 : array<nat>* void))
                      (block
                       (set! arri1 (empty-array<nat> (size-array<prob> topic_prior)))
                       (block
                        (let ((fi1 : nat (%uint 0 nat))
                              (end2 : nat (size-array<prob> topic_prior)))
                          (while (icmp-ult fi1 end2)
                            (block
                             (expr (set-index-in-array<nat> arri1 fi1 (%uint 0 nat)))
                             (set! fi1 (add-nuw fi1 (%uint 1 nat))))))
                        (set! bk8b arri1))))
                    (set! sm4 (sf2 docUpdate z))
                    (set! sm5 (sf3 topic_prior))
                    (block
                     (block
                      (let ((bk8i1 : nat (%uint 0 nat)) (end3 : nat (size-array<nat> z)))
                        (while (icmp-ult bk8i1 end3)
                          (block
                           (if (icmp-eq bk8i1 docUpdate)
                               (expr void)
                               (let ((indi1 : nat void))
                                 (block
                                  (set! indi1 (index-array<nat> z bk8i1))
                                  (expr
                                   (set-index-in-array<nat>
                                    bk8b
                                    indi1
                                    (add-2-nat
                                     (index-array<nat> bk8b indi1)
                                     (%uint 1 nat)))))))
                           (set! bk8i1 (add-nuw bk8i1 (%uint 1 nat)))))))
                     (let ((pr6 : prob void))
                       (block
                        (set! pr6 (pf5 ai1 w docUpdate word_prior doc z topic_prior))
                        (expr
                         (set-index-in-array<prob>
                          ar3
                          ai1
                          (mul-4-prob
                           pr6
                           (add-2-prob
                            (nat2prob (index-array<nat> bk8b ai1))
                            (index-array<prob> topic_prior ai1))
                           (recip-prob (add-2-prob (nat2prob sm4) sm5))
                           (recip-prob pr9)))))))))
                 (set! ai1 (add-nuw ai1 (%uint 1 nat))))))
            (return ar3)))))

      (define-function (passes)
        (attrs AlwaysInline)
        (pf5
         (ai1 : nat)
         (w : array<nat>*)
         (docUpdate : nat)
         (word_prior : array<prob>*)
         (doc : array<nat>*)
         (z : array<nat>*)
         (topic_prior : array<prob>*)
         :
         prob)
        (let ((pr15 : prob void))
          (block
           (set! pr15 (real2prob (%float 1.0 real)))
           (block
            (let ((pi1 : nat (%uint 0 nat)) (end4 : nat (size-array<prob> topic_prior)))
              (while (icmp-ult pi1 end4)
                (block
                 (let ((pr7 : prob void))
                   (block
                    (set! pr7 (pf4 ai1 pi1 w docUpdate word_prior doc z topic_prior))
                    (set! pr15 (mul-2-prob pr15 pr7))))
                 (set! pi1 (add-nuw pi1 (%uint 1 nat))))))
            (return pr15)))))

      (define-function (passes)
        (attrs AlwaysInline)
        (pf4
         (ai1 : nat)
         (pi1 : nat)
         (w : array<nat>*)
         (docUpdate : nat)
         (word_prior : array<prob>*)
         (doc : array<nat>*)
         (z : array<nat>*)
         (topic_prior : array<prob>*)
         :
         prob)
        (let ((pr14 : prob void))
          (block
           (set! pr14 (real2prob (%float 1.0 real)))
           (block
            (let ((pi2 : nat (%uint 0 nat)) (end5 : nat (size-array<prob> word_prior)))
              (while (icmp-ult pi2 end5)
                (block
                 (let ((bk7a : array<array<nat>*>* void))
                   (block
                    (let ((arri4 : array<array<nat>*>* void))
                      (block
                       (set! arri4 (empty-array<array<nat>*> (size-array<nat> z)))
                       (block
                        (let ((fi4 : nat (%uint 0 nat)) (end6 : nat (size-array<nat> z)))
                          (while (icmp-ult fi4 end6)
                            (block
                             (let ((arri5 : array<nat>* void))
                               (block
                                (set! arri5
                                      (empty-array<nat> (size-array<prob> word_prior)))
                                (block
                                 (let ((fi5 : nat (%uint 0 nat))
                                       (end7 : nat (size-array<prob> word_prior)))
                                   (while (icmp-ult fi5 end7)
                                     (block
                                      (expr
                                       (set-index-in-array<nat>
                                        arri5
                                        fi5
                                        (%uint 0 nat)))
                                      (set! fi5 (add-nuw fi5 (%uint 1 nat))))))
                                 (expr
                                  (set-index-in-array<array<nat>*> arri4 fi4 arri5)))))
                             (set! fi4 (add-nuw fi4 (%uint 1 nat))))))
                        (set! bk7a arri4))))
                    (block
                     (let ((bk7i1 : nat (%uint 0 nat)) (end8 : nat (size-array<nat> w)))
                       (while (icmp-ult bk7i1 end8)
                         (block
                          (block
                           (let ((indi4 : nat void))
                             (block
                              (set! indi4 (index-array<nat> doc bk7i1))
                              (let ((indi5 : nat void))
                                (block
                                 (set! indi5 (index-array<nat> w bk7i1))
                                 (expr
                                  (set-index-in-array<nat>
                                   (index-array<array<nat>*> bk7a indi4)
                                   indi5
                                   (add-2-nat
                                    (index-array<nat>
                                     (index-array<array<nat>*> bk7a indi4)
                                     indi5)
                                    (%uint 1 nat))))))))
                           (expr void))
                          (set! bk7i1 (add-nuw bk7i1 (%uint 1 nat))))))
                     (let ((pr8 : prob void))
                       (block
                        (set! pr8
                              (pf3 ai1 pi1 w docUpdate bk7a word_prior pi2 doc z topic_prior))
                        (set! pr14 (mul-2-prob pr14 pr8)))))))
                 (set! pi2 (add-nuw pi2 (%uint 1 nat))))))
            (return pr14)))))

      (define-function (passes)
        (attrs AlwaysInline)
        (pf3
         (ai1 : nat)
         (pi1 : nat)
         (w : array<nat>*)
         (docUpdate : nat)
         (bk7a : array<array<nat>*>*)
         (word_prior : array<prob>*)
         (pi2 : nat)
         (doc : array<nat>*)
         (z : array<nat>*)
         (topic_prior : array<prob>*)
         :
         prob)
        (let ((pr13 : prob void))
          (block
           (set! pr13 (real2prob (%float 1.0 real)))
           (block
            (let ((pi3 : nat (%uint 0 nat)) (end9 : nat (ifun1 ai1 pi1 docUpdate bk7a pi2)))
              (while (icmp-ult pi3 end9)
                (block
                 (let ((bk6b : array<array<nat>*>* void))
                   (block
                    (let ((arri6 : array<array<nat>*>* void))
                      (block
                       (set! arri6 (empty-array<array<nat>*> (size-array<prob> word_prior)))
                       (block
                        (let ((fi6 : nat (%uint 0 nat))
                              (end10 : nat (size-array<prob> word_prior)))
                          (while (icmp-ult fi6 end10)
                            (block
                             (let ((arri7 : array<nat>* void))
                               (block
                                (set! arri7
                                      (empty-array<nat> (size-array<prob> topic_prior)))
                                (block
                                 (let ((fi7 : nat (%uint 0 nat))
                                       (end11 : nat (size-array<prob> topic_prior)))
                                   (while (icmp-ult fi7 end11)
                                     (block
                                      (expr
                                       (set-index-in-array<nat>
                                        arri7
                                        fi7
                                        (%uint 0 nat)))
                                      (set! fi7 (add-nuw fi7 (%uint 1 nat))))))
                                 (expr
                                  (set-index-in-array<array<nat>*> arri6 fi6 arri7)))))
                             (set! fi6 (add-nuw fi6 (%uint 1 nat))))))
                        (set! bk6b arri6))))
                    (block
                     (let ((bk6i1 : nat (%uint 0 nat)) (end12 : nat (size-array<nat> w)))
                       (while (icmp-ult bk6i1 end12)
                         (block
                          (if (icmp-eq (index-array<nat> doc bk6i1) docUpdate)
                              (expr void)
                              (let ((indi6 : nat void))
                                (block
                                 (set! indi6 (index-array<nat> w bk6i1))
                                 (let ((indi7 : nat void))
                                   (block
                                    (set! indi7
                                          (index-array<nat> z (index-array<nat> doc bk6i1)))
                                    (expr
                                     (set-index-in-array<nat>
                                      (index-array<array<nat>*> bk6b indi6)
                                      indi7
                                      (add-2-nat
                                       (index-array<nat>
                                        (index-array<array<nat>*> bk6b indi6)
                                        indi7)
                                       (%uint 1 nat)))))))))
                          (set! bk6i1 (add-nuw bk6i1 (%uint 1 nat))))))
                     (set! pr13
                           (mul-2-prob
                            pr13
                            (add-3-prob
                             (nat2prob (index-array<nat> (index-array<array<nat>*> bk6b pi2) pi1))
                             (nat2prob pi3)
                             (index-array<prob> word_prior pi2)))))))
                 (set! pi3 (add-nuw pi3 (%uint 1 nat))))))
            (return pr13)))))

      (define-function (passes)
        (attrs AlwaysInline)
        (sf3 (topic_prior : array<prob>*) : prob)
        (let ((sr3 : prob void))
          (block
           (set! sr3 (real2prob (%float 0.0 real)))
           (block
            (let ((si2 : nat (%uint 0 nat)) (end13 : nat (size-array<prob> topic_prior)))
              (while (icmp-ult si2 end13)
                (block
                 (set! sr3 (add-2-prob sr3 (index-array<prob> topic_prior si2)))
                 (set! si2 (add-nuw si2 (%uint 1 nat))))))
            (return sr3)))))

      (define-function (passes)
        (attrs AlwaysInline)
        (sf2 (docUpdate : nat) (z : array<nat>*) : nat)
        (let ((sr2 : nat void))
          (block
           (set! sr2 (%uint 0 nat))
           (block
            (let ((si1 : nat (%uint 0 nat)) (end14 : nat (size-array<nat> z)))
              (while (icmp-ult si1 end14)
                (block
                 (set! sr2 (add-2-nat sr2 (ifun3 docUpdate si1 z)))
                 (set! si1 (add-nuw si1 (%uint 1 nat))))))
            (return sr2)))))

      (define-function (passes)
        (attrs AlwaysInline)
        (pf2
         (ai1 : nat)
         (w : array<nat>*)
         (docUpdate : nat)
         (word_prior : array<prob>*)
         (doc : array<nat>*)
         (z : array<nat>*)
         (topic_prior : array<prob>*)
         :
         prob)
        (let ((pr12 : prob void))
          (block
           (set! pr12 (real2prob (%float 1.0 real)))
           (block
            (let ((pi4 : nat (%uint 0 nat)) (end15 : nat (size-array<prob> topic_prior)))
              (while (icmp-ult pi4 end15)
                (block
                 (let ((bk10ab : array<nat>* void))
                   (block
                    (let ((arri2 : array<nat>* void))
                      (block
                       (set! arri2 (empty-array<nat> (size-array<nat> z)))
                       (block
                        (let ((fi2 : nat (%uint 0 nat)) (end16 : nat (size-array<nat> z)))
                          (while (icmp-ult fi2 end16)
                            (block
                             (expr (set-index-in-array<nat> arri2 fi2 (%uint 0 nat)))
                             (set! fi2 (add-nuw fi2 (%uint 1 nat))))))
                        (set! bk10ab arri2))))
                    (block
                     (let ((bk10i1 : nat (%uint 0 nat)) (end17 : nat (size-array<nat> w)))
                       (while (icmp-ult bk10i1 end17)
                         (block
                          (block
                           (if (icmp-ult (index-array<nat> w bk10i1) (%uint 0 nat))
                               (expr void)
                               (let ((indi2 : nat void))
                                 (block
                                  (set! indi2 (index-array<nat> doc bk10i1))
                                  (expr
                                   (set-index-in-array<nat>
                                    bk10ab
                                    indi2
                                    (add-2-nat
                                     (index-array<nat> bk10ab indi2)
                                     (%uint 1 nat)))))))
                           (expr void))
                          (set! bk10i1 (add-nuw bk10i1 (%uint 1 nat))))))
                     (let ((pr10 : prob void))
                       (block
                        (set! pr10
                              (pf1 ai1 w docUpdate bk10ab word_prior doc z pi4 topic_prior))
                        (set! pr12 (mul-2-prob pr12 pr10)))))))
                 (set! pi4 (add-nuw pi4 (%uint 1 nat))))))
            (return pr12)))))

      (define-function (passes)
        (attrs AlwaysInline)
        (pf1
         (ai1 : nat)
         (w : array<nat>*)
         (docUpdate : nat)
         (bk10ab : array<nat>*)
         (word_prior : array<prob>*)
         (doc : array<nat>*)
         (z : array<nat>*)
         (pi4 : nat)
         (topic_prior : array<prob>*)
         :
         prob)
        (let ((pr11 : prob void))
          (block
           (set! pr11 (real2prob (%float 1.0 real)))
           (block
            (let ((pi5 : nat (%uint 0 nat)) (end18 : nat (ifun4 ai1 docUpdate bk10ab pi4)))
              (while (icmp-ult pi5 end18)
                (block
                 (let ((bk9bb : array<nat>* void) (sm6 : prob void))
                   (block
                    (let ((arri3 : array<nat>* void))
                      (block
                       (set! arri3 (empty-array<nat> (size-array<prob> topic_prior)))
                       (block
                        (let ((fi3 : nat (%uint 0 nat))
                              (end19 : nat (size-array<prob> topic_prior)))
                          (while (icmp-ult fi3 end19)
                            (block
                             (expr (set-index-in-array<nat> arri3 fi3 (%uint 0 nat)))
                             (set! fi3 (add-nuw fi3 (%uint 1 nat))))))
                        (set! bk9bb arri3))))
                    (set! sm6 (sf1 word_prior))
                    (block
                     (block
                      (let ((bk9i1 : nat (%uint 0 nat)) (end20 : nat (size-array<nat> w)))
                        (while (icmp-ult bk9i1 end20)
                          (block
                           (if (icmp-ult (index-array<nat> w bk9i1) (%uint 0 nat))
                               (expr void)
                               (if (icmp-eq (index-array<nat> doc bk9i1) docUpdate)
                                   (expr void)
                                   (let ((indi3 : nat void))
                                     (block
                                      (set! indi3
                                            (index-array<nat> z (index-array<nat> doc bk9i1)))
                                      (expr
                                       (set-index-in-array<nat>
                                        bk9bb
                                        indi3
                                        (add-2-nat
                                         (index-array<nat> bk9bb indi3)
                                         (%uint 1 nat))))))))
                           (set! bk9i1 (add-nuw bk9i1 (%uint 1 nat)))))))
                     (set! pr11
                           (mul-2-prob
                            pr11
                            (add-3-prob
                             (nat2prob (index-array<nat> bk9bb pi4))
                             (nat2prob pi5)
                             sm6))))))
                 (set! pi5 (add-nuw pi5 (%uint 1 nat))))))
            (return pr11)))))

      (define-function (passes)
        (attrs AlwaysInline)
        (sf1 (word_prior : array<prob>*) : prob)
        (let ((sr1 : prob void))
          (block
           (set! sr1 (real2prob (%float 0.0 real)))
           (block
            (let ((si3 : nat (%uint 0 nat)) (end21 : nat (size-array<prob> word_prior)))
              (while (icmp-ult si3 end21)
                (block
                 (set! sr1 (add-2-prob sr1 (index-array<prob> word_prior si3)))
                 (set! si3 (add-nuw si3 (%uint 1 nat))))))
            (return sr1)))))

      (define-function (passes)
        (attrs AlwaysInline)
        (ifun4 (ai1 : nat) (docUpdate : nat) (bk10ab : array<nat>*) (pi4 : nat) : nat)
        (if (icmp-eq pi4 ai1) (return (index-array<nat> bk10ab docUpdate)) (return (%uint 0 nat))))
      (define-function (passes)
        (attrs AlwaysInline)
        (ifun3 (docUpdate : nat) (si1 : nat) (z : array<nat>*) : nat)
        (if (icmp-eq si1 docUpdate) (return (%uint 0 nat)) (return (ifun2 si1 z))))
      (define-function (passes)
        (attrs AlwaysInline)
        (ifun2 (si1 : nat) (z : array<nat>*) : nat)
        (if (icmp-ult (index-array<nat> z si1) (%uint 0 nat))
            (return (%uint 0 nat))
            (return (%uint 1 nat))))
      (define-function (passes)
        (attrs AlwaysInline)
        (ifun1 (ai1 : nat) (pi1 : nat) (docUpdate : nat) (bk7a : array<array<nat>*>*) (pi2 : nat) : nat)
        (if (icmp-eq pi1 ai1)
            (return (index-array<nat> (index-array<array<nat>*> bk7a docUpdate) pi2))
            (return (%uint 0 nat)))))))

;; (printf "ast:\n")
;; (define sast (sham-ast->sexp ast))
;; (pretty-display sast)
;; (pretty-display (print-sham-ast ast))

(define mod (compile-module ast))
(error 'stop)
(jit-verify-module mod)
;; (jit-dump-module mod)
(error 'stop)
(define benv (initialize-jit mod))

;(jit-dump-module benv)
  (define (get-t t) (jit-get-racket-type (env-lookup t benv)))
  (define (get-f f) (jit-get-function f benv))
  (define t-real (get-t 'real))
  (define t-nat (get-t 'nat))
  (define t-prob (get-t 'prob))

  (define c-nat2prob (get-f 'nat2prob))
  (define c-prob2real (get-f 'prob2real))
  (define c-real2prob (get-f 'real2prob))

  (define recip-nat (get-f 'recip-nat))
  (define recip-real (get-f 'recip-real))
  (define recip-prob (get-f 'recip-prob))

  (define add-2-nat (get-f 'add-2-nat))
  (define add-2-real (get-f 'add-2-real))
  (define add-3-real (get-f 'add-3-real))
  (define add-2-prob (get-f 'add-2-prob))
  (define add-3-prob (get-f 'add-3-prob))
  
  (define mul-2-nat (get-f 'mul-2-nat))
  (define mul-2-real (get-f 'mul-2-real))
  (define mul-2-prob (get-f 'mul-2-prob))
  (define mul-4-prob (get-f 'mul-4-prob))


  ;; (define make-array-nat (get-f 'make-array-nat))
  ;; (define index-array-nat (get-f 'index-array-nat-p))
  ;; (define size-nat (get-f 'size-array-nat-p))
  ;; (define set-array-nat-at-index! (get-f 'set-array-nat-at-index))
  ;; (define empty-array-nat (get-f 'empty-array-nat))


  ;; (define make-array-array-nat (get-f 'make-array-array-nat))
  ;; (define index-array-array-nat (get-f 'index-array-array-nat-p))
  ;; (define size-array-array-nat (get-f 'size-array-array-nat-p))
  ;; (define set-array-array-nat-at-index! (get-f 'set-array-array-nat-at-index))
  ;; (define empty-array-array-nat (get-f 'empty-array-array-nat))

  ;; (define make-array-real (get-f 'make-array-real))
  ;; (define index-array-real (get-f 'index-array-real-p))
  ;; (define size-real (get-f 'size-array-real-p))
  ;; (define set-array-real-at-index! (get-f 'set-array-real-at-index))
  ;; (define empty-array-real (get-f 'empty-array-real))

  ;; (define make-array-prob (get-f 'make-array-prob))
  ;; (define index-array-prob (get-f 'index-array-prob-p))
  ;; (define size-prob (get-f 'size-array-prob-p))
  ;; (define set-array-prob-at-index! (get-f 'set-array-prob-at-index))
  ;; (define empty-array-prob (get-f 'empty-array-prob))  


(module+ test
  
  (hakaru-defines benv)
  (define main (get-f 'main))

  (define read-from-csv (compose make-c-array-nat read-vector-from-csv))
  (define topic-prior (make-c-array-prob (replicate-vector 10 1.0)))
  (define word-prior (make-c-array-prob (replicate-vector 100 1.0)))
  (define v (read-from-csv "../test/input/small-arg3.csv")) ;;size 40x0   ;; values 0-20
  (define words (read-from-csv "../test/input/small-arg4.csv")) ;;size 47049 ;; values 0-7022
  (define docs (read-from-csv "../test/input/small-arg5.csv")) ;;size 47049 ;; values 0-400
  (define docUpdate 0) ;; value 0-400
  (define result-raw (time (main topic-prior word-prior v words docs docUpdate)))
  (define result-vector
    (cblock->vector (get-array-prob result-raw) prob-type (size-array-prob result-raw)))
  (pretty-display result-vector)
  )
