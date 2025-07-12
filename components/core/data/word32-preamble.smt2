(define-fun
   load-word32
   ((m {MemSort}) (p (_ BitVec 32)))
   (_ BitVec 32)
   (select m ((_ extract 31 2) p)))
(define-fun
   store-word32
   ((m {MemSort}) (p (_ BitVec 32)) (v (_ BitVec 32)))
   {MemSort}
   (store m ((_ extract 31 2) p) v))
(define-fun
   load-word64
   ((m {MemSort}) (p (_ BitVec 32)))
   (_ BitVec 64)
   (bvor
      ((_ zero_extend 32) (load-word32 m p))
      (bvshl
         ((_ zero_extend 32) (load-word32 m (bvadd p #x00000004)))
         #x0000000000000020)))
(define-fun
   store-word64
   ((m {MemSort}) (p (_ BitVec 32)) (v (_ BitVec 64)))
   {MemSort}
   (store-word32
      (store-word32 m p ((_ extract 31 0) v))
      (bvadd p #x00000004)
      ((_ extract 63 32) v)))
(define-fun
   word8-shift
   ((p (_ BitVec 32)))
   (_ BitVec 32)
   (bvshl ((_ zero_extend 30) ((_ extract 1 0) p)) #x00000003))
(define-fun
   word8-get
   ((p (_ BitVec 32)) (x (_ BitVec 32)))
   (_ BitVec 8)
   ((_ extract 7 0) (bvlshr x (word8-shift p))))
(define-fun
   load-word8
   ((m {MemSort}) (p (_ BitVec 32)))
   (_ BitVec 8)
   (word8-get p (load-word32 m p)))
(define-fun
   word8-put
   ((p (_ BitVec 32)) (x (_ BitVec 32)) (y (_ BitVec 8)))
   (_ BitVec 32)
   (bvor
      (bvshl ((_ zero_extend 24) y) (word8-shift p))
      (bvand x (bvnot (bvshl #x000000FF (word8-shift p))))))
(define-fun
   store-word8
   ((m {MemSort}) (p (_ BitVec 32)) (v (_ BitVec 8)))
   {MemSort}
   (store-word32 m p (word8-put p (load-word32 m p) v)))
(define-fun
   mem-dom
   ((p (_ BitVec 32)) (d {MemDomSort}))
   Bool
   (not (= (select d p) #b0)))
(define-fun mem-eq ((x {MemSort}) (y {MemSort})) Bool (= x y))
(define-fun word32-eq ((x (_ BitVec 32)) (y (_ BitVec 32))) Bool (= x y))
(define-fun
   word2-xor-scramble
   ((a (_ BitVec 2))
    (x (_ BitVec 2))
    (b (_ BitVec 2))
    (c (_ BitVec 2))
    (y (_ BitVec 2))
    (d (_ BitVec 2)))
   Bool
   (bvult (bvadd (bvxor a x) b) (bvadd (bvxor c y) d)))
(declare-fun unspecified-precond () Bool)
