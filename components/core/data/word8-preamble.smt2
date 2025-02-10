(define-fun
   load-word32
   ((m {MemSort}) (p (_ BitVec 32)))
   (_ BitVec 32)
   (concat
      (concat (select m (bvadd p #x00000003)) (select m (bvadd p #x00000002)))
      (concat (select m (bvadd p #x00000001)) (select m p))))
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
   store-word32
   ((m {MemSort}) (p (_ BitVec 32)) (v (_ BitVec 32)))
   {MemSort}
   (store
      (store
         (store
            (store m p ((_ extract 7 0) v))
            (bvadd p #x00000001)
            ((_ extract 15 8) v))
         (bvadd p #x00000002)
         ((_ extract 23 16) v))
      (bvadd p #x00000003)
      ((_ extract 31 24) v)))
(define-fun
   store-word64
   ((m {MemSort}) (p (_ BitVec 32)) (v (_ BitVec 64)))
   {MemSort}
   (store-word32
      (store-word32 m p ((_ extract 31 0) v))
      (bvadd p #x00000004)
      ((_ extract 63 32) v)))
(define-fun
   load-word8
   ((m {MemSort}) (p (_ BitVec 32)))
   (_ BitVec 8)
   (select m p))
(define-fun
   store-word8
   ((m {MemSort}) (p (_ BitVec 32)) (v (_ BitVec 8)))
   {MemSort}
   (store m p v))
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
