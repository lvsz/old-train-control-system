#lang racket

(require "../common/node.rkt")
(provide read-railway
         get-node
         get-nodes
         get-switch
         get-switches
         get-locos
         get-railway-width
         get-railway-height)

(define locos (mutable-set))
(define nodes (make-hash))
(define switches (mutable-set))
(define tracks (mutable-set))
(define detection-blocks (mutable-set))

(define (get-node id)
  (hash-ref nodes id))

(define (get-nodes)
  nodes)

(define (get-switch id)
  (hash-ref nodes id))

(define (get-switches)
  switches)

(define (get-locos)
  locos)

(define min-x +inf.0)
(define max-x -inf.0)
(define min-y +inf.0)
(define max-y -inf.0)

(define (get-railway-width)
  (- max-x min-x))

(define (get-railway-height)
  (- max-y min-y))

(define (read-railway file)
  (let ((file (open-input-file file #:mode 'text)))
    (for ((input (in-port read file)))
      (case input
        ((N)
         (let ((id (read file))
               (x (read file))
               (y (read file)))
           (cond ((< x min-x) (set! min-x x))
                 ((> x max-x) (set! max-x x)))
           (cond ((< y min-y) (set! min-y y))
                 ((> y max-y) (set! max-y y)))
           (hash-set! nodes id (make-object node% id x y))))
        ((D)
         (let ((id (read file))
               (n1 (get-node (read file)))
               (n2 (get-node (read file))))
           (set-add! detection-blocks id)
           (connect-nodes! n1 n2)))
        ((T)
         (let ((n1 (get-node (read file)))
               (n2 (get-node (read file))))
           (connect-nodes! n1 n2)))
        ((S)
         (let* ((id (read file))
                (s  (get-node id))
                (n1 (get-node (read file)))
                (n2 (get-node (read file)))
                (n3 (get-node (read file))))
           (set-add! switches id)
           (connect-nodes! s n1)
           (connect-nodes! s n2)
           (connect-nodes! s n3)))
        ((L)
         (set-add! locos (read file))
         ; unused information
         (read file)
         (read file))))))

