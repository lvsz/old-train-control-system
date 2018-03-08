#lang racket

(require "../common/node.rkt"
         "../common/loco.rkt"
         "../common/detection-block.rkt"
         "../infrabel/command.rkt")

(provide read-railway
         run-nmbs
         get-node
         get-nodes
         get-switch
         get-switches
         get-loco
         get-locos
         get-detection-blocks
         get-railway-width
         get-railway-height)

(define (run-nmbs)
  (define (nmbs-thread)
    (thread
      (lambda ()
        (let ((evt (thread-receive)))
          (case (car evt)
            ((db-status)
             (send (get-detection-block (list-ref evt 1))
                   set-status
                   (list-ref evt 2)))
            ((loco-db)
             (let ((lid  (list-ref evt 1))
                   (dbid (list-ref evt 2)))
               (send (get-loco lid) set-detection-block dbid)
               (when dbid
                 (let ((track (send (get-detection-block dbid)
                                    get-track)))
                   (send (get-loco lid) set-position track))))))))))
  (thread (lambda ()
            (let loop ()
              (listen (nmbs-thread))
              (loop)))))



(define locos (make-hash))
(define nodes (make-hash))
(define switches (mutable-set))
(define tracks (mutable-set))
(define detection-blocks (make-hash))

(define (get-node id)
  (hash-ref nodes id))

(define (get-nodes)
  nodes)

(define (get-switch id)
  (hash-ref nodes id))

(define (get-switches)
  switches)

(define (get-loco id)
  (hash-ref locos id))

(define (get-locos)
  locos)

(define (get-detection-block id)
  (hash-ref detection-blocks id))

(define (get-detection-blocks)
  detection-blocks)

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
    (for ((input (sequence-map string-split (in-lines file))))
      (case (string->symbol (car input))
        ((N)
         (let ((id (string->symbol (list-ref input 1)))
               (x (string->number (list-ref input 2)))
               (y (string->number (list-ref input 3))))
           (cond ((< x min-x) (set! min-x x))
                 ((> x max-x) (set! max-x x)))
           (cond ((< y min-y) (set! min-y y))
                 ((> y max-y) (set! max-y y)))
           (hash-set! nodes id (make-object node% id x y))))
        ((D)
         (let ((id (string->symbol (list-ref input 1)))
               (n1 (get-node (string->symbol (list-ref input 2))))
               (n2 (get-node (string->symbol (list-ref input 3)))))
           (hash-set! detection-blocks id
                      (make-object detection-block% id (node-pair n1 n2)))
           (send n1 add-detection-block id)
           (send n2 add-detection-block id)
           (connect-nodes! n1 n2)))
        ((T)
         (let ((n1 (get-node (string->symbol (list-ref input 1))))
               (n2 (get-node (string->symbol (list-ref input 2)))))
           (connect-nodes! n1 n2)))
        ((S)
         (let* ((id (string->symbol (list-ref input 1)))
                (s  (get-node id))
                (n1 (get-node (string->symbol (list-ref input 2))))
                (n2 (get-node (string->symbol (list-ref input 3))))
                (n3 (get-node (string->symbol (list-ref input 4)))))
           (set-add! switches id)
           (connect-nodes! s n1)
           (connect-nodes! s n2)
           (connect-nodes! s n3)))
        ((L)
         (let* ((id (string->symbol (list-ref input 1)))
                (n1 (get-node (string->symbol (list-ref input 2))))
                (n2 (get-node (string->symbol (list-ref input 3))))
                (loco (make-object loco% id (node-pair n1 n2)))
                (db (set-intersect (send n1 get-detection-blocks)
                                   (send n2 get-detection-blocks))))
           (hash-set! locos id loco)
           (if (null? db)
             (send loco set-detection-block '|-1|)
             (send loco set-detection-block (car db)))))))))

