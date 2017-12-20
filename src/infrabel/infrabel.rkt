#lang racket

(require racket/tcp
         "../common/node.rkt"
         "../common/loco.rkt")

(provide infrabel-init
         infrabel-host
         infrabel-port
         get-loco
         get-detection-block
         get-switch)

(define infrabel-host "localhost")
(define infrabel-port 9099)

(define nodes (make-hash))
(define switches (make-hash))
(define detection-blocks (make-hash))
(define locos (make-hash))

(define (get-node id)
  (hash-ref nodes id))

(define (get-loco id)
  (hash-ref locos id))

(define (get-switch id)
  (hash-ref switches id))

(define (get-detection-block id)
  (hash-ref detection-blocks id))

(define (infrabel-init file)
  (let ((file (open-input-file file #:mode 'text)))
    (for ((input (in-port read file)))
      (case input
        ((N)
         (let ((id (read file))
               (x (read file))
               (y (read file)))
           (hash-set! nodes id (make-object node% id x y))))
        ((D)
         (let ((id (read file))
               (n1 (get-node (read file)))
               (n2 (get-node (read file))))
           (hash-set! detection-blocks id (make-object detection-block% id n1 n2))
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
           (hash-set! switches id (make-object switch% id s n2 n3))
           (connect-nodes! s n1)
           (connect-nodes! s n2)
           (connect-nodes! s n3)))
        ((L)
         (let ((id (read file))
               (n1 (read file))
               (n2 (read file)))
         (hash-set! locos id (make-object loco% id (cons n1 n2)))))))))

