#lang racket

(require racket/tcp
         "../common/node.rkt"
         "../common/loco.rkt"
         "../common/detection-block.rkt"
         (prefix-in z21: "../simulator/interface.rkt"))

(provide run
         infrabel-init
         infrabel-host
         infrabel-port
         get-loco
         get-loco-speed
         set-loco-speed!
         get-loco-position
         get-loco-detection-block
         get-switch
         get-switch-position
         set-switch-position!
         change-switch-position!
         get-detection-block)

(define infrabel-host "localhost")
(define infrabel-port 10133)

(define nodes (make-hash))
(define switches (make-hash))
(define detection-blocks (make-hash))
(define locos (make-hash))

(define no-db #f) ;'|-1|)

(define (get-node id)
  (hash-ref nodes id))

(define (get-switch id)
  (hash-ref switches id))

(define (get-loco id)
  (hash-ref locos id))

(define (get-loco-position id)
  (send (hash-ref locos id) get-position))

(define (get-loco-speed id)
  (z21:get-loco-speed id))

(define (set-loco-speed! id speed)
  (printf "setting speed to ~a~%" speed)
  (z21:set-loco-speed! id speed))

(define (get-loco-detection-block id)
  (z21:get-loco-detection-block))

(define (get-switch-position id)
  ;(z21:get-switch-position id))
  (send (get-switch id) get-current-position))

(define (set-switch-position! id pos)
  (z21:set-switch-position! id pos))

(define (change-switch-position! id)
  (if (= (z21:get-switch-position id) 1)
    (z21:set-switch-position! id 2)
    (z21:set-switch-position! id 1))
  (send (get-switch id) change-position))

(define (get-detection-block id)
  (hash-ref detection-blocks id))

(define (infrabel-init file)
  (let ((file (open-input-file file #:mode 'text)))
    (for ((input (sequence-map string-split (in-lines file))))
      (case (string->symbol (car input))
        ((N)
         (let ((id (string->symbol (list-ref input 1)))
               (x (string->number (list-ref input 2)))
               (y (string->number (list-ref input 3))))
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
           (hash-set! switches id (make-object switch% id s n2 n3))
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
             (send loco set-detection-block (car db))))))))
  (z21:start-simulator file))

(define (run push-evt)
  (hash-for-each
    locos
    (lambda (lid loco)
      (let ((db1 (z21:get-loco-detection-block lid))
            (db2 (send loco get-detection-block)))
        (unless (eq? db1 db2)
          (displayln 'pushed-stufff)
          (unless (eq? db1 no-db)
            (send (get-detection-block db1) set-status 'red)
            (push-evt 'db-status db1 'red))
          (unless (eq? db2 no-db)
            (send (get-detection-block db2) set-status 'green)
            (push-evt 'db-status db2 'green))
          (send loco set-detection-block db1)
          (push-evt 'loco-db lid db1)))))
  (sleep 0.5)
  (run push-evt))



