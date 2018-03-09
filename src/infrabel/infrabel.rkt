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
         get-loco-track
         get-loco-detection-block
         get-switch
         get-current-switch-position
         get-alternative-switch-position
         set-switch-position!
         change-switch-position!
         get-detection-block)

(define infrabel-host "localhost")
(define infrabel-port 10171)

(define nodes (make-hash))
(define switches (make-hash))
(define detection-blocks (make-hash))
(define locos (make-hash))

(define (get-node id)
  (hash-ref nodes id))

(define (get-switch id)
  (hash-ref switches id))

(define (get-loco id)
  (hash-ref locos id))

(define (get-loco-track id)
  (send (hash-ref locos id) get-current-track))

(define (get-loco-speed id)
  (z21:get-loco-speed id))

(define (set-loco-speed! id speed)
  (z21:set-loco-speed! id speed))

(define (get-loco-detection-block id)
  (z21:get-loco-detection-block))

(define (get-current-switch-position id)
  ;(z21:get-switch-position id))
  (send (get-switch id) get-current-position))

(define (get-alternative-switch-position id)
  (send (get-switch id) get-alternative-position))

(define (set-switch-position! id pos)
  (let* ((node (get-node pos))
         (switch (get-switch id))
         (pos1 (send switch get-current-position))
         (pos2 (send switch get-alternative-position)))
    (unless (eq? node pos1)
      (if (eq? node pos2)
        (change-switch-position! id)
        (raise-arguments-error 'set-switch-position
                               (format "invalid position for node ~a" id)
                               "given" pos
                               "expected" `(or ,(send pos1 get-id)
                                               ,(send pos2 get-id)))))))

(define (change-switch-position! id)
  (if (= (z21:get-switch-position id) 1)
    (z21:set-switch-position! id 2)
    (z21:set-switch-position! id 1))
  (send (get-switch id) change-position))

(define (get-detection-block id)
  (hash-ref detection-blocks id (lambda () no-db)))

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
         (let* ((id (string->symbol (list-ref input 1)))
                (n1 (get-node (string->symbol (list-ref input 2))))
                (n2 (get-node (string->symbol (list-ref input 3))))
                (detection-block
                  (make-object detection-block% id (node-pair n1 n2))))
           (hash-set! detection-blocks id detection-block)
           (send n1 add-detection-block detection-block)
           (send n2 add-detection-block detection-block)
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
                (n3 (get-node (string->symbol (list-ref input 4))))
                (switch (make-object switch% id n1 s n2 n3)))
           (hash-set! switches id switch)
           (send s set-switch switch)
           (connect-nodes! s n1)
           (connect-nodes! s n2)
           (connect-nodes! s n3)))
        ((L)
         (let* ((id (string->symbol (list-ref input 1)))
                (n1 (get-node (string->symbol (list-ref input 2))))
                (n2 (get-node (string->symbol (list-ref input 3))))
                (loco (make-object loco% id (send n1 get-track-to n2)))
                (db (set-intersect (send n1 get-detection-blocks)
                                   (send n2 get-detection-blocks))))
           (hash-set! locos id loco)
           (if (null? db)
             (send loco set-detection-block no-db)
             (begin (send loco set-detection-block (car db))
                    (send (car db) set-status 'red loco))))))))
  (z21:start-simulator file))

(define (run push-evt)
  (hash-for-each
    locos
    (lambda (lid loco)
      (let* ((dbid1 (z21:get-loco-detection-block lid))
             (db1 (get-detection-block dbid1))
             (db2 (send loco get-detection-block))
             (dbid2 (if (eq? db2 no-db) no-db (send db2 get-id))))
        (when (and (not (eq? db2 no-db))
                   (not (eq? (send db2 get-status) 'red)))
          (send db2 set-status 'red loco))
        (unless (eq? dbid1 dbid2)
          (unless (eq? dbid1 no-db)
            (send db1 set-status 'red)
            (push-evt 'db-status dbid1 'red lid))
          (unless (or (eq? dbid2 no-db) (not (eq? dbid1 no-db)))
            (send db2 set-status 'green)
            (push-evt 'db-status dbid2 'green #f))
          (send loco set-detection-block db1)
          (push-evt 'loco-db lid dbid1)))))
  (sleep 0.1)
  (run push-evt))

