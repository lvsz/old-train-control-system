#lang racket

(require "../common/ordered-hash.rkt"
         "../common/node.rkt"
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
         get-railway-height
         calculate-route)


(define locos (make-hash))
(define nodes (make-hash))
(define switches (make-hash))
(define tracks (mutable-set))
(define detection-blocks (make-hash))

(define (get-node id)
  (hash-ref nodes id))

(define (get-nodes)
  nodes)

(define (get-switch id)
  (hash-ref switches id))

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
                (loco (make-object loco% id (node-pair n1 n2)))
                (db (set-intersect (send n1 get-detection-blocks)
                                   (send n2 get-detection-blocks))))
           (hash-set! locos id loco)
           (if (null? db)
             (send loco set-detection-block '|-1|)
             (send loco set-detection-block (car db)))))))))

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

(define (hash-key-of-min-value h)
  (match-let-values
    (((k _) (for/fold ((min-k #f)
                       (min-v +inf.0))
                      (((k v) (in-hash h)))
              (if (< v min-v)
                (values k v)
                (values min-k min-v)))))
    k))

(define (set-min-by s <<?)
  (let ((st (in-set s)))
    (for/fold ((min-v (sequence-ref st 0)))
              ((v (sequence-tail st 1)))
      (if (<<? v min-v)
        v
        min-v))))

(define (calculate-route start end)
  (define (sharp-turn? n1 n2 n3)
    (let ((switch (send n2 get-switch)))
      (if switch
        (let ((pos1 (send switch get-current-position))
              (pos2 (send switch get-alternative-position)))
          (or (and (eq? n1 pos1) (eq? n3 pos2))
              (and (eq? n1 pos2) (eq? n3 pos1))))
        #f)))

  (define (make-path came-from current)
    (let loop ((node (hash-ref came-from current (lambda () #f)))
               (path (list current)))
      (if node
        (loop (hash-ref came-from node (lambda () #f))
              (cons node path))
        path)))
  (let ((open (mutable-seteq start))
        (closed (mutable-seteq))
        (came-from (make-hash))
        (gscores (make-hash (list (cons start 0))))
        (fscores (make-hash))
        (score-ref (lambda (s k)
                     (hash-ref s k (lambda () +inf.0)))))
    (let while ((i 0))
      (let ((current
              (set-min-by open (lambda (a b)
                                 (< (score-ref fscores a)
                                    (score-ref fscores b))))))
        (if (eq? current end)
          (make-path came-from current)
          (begin
            (set-remove! open current)
            (set-add! closed current)
            (for-each
              (lambda (track)
                (let* ((node (track-to track))
                       (tlen (track-length track))
                       (prev (and (send current get-switch)
                                  (hash-ref came-from current (lambda () #f)))))
                  (when (and (not (set-member? closed node))
                             (or (not prev)
                                 (not (sharp-turn? node current prev))))
                    (set-add! open node)
                    (let ((new-gscore (+ tlen (score-ref gscores current))))
                      (when (< new-gscore (score-ref gscores node))
                        (hash-set! came-from node current)
                        (hash-set! gscores node new-gscore)
                        (hash-set! fscores node (+ (point-distance node end)
                                                   new-gscore)))))))
              (send current get-tracks))
            (unless (or (set-empty? open) (> i 10000))
              (while (+ i 1)))))))))

;  (let ((open (make-ordered-hash (lambda (x y) (<= (cdr x) (cdr y)))
;                                 (cons start (point-distance start end))))
;        (closed (mutable-seteq))
;        (came-from (make-hash))
;        (gscores (make-hash (cons start 0)))
;        (gscore-ref (lambda (k) (hash-ref gscores k (lambda () +inf.0)))))
;  (let loop ()
;    (when (not (ordered-hash-empty? open))
;      (let* ((current (ordered-hash-min open))
;             (node (car current))
;             (fscore (cdr current)))
;        (if (eq? (car current) end)
;          (make-path came-from current)
;          (begin
;            (ordered-hash-remove-min! open)
;            (set-add! closed 
;              ((set-member? (car current) closed)
;               (sorted-set-remove! 
;          (begin
;            (sorted-set-remove-min! open)
;            (
;            (set-add! closed (car current))
;            (let while ((next (send (car current) get-adjacent)))
;              (unless (null? next)
;                (when (set-member? closed (car next))
;                  (while (cdr next)))
;                (let* ((node (car next))
;                       (gscore (hash-ref gscores node (lambda () +inf.0)))
;                       (tent-gscore (+ gscore (point-distance node end))))
;                  (sorted-set-add! open (cons node fscore)))
;                  (when (>= (+ (hash-ref gscores current)
;                               (point-distance (car current) node))
;                            (hash-ref gscores node (lambda () +inf.0)))
;                    (while (cdr next)))
;                  (



