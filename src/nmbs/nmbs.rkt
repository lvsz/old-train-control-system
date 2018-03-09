#lang racket

(require "../common/ordered-hash.rkt"
         "../common/node.rkt"
         "../common/loco.rkt"
         "../common/detection-block.rkt"
         (prefix-in remote: "../infrabel/client.rkt"))

(provide read-railway
         run-nmbs
         get-node
         get-nodes
         get-switch
         get-switches
         get-loco
         get-locos
         get-detection-block
         get-detection-blocks
         get-railway-width
         get-railway-height
         get-loco-speed
         set-loco-speed!
         get-loco-track
         get-detection-block-status
         get-current-switch-position
         get-alternative-switch-position
         change-switch-position!
         calculate-route
         go-to)


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
  (hash-ref detection-blocks id (lambda () no-db)))

(define (get-detection-blocks)
  detection-blocks)

(define (get-loco-speed id)
  (send (get-loco id) get-speed))

(define (set-loco-speed! id speed)
  (remote:set-loco-speed! id speed)
  (send (get-loco id) set-speed speed))

(define (get-loco-track id)
  (send (get-loco id) get-current-track))

(define (get-detection-block-status id)
  (send (get-detection-block id) get-status))

(define (get-current-switch-position id)
  (send (get-switch id) get-current-position))

(define (get-alternative-switch-position id)
  (send (get-switch id) get-alternative-position))

(define (change-switch-position! id)
  (remote:change-switch-position! id)
  (send (get-switch id) change-position))

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
                    (send (car db) set-status 'red loco)))))))))

(define (run-nmbs)
  (define (nmbs-thread)
    (thread
      (lambda ()
        (let ((evt (thread-receive)))
          (case (car evt)
            ((db-status)
             (send (get-detection-block (list-ref evt 1))
                   set-status
                   (list-ref evt 2)
                   (if (list-ref evt 3)
                     (get-loco (list-ref evt 3))
                     #f)))
            ((loco-db)
             (let ((lid  (list-ref evt 1))
                   (dbid (list-ref evt 2)))
               (send (get-loco lid) set-detection-block dbid)
               (when dbid
                 (let* ((nodes (send (get-detection-block dbid)
                                     get-nodes))
                        (track (send (fst-node nodes)
                                     get-track-to
                                     (snd-node nodes))))
                   (send (get-loco lid) set-track track))))))))))
  (thread (lambda ()
            (let loop ()
              (remote:listen (nmbs-thread))
              (loop)))))

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

(define (clear-path? loco p)
  (let loop ((path p))
    (cond ((null? path)
           #t)
          ((and (send (car path) detection-block?)
                (not (andmap (lambda (db)
                               (eq? (send db get-status) 'green))
                             (send (car path) get-detection-blocks)))
                (not (ormap (lambda (db)
                              (eq? loco (send db get-active-loco)))
                            (send (car path) get-detection-blocks))))
           #f)
          (else (loop (cdr path))))))

(define (switch-switches p)
  (let loop ((prev (car p)) (path (cdr p)))
    (unless (or (null? path) (null? (cdr path)))
      (let ((switch (send (car path) get-switch)))
        (when switch
          (let ((alternative (send switch get-alternative-position)))
            (when (or (eq? (cadr path) alternative)
                      (eq? prev alternative))
              (change-switch-position! (send switch get-id)))))
        (loop (car path) (cdr path))))))

(define (wait-till-next-detection-block loco p)
  (let* ((path (memf (lambda (x) (send x detection-block?)) p))
         (dest (last p))
         (db (when path (car (send (car path) get-detection-blocks)))))
    (when path
      (let loop ((there? (eq? (send db get-active-loco) loco)))
        (sleep 0.5)
        (if there?
          path
          (loop (eq? (send db get-active-loco) loco)))))))

(define (go-to loco dest)
  (let* ((lid (send loco get-id))
         (current-track (send loco get-current-track))
         (front-node (track-to current-track))
         (back-node  (track-from current-track))
         (route (memf (lambda (x)
                        (not (or (eq? x front-node)
                                 (eq? x back-node))))
                      (calculate-route front-node dest)))
         (i 100)
         (forward? (>= 0 (get-loco-speed lid))))
    (let-values
      (((from to) (if forward?
                          (values back-node front-node)
                          (values front-node back-node))))
      (displayln (map (lambda (x) (send x get-id)) route))
      (thread
        (lambda ()
          (let loop ((from from) (to to) (nodes route))
            (let* ((next (next-node from to))
                   (switch (send to get-switch))
                   (_db_ (if next
                           (set-intersect (send next get-detection-blocks)
                                          (send to get-detection-blocks))
                           null))
                   (db (if (null? _db_) #f (car _db_))))
                      ;(displayln (send from get-id))
                      ;(displayln(send to get-id))
                      ;(if next (displayln(send next get-id)) (displayln 'no-next))
                      ;(displayln(map (lambda (x) (send x get-id)) nodes))
                      ;(newline)
                      (cond ((null? nodes)
                      (set-loco-speed! lid 0)
                      (displayln "reached destination"))
                    ((and (eq? next (car nodes))
                          (not db))
                      (set-loco-speed! lid (if forward? 0.7 -0.7))
                      (send loco move)
                      (loop to (car nodes) (cdr nodes)))
                    ((and (eq? next (car nodes))
                          (not (eq? (send db get-active-loco) loco))
                          (not (send db green?)))
                      (set-loco-speed! lid 0)
                      (sleep 0.1)
                      (loop from to nodes))
                    ((and (eq? next (car nodes))
                          (send db green?))
                      (set-loco-speed! lid (if forward? 0.7 -0.7))
                      (sleep 0.1)
                      (loop from to nodes))
                    ((eq? next (car nodes))
                      (send loco move)
                      (loop to (car nodes) (cdr nodes)))
                    ((and (> i 0) switch)
                     (set! i (sub1 i))
                      (change-switch-position! (send switch get-id))
                      (loop from to nodes))
                    ((> i 0)
                      (set! i (- i 1))
                      (set! forward? (not forward?))
                      (set-loco-speed! lid (- (get-loco-speed lid)))
                      (loop to from nodes))))))))))
