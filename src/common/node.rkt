#lang racket/gui

(provide node%
         switch%
         node-pair
         fst-node
         snd-node
         switch-nodes
         track?
         track-from
         track-to
         track-length
         point-distance
         nodes-slope
         connect-nodes!
         next-node)

(struct track (from to length))

(define node-pair cons)
(define fst-node car)
(define snd-node cdr)
(define (switch-nodes pair)
  (let ((n1 (fst-node pair))
        (n2 (snd-node pair)))
    (node-pair n2 n1)))

(define node%
  (class point%
    (super-new)
    (init-field id x y)
    (send* this (set-x x)
                (set-y y))

    (define/public (get-id) id)

    (define adjacent '())
    (define/public (get-adjacent)
      (map track-to adjacent))
    (define/public (add-adjacent node distance)
      (set! adjacent (cons (track this node distance) adjacent)))
    (define/public (get-tracks)
      adjacent)
    (define/public (get-track-to node)
      (let loop ((tracks adjacent))
        (cond ((null? tracks)
               #f)
              ((eq? node (track-to (car tracks)))
               (car tracks))
              (else (loop (cdr tracks))))))
    (define/public (get-track-length node)
      (let ((t (get-track-to node)))
        (if t
          (track-length t)
          #f)))

    (define switch #f)
    (define/public (get-switch)
      switch)
    (define/public (set-switch id)
      (set! switch id))

    (define detection-blocks '())
    (define/public (detection-block?)
      (pair? detection-blocks))
    (define/public (get-detection-blocks)
      detection-blocks)
    (define/public (add-detection-block id)
      (set! detection-blocks (cons id detection-blocks))
      (when (pair? (cdr detection-blocks))
        (for-each (lambda (db)
                    (send db set-connecting (remq db detection-blocks)))
        detection-blocks)))))

(define switch%
  (class object%
    (super-new)
    (init-field id entry midpoint exit-1 exit-2)

    (define/public (get-id) id)

    (define/public (get-entry-node)
      entry)
    (define current-position exit-1)
    (define/public (get-current-position)
      current-position)
    (define/public (get-alternative-position)
      (if (eq? current-position exit-1)
        exit-2
        exit-1))
    (define/public (change-position)
      (if (eq? current-position exit-1)
        (set! current-position exit-2)
        (set! current-position exit-1)))))

;    (define (replace-track tracks to new-track)
;      (map (lambda (t) (if (eq? (track-to t) to) new-track t)) tracks))
;    (when (> 0 10)
;    (let* ((tlen-e (send midpoint get-track-length entry))
;           (tlen-1 (send midpoint get-track-length exit-1))
;           (tlen-2 (send midpoint get-track-length exit-2))
;           (tlen-e1 (+ tlen-e tlen-1))
;           (tlen-e2 (+ tlen-e tlen-2)))
;      (send entry set-tracks
;            (cons (track entry exit-1 tlen-e1)
;                  (replace-track (send entry get-tracks)
;                                 exit-2
;                                 (track entry exit-2 tlen-e2))))
;      (send exit-1 set-tracks
;            (replace-track (send exit-1 get-tracks)
;                           entry
;                           (track exit-1 entry tlen-e1)))
;      (send exit-2 set-tracks
;            (replace-track (send exit-2 get-tracks)
;                           entry
;                           (track exit-2 entry tlen-e2)))))

(define (point-distance n1 n2)
  (let ((x1 (send n1 get-x))
        (y1 (send n1 get-y))
        (x2 (send n2 get-x))
        (y2 (send n2 get-y)))
    (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))))


(define (connect-nodes! n1 n2)
  (let ((distance (point-distance n1 n2)))
    (send n1 add-adjacent n2 distance)
    (send n2 add-adjacent n1 distance)))


(define (nodes-slope n1 n2)
  (let ((x1 (send n1 get-x))
        (y1 (send n1 get-y))
        (x2 (send n2 get-x))
        (y2 (send n2 get-y)))
    (cond ((not (= x1 x2))
            (/ (- y1 y2) (- x2 x1)))
          ((< y1 y2)
            +inf.0)
          (else
            -inf.0))))

(define (next-node from to)
  (let ((adjacent (remq from (send to get-adjacent))))
    (cond ((null? adjacent)
           #f)
          ((null? (cdr adjacent))
           (car adjacent))
          (else (let ((switch (send to get-switch)))
                  (cond ((eq? (send switch get-entry-node) from)
                         (send switch get-current-position))
                        ((eq? (send switch get-current-position) from)
                         (send switch get-entry-node))
                        (else #f)))))))


