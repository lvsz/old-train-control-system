#lang racket/gui

(provide node%
         switch%
         node-pair
         fst-node
         snd-node
         switch-nodes
         distance-between-nodes
         nodes-slope
         connect-nodes!)

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
    (init-field id)

    (define/public (get-id) id)

    (define adjacent '())
    (define/public (get-adjacent)
      (map car adjacent))
    (define/public (add-adjacent node distance)
      (set! adjacent (cons (cons node distance) adjacent)))
    (define/public (get-distance node)
      (let ((distance (assoc node adjacent)))
        (if distance
          (cdr distance)
          #f)))

    (define switch #f)
    (define/public (switch?)
      switch)
    (define/public (set-switch id)
      (set! switch id))

    (define detection-blocks '())
    (define/public (detection-block?)
      (not (null? detection-blocks)))
    (define/public (get-detection-blocks)
      detection-blocks)
    (define/public (add-detection-block id)
      (set! detection-blocks (cons id detection-blocks)))))


(define switch%
  (class object%
    (super-make-object)
    (init-field id switch-node route-1 route-2)

    (define/public (get-id) id)

    (define current-position route-1)
    (define/public (get-current-position)
      (send current-position get-id))
    (define/public (get-alternative-position)
      (if (eq? current-position route-1)
        (send route-2 get-id)
        (send route-1 get-id)))
    (define/public (change-position)
      (if (eq? current-position route-1)
        (set! current-position route-2)
        (set! current-position route-1)))))

; (define switch%
;   (class object%
;     (super-make-object)
;     (init-field id switch-node route-1 route-2)
;
;     (define/public (get-id) id)
;
;     (define positions (vector route-1 route-2))
;     (define current 0)
;     (define/public (get-current-position)
;       (send (vector-ref positions current) get-id))
;     (define/public (get-alternative-position)
;       (send (vector-ref positions (bitwise-and (+ current 1) 1)) get-id))
;     (define/public (change-position)
;       (set! current (bitwise-and (+ current 1) 1)))))


(define (distance-between-nodes n1 n2)
  (let ((x1 (send n1 get-x))
        (y1 (send n1 get-y))
        (x2 (send n2 get-x))
        (y2 (send n2 get-y)))
    (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))))


(define (connect-nodes! n1 n2)
  (let ((distance (distance-between-nodes n1 n2)))
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

