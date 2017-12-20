#lang racket/gui

(provide node%
         switch%
         detection-block%
         distance-between-nodes
         nodes-slope
         connect-nodes!)

(define node%
  (class point%
    (super-new)
    (init-field id)

    (define/public (get-id) id)

    (define connected '())
    (define/public (get-connected)
      (map car connected))
    (define/public (add-connected node distance)
      (set! connected (cons (cons node distance) connected)))

    (define switch #f)
    (define/public (switch?)
      switch)
    (define/public (set-switch id)
      (set! switch id))

    (define detection-block #f)
    (define/public (detection-block?)
      detection-block)
    (define/public (set-detection-block id)
      (set! detection-block id))))


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


(define detection-block%
  (class object%
    (super-make-object)
    (init-field id node-1 node-2)

    (define/public (get-id) id)

    (define status 'green)
    (define/public (get-status)
      status)
    (define/public (set-status new-status)
      (set! status new-status))))


(define (distance-between-nodes n1 n2)
  (let ((x1 (send n1 get-x))
        (y1 (send n1 get-y))
        (x2 (send n2 get-x))
        (y2 (send n2 get-y)))
    (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))))


(define (connect-nodes! n1 n2)
  (let ((distance (distance-between-nodes n1 n2)))
    (send n1 add-connected n2 distance)
    (send n2 add-connected n1 distance)))


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

