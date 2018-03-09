#lang racket

(provide loco%)

(require "node.rkt")

(define loco%
  (class object%
    (super-new)
    (init-field id current-position)

    (define/public (get-id)
      id)

    (define speed 0)
    (define/public (get-speed)
      speed)
    (define/public (set-speed new-speed)
      (set! speed new-speed))

    (define detection-block '|-1|)
    (define/public (get-detection-block)
      detection-block)
    (define/public (set-detection-block db-id)
      (set! detection-block db-id))

    (define/public (get-position)
      current-position)
    (define/public (set-position new-position)
      (set! current-position new-position))

    ;(define/public (change-direction)
    ;  (set! current-position (switch-nodes current-position)))
    ))

