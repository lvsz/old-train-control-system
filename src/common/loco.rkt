#lang racket

(provide loco%)

(define loco%
  (class object%
    (super-new)
    (init-field id position)

    (define speed 0)
    (define/public (get-speed)
      speed)
    (define/public (set-speed new-speed)
      (set! speed new-speed))

    (define/public (get-position)
      position)
    (define/public (set-position new-position)
      (set! position new-position))

    (define/public (change-direction)
      (let ((n1 (car position))
            (n2 (cdr position)))
        (set! position (cons n2 n1))))))
