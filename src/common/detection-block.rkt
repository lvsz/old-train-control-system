#lang racket

(provide detection-block%)

(define detection-block%
  (class object%
    (super-new)
    (init-field id track)

    (define/public (get-id) id)
    (define/public (get-track)
      track)

    (define status 'green)
    (define/public (get-status)
      status)
    (define/public (set-status new-status)
      (set! status new-status))))

