#lang racket

(provide loco%)

(require "node.rkt"
         "detection-block.rkt")

(define loco%
  (class object%
    (super-new)
    (init-field id current-track)

    (define/public (get-id)
      id)

    (define speed 0)
    (define/public (get-speed)
      speed)
    (define/public (set-speed new-speed)
      (set! speed new-speed))

    (define detection-block no-db)
    (define/public (get-detection-block)
      detection-block)
    (define/public (set-detection-block db-id)
      (set! detection-block db-id))

    (define/public (get-current-track)
      current-track)
    (define/public (move)
      (let* ((from (track-from current-track))
             (to   (track-to current-track))
             (next (if (< speed 0)
                     (next-node to from)
                     (next-node from to))))
        (when next
          (set! current-track
            (send (if (< speed 0) from to) get-track-to next)))))
    (define/public (set-track new-track)
      (set! current-track new-track))))

