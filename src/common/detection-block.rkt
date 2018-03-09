#lang racket

(provide detection-block%
         no-db)

(define no-db #f)

(define detection-block%
  (class object%
    (super-new)
    (init-field id nodes)

    (define/public (get-id) id)
    (define/public (get-nodes)
      nodes)

    (define active-loco #f)
    (define/public (get-active-loco)
      active-loco)

    (define status 'green)
    (define/public (green?)
      (eq? status 'green))
    (define/public (red?)
      (eq? status 'red))
    (define/public (orange?)
      (eq? status 'orange))
    (define/public (get-status)
      status)
    (define/public (set-status new-status (loco #f) (except #f))
      (set! active-loco loco)
      (set! status new-status)
      (for-each (lambda (db)
                  (send db set-status
                        (if (eq? new-status 'green) 'green 'orange)
                        loco
                        this))
                (remq except connecting)))

    (define connecting '())
    (define/public (set-connecting dbs)
      (set! connecting dbs))))

