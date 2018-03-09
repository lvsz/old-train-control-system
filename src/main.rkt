#lang racket

(require "nmbs/nmbs.rkt"
         "gui/gui.rkt")

(define border 30)
;(map (lambda (x) (send x get-id)) (calculate-route (get-node '|6|) (get-node '|26|)))

(define (main (input "setup_loop.txt"))
  (read-railway input)
  (run-nmbs)
  (make-object window%
               (+ (get-railway-width) border)
               (+ (get-railway-height) border))
  (void))

(main)

