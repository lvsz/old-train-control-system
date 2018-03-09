#lang racket

(require "nmbs/nmbs.rkt"
         "gui/gui.rkt")

(define border 30)

(define (main (input "input.txt"))
  (read-railway input)
  (run-nmbs)
  (make-object window%
               (+ (get-railway-width) border)
               (+ (get-railway-height) border))
  (void))

(main)
