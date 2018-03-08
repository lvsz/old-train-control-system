#lang racket

(require "nmbs/nmbs.rkt"
         "infrabel/command.rkt"
         "gui/gui.rkt")

(define border 30)
;(define infrabel-command (new infrabel-command%))

(define (main (input "setup_loop.txt"))
  (read-railway input)
  (run-nmbs)
  (make-object window%
               (+ (get-railway-width) border)
               (+ (get-railway-height) border))
  (void))

(main)

