#lang racket

(require "nmbs/nmbs.rkt"
         "infrabel/command.rkt"
         "gui/gui.rkt")

(define border 30)
(define infrabel-command (new infrabel-command%))

(define (main (input "input.txt"))
  (read-railway input)
  (window-init
    (+ (get-railway-width) border)
    (+ (get-railway-height) border)
    infrabel-command)
  (draw-window))

(main)

