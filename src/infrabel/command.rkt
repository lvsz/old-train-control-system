#lang racket

(require "infrabel.rkt")

(provide infrabel-command%)

(define infrabel-command%
  (class object%
    (super-new)

    (define-values (in out) (tcp-connect infrabel-host infrabel-port))

    (define (send-msg . msgs)
      (for-each (lambda (msg) (writeln msg out)) msgs)
      (flush-output out))

    (define (send-and-receive-msg . msgs)
      (apply send-msg msgs)
      (read in))

    (define/public (get-loco-speed id)
      (send-and-receive-msg 'get-loco-speed id))

    (define/public (set-loco-speed id speed)
      (send-msg 'set-loco-speed id speed))

    (define/public (get-loco-position id)
      (send-and-receive-msg 'get-loco-position id))

    (define/public (change-loco-position id position)
      (send-msg 'change-loco-position id position))

    (define/public (change-loco-direction id)
      (send-msg 'change-loco-direction id))

    (define/public (get-detection-block-status id)
      (send-and-receive-msg 'get-detection-block-status id))

    (define/public (get-current-switch-position id)
      (send-and-receive-msg 'get-current-switch-position id))

    (define/public (get-alternative-switch-position id)
      (send-and-receive-msg 'get-alternative-switch-position id))

    (define/public (change-switch-position id)
      (send-msg 'change-switch-position id))

    (define/public (exit)
      (close-input-port in)
      (close-output-port out))))

