#lang racket

(require "infrabel.rkt")

(define infrabel-listener (tcp-listen infrabel-port))
(define-values (inp outp) (tcp-accept infrabel-listener))


(define (run-server (input "../input.txt"))
  (infrabel-init input)
  (let loop ((msg (begin (displayln "server activated") (read inp))))
    (when (eof-object? msg)
      (close-input-port inp)
      (close-output-port outp)
      (tcp-close infrabel-listener)
      (displayln "Infrabel quit successfully.")
      (exit))
    (display "Infrabel request received: ")
    (displayln msg)
    (writeln
      (case msg
        ((get-loco-speed)
         (send (get-loco (read inp)) get-speed))
        ((set-loco-speed)
         (send (get-loco (read inp)) set-speed (read inp)))
        ((get-loco-position)
         (send (get-loco (read inp)) get-position))
        ((change-loco-position)
         (send (get-loco (read inp)) set-position (read inp)))
        ((change-loco-direction)
         (send (get-loco (read inp)) change-direction))
        ((get-detection-block-status)
         (send (get-detection-block (read inp)) get-status))
        ((get-current-switch-position)
         (send (get-switch (read inp)) get-current-position))
        ((get-alternative-switch-position)
         (send (get-switch (read inp)) get-alternative-position))
        ((change-switch-position)
         (send (get-switch (read inp)) change-position)))
      outp)
    (flush-output outp)
    (loop (read inp))))

(run-server)
