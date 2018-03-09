#lang racket

(require (prefix-in infra: "infrabel.rkt"))

(provide get-loco-speed
         set-loco-speed!
         get-loco-position
         get-detection-block-status
         get-current-switch-position
         get-alternative-switch-position
         change-switch-position!
         listen)


(define-values (in out)
  (tcp-connect infra:infrabel-host infra:infrabel-port))

(define-values (evt-in evt-out)
  (tcp-connect infra:infrabel-host (+ infra:infrabel-port 1)))

(define (listen thd)
  (let ((evt (read evt-in)))
    (thread-send thd evt)))

(define (send-msg . msgs)
  (for-each (lambda (msg) (writeln msg out)) msgs)
  (flush-output out))

(define (send-and-receive-msg . msgs)
  (apply send-msg msgs)
  (read in))

(define (get-loco-speed id)
  (send-and-receive-msg 'get-loco-speed id))

(define (set-loco-speed! id speed)
  (send-msg 'set-loco-speed id speed))

(define (get-loco-position id)
  (send-and-receive-msg 'get-loco-position id))

; (define (change-loco-position id position)
; (send-msg 'change-loco-position id position))

(define (change-loco-direction! id)
  (send-msg 'change-loco-direction id))

(define (get-detection-block-status id)
  (send-and-receive-msg 'get-detection-block-status id))

(define (get-current-switch-position id)
  ;(printf "getting switch position for ~s~%" id)
  (send-and-receive-msg 'get-current-switch-position id))

(define (get-alternative-switch-position id)
  ;(printf "getting alt switch position for ~s~%" id)
  (send-and-receive-msg 'get-alternative-switch-position id))

(define (change-switch-position! id)
  (send-msg 'change-switch-position id))

(define (exit)
  (close-input-port in)
  (close-output-port out)
  (close-input-port evt-in)
  (close-output-port evt-out))

