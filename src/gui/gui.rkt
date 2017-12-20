#lang racket/gui

(require "../nmbs/nmbs.rkt"
         "../common/node.rkt")

(provide window-init
         draw-window)


(define frame #f)


(define (control-panel infrabel)
  (let* ((panel (new horizontal-panel%
                     (parent frame)
                     (border 0)))
         (buttons (for/list ((id (in-set (get-switches))))
                    (new button%
                         (label (~a id))
                         (parent panel)
                         (horiz-margin 0)
                         (font
                           (send the-font-list
                                 find-or-create-font
                                 10
                                 'swiss
                                 'normal
                                 'light))
                         (callback
                           (lambda (b e)
                             (send infrabel change-switch-position id)))))))
    panel))


(define (railway-bmp)
  (let* ((bmp (make-bitmap (+ 15 (get-railway-width)) (+ 15 (get-railway-height))))
         (dc (new bitmap-dc% (bitmap bmp)))
         (font (make-object font% 10 'swiss))
         (node-radius 2)
         (rail-layer '())
         (node-layer '())
         (visited (mutable-set)))
    (define (draw-layer layer)
      (for-each (lambda (x) (x)) layer))
    (send dc set-smoothing 'smoothed)
    (send dc set-font font)
    (send dc set-text-foreground "blue")
    (for (((id n) (in-hash (get-nodes))))
      (set-add! visited id)
      (set! node-layer (cons (lambda ()
                               (send dc draw-ellipse
                                     (- (send n get-x) node-radius)
                                     (- (send n get-y) node-radius)
                                     (* 2 node-radius)
                                     (* 2 node-radius)))
                             node-layer))
      (for ((m (in-list (send n get-connected))))
        (unless (set-member? visited m)
            (set! rail-layer (cons (lambda ()
                                      (send dc draw-lines (list m n)))
                                    rail-layer)))))
    (send dc set-pen (send the-pen-list find-or-create-pen "black" 3 'solid))
    (send dc set-brush (send the-brush-list find-or-create-brush "black" 'solid))
    (draw-layer rail-layer)
    (draw-layer node-layer)
    bmp))


(define (draw-arrow dc x y rotation color)
  (let* ((p (new dc-path%)))
    (send dc set-pen
          (send the-pen-list find-or-create-pen "black" 1 'solid 'butt 'miter))
    (send dc set-brush
          (send the-brush-list find-or-create-brush color 'solid))
    (send dc set-origin x y)
    (send p move-to -2 0)
    (send p line-to 2 0)
    (send p line-to 2 16)
    (send p line-to 6 16)
    (send p line-to 0 25)
    (send p line-to -6 16)
    (send p line-to -2 16)
    (send p line-to -2 0)
    (send p rotate (+ (/ pi 2) rotation))
    (send dc draw-path p 0 0 'winding)
    (send dc set-origin 0 0)))


(define (draw-switches dc infrabel)
  (set-for-each
    (get-switches)
    (lambda (id)
      (let* ((n1 (get-node id))
             (n2 (get-node (send infrabel get-current-switch-position id)))
             (n3 (get-node (send infrabel get-alternative-switch-position id)))
             (x1 (send n1 get-x))
             (y1 (send n1 get-y))
             (x2 (send n2 get-x))
             (x3 (send n3 get-x))
             (angle1 (if (> x1 x2)
                       (+ (atan (nodes-slope n1 n2)) pi)
                       (atan (nodes-slope n1 n2))))
             (angle2 (if (> x1 x3)
                       (+ (atan (nodes-slope n1 n3)) pi)
                       (atan (nodes-slope n1 n3)))))
        (draw-arrow dc x1 y1 angle2 "firebrick")
        (draw-arrow dc x1 y1 angle1 "lime green")))))

(define (draw-loco dc infrabel id)
  (let* ((p (new dc-path%))
         (w 30)
         (h 10)
         (position (send infrabel get-loco-position id))
         (n1 (get-node (car position)))
         (n2 (get-node (cdr position)))
         (x1 (send n1 get-x))
         (y1 (send n1 get-y))
         (x2 (send n2 get-x))
         (y2 (send n2 get-y))
         (slope (nodes-slope n1 n2)))
    (send dc set-pen
          (send the-pen-list find-or-create-pen "black" 1 'solid 'butt 'miter))
    (send dc set-brush
          (send the-brush-list find-or-create-brush "crimson" 'solid))
    (send dc set-origin (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
    (send p rectangle (- (/ w 2)) (- (/ h 2)) w h)
    (send p rectangle (- (/ w 2)) (- (/ h 2)) (/ w 4) h)
    (send p rotate (atan slope))
    (send dc draw-path p 0 0 'winding)
    (send dc set-origin 0 0)))

(define (draw-locos dc infrabel)
  (set-for-each
    (get-locos)
    (lambda (id)
      (draw-loco dc infrabel id))))


(define (window-init width height infrabel-command)
  (set! frame (new frame%
                   (label "Railway")
                   (width width)
                   (height (+ height 20)))) ; + 20 for titlebar
  ; (control-panel infrabel-command)
  (new canvas%
       (parent frame)
       (min-width width)
       (min-height height)
       (paint-callback
         (lambda (canvas dc)
           (send dc draw-bitmap (railway-bmp) 0 0)
           (draw-switches dc infrabel-command)
           (draw-locos dc infrabel-command))))
  (send frame show #t))


(define (draw-window)
  (send frame show #t))

