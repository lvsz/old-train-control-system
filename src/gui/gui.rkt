#lang racket/gui

(require "../nmbs/nmbs.rkt"
         "../common/node.rkt")

(provide window%)

(define loco-vector #f)
(define loco-idx 0)
(define active-loco #f)

(define (loco-init)
  (set! loco-vector (list->vector (hash-keys (get-locos))))
  (set! active-loco (vector-ref loco-vector loco-idx)))

(define (change-loco! i)
  (set! loco-idx (modulo (+ loco-idx i) (vector-length loco-vector)))
  (set! active-loco (vector-ref loco-vector loco-idx)))
(define (next-loco!) (change-loco! 1))
(define (prev-loco!) (change-loco! -1))

(define (id->symbol x)
  (cond ((number? x)
         (string->symbol (number->string x)))
        ((string? x)
         (string->symbol x))
        (else x)))

(define (sorted-switches)
  (let ((switch-list (map symbol->string (hash-keys (get-switches)))))
    (map id->symbol (if (string->number (car switch-list))
                      (sort (map string->number switch-list) <)
                      (sort switch-list string<?)))))

(define (control-panel frame)
  (let* ((panel (new vertical-panel%
                     (parent frame)
                     (border 0)
                     (alignment '(center top))
                     ;(style '(auto-vscroll))
                     ;(stretchable-height #f)
                     (stretchable-width #f)))
         (buttons (for/list ((id (in-list (sorted-switches))))
                    (new button%
                         (label (format "Switch ~a" id))
                         (parent panel)
                         (font (send the-font-list find-or-create-font
                                     10 'swiss 'normal 'light))
                         (callback
                           (lambda (b e)
                             (change-switch-position! id)))))))
    panel))

(define (draw-node node dc (node-radius 2))
  (send dc draw-ellipse
        (- (send node get-x) node-radius)
        (- (send node get-y) node-radius)
        (* 2 node-radius)
        (* 2 node-radius)))

(define (railway-bmp)
  (let* ((bmp (make-bitmap (+ 30 (get-railway-width)) (+ 30 (get-railway-height))))
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
      (set! node-layer (cons (lambda () (draw-node n dc)) node-layer))
      (for ((m (in-list (send n get-adjacent))))
        (unless (set-member? visited m)
          (set! rail-layer (cons (lambda ()
                                   (send dc draw-lines (list m n)))
                                 rail-layer)))))
    (send dc set-pen (send the-pen-list find-or-create-pen "black" 3 'solid))
    (send dc set-brush (send the-brush-list find-or-create-brush "black" 'solid))
    (draw-layer rail-layer)
    (draw-layer node-layer)
    bmp))

(define (draw-detection-blocks dc)
  (for-each
    (lambda (db)
      (let* ((nodes (send db get-nodes))
             (n1 (fst-node nodes))
             (n2 (snd-node nodes))
             (color (case (send db get-status)
                      ((red)    "red")
                      ((green)  "limegreen")
                      ((orange) "orange"))))
        (send dc set-pen
              (send the-pen-list find-or-create-pen color 5 'solid))
        (send dc set-brush
              (send the-brush-list find-or-create-brush color'solid))
        (send dc draw-lines (list n1 n2))
        (draw-node n1 dc)
        (draw-node n2 dc)))
  (hash-values (get-detection-blocks))))


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


(define (draw-node-label dc x y label)
  (match-let*-values
    (((label-string)    (~a label))
     ((txt-w txt-h _ _) (send dc get-text-extent label-string))
     ((id-x id-y)       (values (- x (/ txt-w 2)) (- y (/ txt-h 2))))
     ((bg-w)            (+ 6 (max txt-w txt-h)))
     ((bg-x bg-y)       (values (- x (/ bg-w 2)) (- y (/ bg-w 2)))))
    (send dc draw-ellipse bg-x bg-y bg-w bg-w)
    (send dc draw-text label-string id-x id-y)))

(define (draw-node-labels dc)
  (send dc set-pen
        (send the-pen-list find-or-create-pen
              "black" 1 'solid 'butt 'miter))
  (send dc set-brush
        (send the-brush-list find-or-create-brush
              "white" 'solid))
  (send dc set-font
        (send the-font-list find-or-create-font
              9 'swiss 'normal 'bold))
  (hash-for-each
    (get-nodes)
    (lambda (k v)
      (let ((x (send v get-x))
            (y (send v get-y)))
        (draw-node-label dc x y k)))))

(define (draw-switches dc)
  (for-each
    (lambda (id)
      (let* ((n1 (get-node id))
             (n2 (get-current-switch-position id))
             (n3 (get-alternative-switch-position id))
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
        (draw-arrow dc x1 y1 angle1 "cyan")
        (send dc set-pen
              (send the-pen-list find-or-create-pen
                    "black" 2 'solid 'butt 'miter))
        (send dc set-brush
              (send the-brush-list find-or-create-brush
                    "white" 'solid))
        (draw-node-label dc x1 y1 id)))
    (hash-keys (get-switches))))

(define (draw-loco dc loco)
  (let* ((id (send loco get-id))
         (p (new dc-path%))
         (w 30)
         (h 10)
         (color (if (eq? id active-loco) "yellow" "crimson"))
         (current-track (send loco get-current-track))
         (n1 (track-from current-track))
         (n2 (track-to current-track))
         (x1 (send n1 get-x))
         (y1 (send n1 get-y))
         (x2 (send n2 get-x))
         (y2 (send n2 get-y))
         (slope (nodes-slope n1 n2)))
    (send dc set-pen
          (send the-pen-list find-or-create-pen "black" 1 'solid 'butt 'miter))
    (send dc set-brush
          (send the-brush-list find-or-create-brush color'solid))
    (send dc set-origin (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
    (send p rectangle (- (/ w 2)) (- (/ h 2)) w h)
    (send p rectangle (- (/ w 2)) (- (/ h 2)) (/ w 4) h)
    (send p rotate (atan slope))
    (send dc draw-path p 0 0 'winding)
    (send dc set-origin 0 0)))

(define (draw-locos dc)
  (for-each
    (lambda (loco)
      (draw-loco dc loco))
    (hash-values (get-locos))))

(define layer%
  (class object%
    (super-new)
    (init canvas width height)
    (define elements '())
    (define updated #f)
    (define bmp (make-object bitmap% width height #f #t))
    (define bmp-dc (make-object bitmap-dc% bmp))
    (define (redraw)
      (set! bmp (make-object bitmap% width height #f #t))
      (set! bmp-dc (make-object bitmap-dc% bmp))
      (for-each (lambda (e) (send bmp-dc draw-bitmap e 0 0)) elements))
    (define/public (draw dc)
      (unless updated
        (redraw)
        (set! updated #t))
      (send dc draw-bitmap bmp 0 0))
    (define/public (add-element element)
      (set! elements (cons element elements))
      (redraw))))

(define (inc-speed!)
  (set-loco-speed! active-loco (+ (get-loco-speed active-loco) 0.5)))
(define (dec-speed!)
  (set-loco-speed! active-loco (- (get-loco-speed active-loco) 0.5)))

(define window%
  (class object%
    (super-new)
    (init-field width height)
  (define key-callback
    (lambda (key)
      (case key
        ((up) (inc-speed!))
        ((down) (dec-speed!))
        ((left) (prev-loco!))
        ((right) (next-loco!)))))
  (define update-callback void)
  (define buffer-bmp (make-object bitmap% width height))
  (define buffer-bmp-dc (make-object bitmap-dc% buffer-bmp))
  (define layers '())
  (define timer #f)
  (define closed #f)

  (define (paint-callback canvas dc)
    (send buffer-bmp-dc clear)
    (for-each (lambda (layer) (send layer draw buffer-bmp-dc)) layers)
    (draw-detection-blocks buffer-bmp-dc)
    (draw-node-labels buffer-bmp-dc)
    (draw-switches buffer-bmp-dc)
    (draw-locos buffer-bmp-dc)
    (send dc clear)
    (send dc draw-bitmap buffer-bmp 0 0))

  (define my-canvas%
    (class canvas%
      (define/override (on-char event)
        (key-callback (send event get-key-code))
        (sleep 0.2))
      (super-new)))

  (define closing-frame%
    (class frame%
      (super-new)
      (define (on-close)
        (set! closed #t))
      (augment on-close)))

  (define frame (new closing-frame%
                     (label "NMBS")
                     (width width)
                     (alignment '(center center))
                     (height (+ height 22))))

  (define panel (new horizontal-panel%
                     (alignment '(center center))
                     (parent frame)))
  (define buttons (control-panel panel))
  (define canvas (new my-canvas%
                      (parent panel)
                      (min-width width)
                      (min-height height)
                      (paint-callback paint-callback)))

  (define (launch-draw-loop)
    (define (draw-loop)
      (send canvas refresh-now)
      (when (not closed)
        (send timer start 100 #t)))
    (set! timer (make-object timer% draw-loop 1000 #t)))

  (define/public (set-key-callback fn)
    (set! key-callback fn))

  (define/public (add-layer (w width) (h height))
    (let ((layer (make-object layer% canvas w h)))
      (set! layers (cons layer layers))
      layer))

  (define/public (refresh)
    (send canvas refresh-now))

  (let ((main-layer (make-object layer% canvas width height)))
    (send main-layer add-element (railway-bmp))
    (set! layers (cons main-layer layers)))
  (send frame show #t)
  (send panel show #t)
  (send buffer-bmp-dc set-background (make-object color% "white"))
  (loco-init)
  (launch-draw-loop)
  (send canvas focus)))

