;{{{ Fractal area generator generator
(define-syntax make-fractal-area-gen
  (lambda (x)
    (syntax-case x ()
      [(name sym margin color subcolor ocolor op trans tmp)
       (with-syntax ([fun (datum->syntax #'name
                            (string->symbol (string-append "fractal-area-gen-"
                              (symbol->string (syntax->datum #'sym)))))]
                     [tmp (datum->syntax #'name (syntax->datum #'tmp))])
         #`(define (fun init base transform iter sub file-name)
             (let* ([x-min +inf.0] [x-max -inf.0] [y-min +inf.0] [y-max -inf.0] [res0 '()] [res
                 (let loop ([pl base] [l init] [c iter])
                   (if (zero? c) l (begin (if (= c sub) (set! res0 l))
                     (let oloop ([l l] [l0 l])
                       (if (null? l0)
                         (loop (map (lambda (p) (let ([x (car p)] [y (cdr p)])
                             (cons (+ (* (vector-ref transform 0) x) (* (vector-ref transform 2) y))
                                   (+ (* (vector-ref transform 1) x) (* (vector-ref transform 3) y))))) pl)
                               l (1- c))
                         (let ([x (caar l0)] [y (cdar l0)])
                           (let loop ([pl pl] [l l])
                             (if (null? pl) (oloop l (cdr l0))
                               (let ([x (+ x (caar pl))] [y (+ y (cdar pl))])
                                 (cond [(< x x-min) (set! x-min x)]
                                       [(> x x-max) (set! x-max x)])
                                 (cond [(< y y-min) (set! y-min y)]
                                       [(> y y-max) (set! y-max y)])
                                 (loop (cdr pl) (cons (cons x y) l)))))))))))]
                 [xl (- x-min 10)] [xr (+ x-max 10)] [wd (- xr xl)]
                 [yl (- y-min 10)] [yr (+ y-max 10)] [ht (- yr yl)]
                 [data (make-bytevector (* 3 wd ht) 0)] [tmp-file-name tmp])
               (let loop ([l res] [c color])
                 (unless (null? l)
                   (bytevector-u24-set! data
                     (* 3 (+ (* wd (- yr (cdar l))) (- (caar l) xl)))
                     c (endianness big))
                   (loop (cdr l) (if (eqv? (cdr l) res0) subcolor c))))
               (let loop ([l op])
                 (unless (null? l)
                   (bytevector-u24-set! data
                     (* 3 (+ (* wd (- yr (cdar l))) (- (caar l) xl)))
                     ocolor (endianness big))
                   (loop (cdr l))))
               (let ([pic (open-file-output-port
                            tmp-file-name (file-options no-fail))])
                 (put-bytevector pic
                   (string->bytevector
                     (with-output-to-string
                       (lambda () (printf "P6~%~d ~d~%255~%" wd ht)))
                     (current-transcoder)))
                 (put-bytevector pic data)
                 (close-output-port pic))
               (system (with-output-to-string (lambda () (printf "gimp -i -b '~s'" `(begin
                   (define image (car (gimp-file-load RUN-NONINTERACTIVE ,tmp-file-name ,tmp-file-name)))
                   (define item (car (gimp-image-get-active-layer image)))
                   (gimp-layer-add-alpha item)
                   (gimp-image-select-color image CHANNEL-OP-REPLACE item (list 0 0 0))
                   (gimp-edit-bucket-fill item BUCKET-FILL-PATTERN LAYER-MODE-ERASE 100 255 TRUE 0 0)
                   (gimp-selection-none image)
                   #,@(let ([v (syntax->datum #'trans)])
                        (if (not v) #'()
                          #`((gimp-item-transform-matrix item
                               #,(datum->syntax #'name (vector-ref v 0)) #,(datum->syntax #'name (vector-ref v 2)) 0
                               #,(datum->syntax #'name (vector-ref v 1)) #,(datum->syntax #'name (vector-ref v 3)) 0
                               0 0 1))))
                   (gimp-image-resize-to-layers image)
                   (plug-in-autocrop RUN-NONINTERACTIVE image item)
                   (gimp-file-save RUN-NONINTERACTIVE image item ,file-name ,file-name)
                   (gimp-quit 0)))))))))])))
;}}}
;{{{ Fractal area generator
(make-fractal-area-gen rect 10 #x00FFFF #xFF0000 #x0000FF
  '[[0 . 0] [-1 . 0] [1 . 0] [0 . -1] [0 . 1]]
  #f "/tmp/area.ppm")
(make-fractal-area-gen hex 10 #x00FFFF #xFF0000 #x0000FF
  '[[0 . 0] [-1 . 0] [1 . 0] [0 . -1] [0 . 1] [1 . -1] [-1 . 1]]
  #(1 0 ,(inexact -1/2) ,(inexact (/ (sqrt 3) 2)))
  "/tmp/area.ppm")
;}}}

(fractal-area-gen-rect '[[0 . 0]]
  '[[1 . 0]]
  '#(-1 1 -1 -1) 20 1 "rect2.png")
(fractal-area-gen-rect '[[0 . 0]]
  '[[1 . 0] [0 . 1] [-1 . 0] [0 . -1]]
  '#(2 1 -1 2) 9 1 "rect5_1.png")
(fractal-area-gen-rect '[[0 . 0] [1 . 1] [-1 . 1] [-1 . -1] [1 . -1]]
  '[[1 . 2] [-2 . 1] [-1 . -2] [2 . -1]]
  '#(1 2 -2 1) 8 1 "rect5_2.png")
(fractal-area-gen-hex '[[0 . 0]]
  '[[1 . 0] [0 . 1]]
  '#(-1 -1 1 -2) 13 1 "hex3.png")
(fractal-area-gen-hex '[[0 . 0]]
  '[[1 . 0] [-1 . 1] [0 . -1]]
  '#(0 2 -2 2) 10 1 "hex4.png")
(fractal-area-gen-hex '[[0 . 0]]
  '[[1 . 0] [0 . 1] [-1 . 1] [-1 . 0] [0 . -1] [1 . -1]]
  '#(2 1 -1 3) 7 1 "hex7.png")
