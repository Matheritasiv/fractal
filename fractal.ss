;{{{ Fractal curve vertex list generator generator
(define-syntax make-fractal-gen
  (lambda (x)
    (syntax-case x ()
      [(name sym)
       (with-syntax ([fun (datum->syntax #'name
                            (string->symbol (string-append "fractal-gen-"
                              (symbol->string (syntax->datum #'sym)))))]
                     [shape (datum->syntax #'name 'shape)])
         #'(define (fun scale vert dir)
             (let ([ndir (reverse! (map not dir))]) (lambda (n)
               (when (and (integer? n) (>= n 0))
                 (let loop ([a (cons 0 0)] [n n] [d #f]
                            [l (list (cons (expt scale n) 0))])
                   (if (zero? n) (cons a l)
                     (let ([ps (cons a (let ([ps
                           (let-values ([(a b) (if d
                               (values (car l) a) (values a (car l)))])
                             (let* ([aa (car a)] [da (cdr a)]
                                    [x (/ (- (car b) aa) scale)]
                                    [y (/ (- (cdr b) da) scale)])
                               (map (lambda (p)
                                      (let ([ap (car p)] [dp (cdr p)])
                                        (cons (+ aa (* x ap) (* y (- dp)))
                                              (+ da (* x dp) (* y (shape ap dp))))))
                                    vert)))])
                         (if d (reverse! ps) ps)))])
                       (fold-right (lambda (x y) (x y)) l
                         (map (lambda (x y) (lambda (l) (loop x (1- n) y l)))
                              ps (if d ndir dir)))))))))))])))
;}}}
;{{{ Fractal curve drawer generator generator
(define-syntax make-fractal-curve-gen
  (lambda (x)
    (syntax-case x ()
      [(name sym)
       (with-syntax ([fun (datum->syntax #'name
                            (string->symbol (string-append "fractal-curve-gen-"
                              (symbol->string (syntax->datum #'sym)))))]
                     [shape (datum->syntax #'name 'shape)])
         #'(define (fun scale vert dir)
             (let ([ndir (reverse! (map not dir))]) (lambda (bx by draw n)
               (when (and (integer? n) (>= n 0))
                 (let* ([denom (expt scale n)] [idenom (inexact denom)]
                        [ax (car bx)] [dx (cdr bx)] [ay (car by)] [dy (cdr by)]
                        [lastp (cons (inexact ax) (inexact dx))])
                   (let loop ([a (cons 0 0)] [n n] [d #f] [l (cons denom 0)])
                     (if (zero? n)
                       (let ([last lastp] [ab (car a)] [db (cdr a)])
                         (draw last (begin
                               (set! lastp
                                 (cons (/ (+ (* ab ax) (* db ay)) idenom)
                                       (/ (+ (* ab dx) (* db dy)) idenom)))
                               lastp))
                         a)
                       (let ([ps (cons a (let ([ps
                             (let-values ([(a b) (if d (values l a) (values a l))])
                               (let* ([aa (car a)] [da (cdr a)]
                                      [x (/ (- (car b) aa) scale)]
                                      [y (/ (- (cdr b) da) scale)])
                                 (map (lambda (p)
                                        (let ([ap (car p)] [dp (cdr p)])
                                          (cons (+ aa (* x ap) (* y (- dp)))
                                                (+ da (* x dp) (* y (shape ap dp))))))
                                      vert)))])
                           (if d (reverse! ps) ps)))])
                         (fold-right (lambda (x y) (x y)) l
                           (map (lambda (x y) (lambda (l) (loop x (1- n) y l)))
                                ps (if d ndir dir))))))
                   (void)))))))])))
;}}}
;{{{ Block-based fractal curve vertex list generator generator
(define-syntax make-bfractal-gen
  (lambda (x)
    (syntax-case x ()
      [(name sym)
       (with-syntax ([fun (datum->syntax #'name
                            (string->symbol (string-append "bfractal-gen-"
                              (symbol->string (syntax->datum #'sym)))))]
                     [shape (datum->syntax #'name 'shape)])
         #'(define (fun scale loc vert dir)
             (let ([ndir (reverse! (map not dir))]) (lambda (n)
               (when (and (integer? n) (>= n 0))
                 (let loop ([a (cons 0 0)] [b (cons (expt scale n) 0)]
                            [n n] [d #f] [l '()])
                   (call/cc (lambda (k)
                     (let ([ps (cons a (let ([ps
                           (let-values ([(a b) (if d (values b a) (values a b))])
                             (let* ([aa (car a)] [da (cdr a)]
                                    [x& (- (car b) aa)] [x (/ x& scale)]
                                    [y& (- (cdr b) da)] [y (/ y& scale)])
                               (if (zero? n)
                                 (let ([ap (car loc)] [dp (cdr loc)])
                                   (k (cons
                                        (cons (+ aa (* x& ap) (* y& (- dp)))
                                              (+ da (* x& dp) (* y& (shape ap dp))))
                                        l))))
                               (map (lambda (p)
                                      (let ([ap (car p)] [dp (cdr p)])
                                        (cons (+ aa (* x ap) (* y (- dp)))
                                              (+ da (* x dp) (* y (shape ap dp))))))
                                    vert)))])
                         (if d (reverse! ps) ps)))])
                       (let loop ([l ps])
                         (set-car! l (cons (car l) (cdr l)))
                         (if (null? (cdr l))
                           (set-cdr! (car l) (list (list b)))
                           (loop (cdr l))))
                       (fold-right (lambda (x y) (x y)) l
                         (map (lambda (x y) (lambda (l)
                                (loop (car x) (caadr x) (1- n) y l)))
                              ps (if d ndir dir))))))))))))])))
;}}}
;{{{ Block-based fractal curve drawer generator generator
(define-syntax make-bfractal-curve-gen
  (lambda (x)
    (syntax-case x ()
      [(name sym)
       (with-syntax ([fun (datum->syntax #'name
                            (string->symbol (string-append "bfractal-curve-gen-"
                              (symbol->string (syntax->datum #'sym)))))]
                     [shape (datum->syntax #'name 'shape)])
         #'(define (fun scale loc vert dir)
             (let ([ndir (reverse! (map not dir))]) (lambda (bx by draw n)
               (when (and (integer? n) (>= n 0))
                 (let* ([denom (expt scale n)] [idenom (inexact denom)]
                        [ax (car bx)] [dx (cdr bx)] [ay (car by)] [dy (cdr by)]
                        [lastp #f])
                   (let loop ([a (cons 0 0)] [b (cons denom 0)] [n n] [d #f])
                     (call/cc (lambda (k)
                       (let ([ps (cons a (let ([ps
                             (let-values ([(a& b) (if d (values b a) (values a b))])
                               (let* ([aa (car a&)] [da (cdr a&)]
                                      [x& (- (car b) aa)] [x (/ x& scale)]
                                      [y& (- (cdr b) da)] [y (/ y& scale)])
                                 (if (zero? n)
                                   (let* ([ap (car loc)] [dp (cdr loc)] [last lastp]
                                          [ab (+ aa (* x& ap) (* y& (- dp)))]
                                          [db (+ da (* x& dp) (* y& (shape ap dp)))])
                                     (set! lastp
                                       (cons (/ (+ (* ab ax) (* db ay)) idenom)
                                             (/ (+ (* ab dx) (* db dy)) idenom)))
                                     (if last (draw last lastp))
                                     (k a)))
                                 (map (lambda (p)
                                        (let ([ap (car p)] [dp (cdr p)])
                                          (cons (+ aa (* x ap) (* y (- dp)))
                                                (+ da (* x dp) (* y (shape ap dp))))))
                                      vert)))])
                           (if d (reverse! ps) ps)))])
                         (fold-right (lambda (x y) (x y)) b
                           (map (lambda (x y) (lambda (b) (loop x b (1- n) y)))
                                ps (if d ndir dir)))))))
                   (void)))))))])))
;}}}
;{{{ Fractal curve generator
(let-syntax ([shape (syntax-rules ()
    [(_ x y) x])])
  (make-fractal-gen rect)
  (make-fractal-curve-gen rect)
  (make-bfractal-gen rect)
  (make-bfractal-curve-gen rect))
(let-syntax ([shape (syntax-rules ()
    [(_ x y) (+ x y)])])
  (make-fractal-gen hex)
  (make-fractal-curve-gen hex)
  (make-bfractal-gen hex)
  (make-bfractal-curve-gen hex))
;}}}

;{{{ Eight segment curve
(define Eight-meta '[4
  [[1 . 0] [1 . 1] [2 . 1] [2 . 0] [2 . -1] [3 . -1] [3 . 0]]
  [#f #f #f #f #f #f #f #f]])
(define Eight-list (apply fractal-gen-rect Eight-meta))
(define Eight-draw (apply fractal-curve-gen-rect Eight-meta))
;}}}
;{{{ Levy C curve
(define Levy-meta '[2
  [[1 . 1]]
  [#f #f]])
(define Levy-list (apply fractal-gen-rect Levy-meta))
(define Levy-draw (apply fractal-curve-gen-rect Levy-meta))
;}}}
;{{{ Dragon curve
(define Dragon-meta '[2
  [[1 . 1]]
  [#f #t]])
(define Dragon-list (apply fractal-gen-rect Dragon-meta))
(define Dragon-draw (apply fractal-curve-gen-rect Dragon-meta))
;}}}
;{{{ Dragon curve (no self-intersection)
(define Dragon-nsi-meta '[2 [1/2 . 0]
  [[1 . 1]]
  [#f #t]])
(define Dragon-nsi-list (apply bfractal-gen-rect Dragon-nsi-meta))
(define Dragon-nsi-draw (apply bfractal-curve-gen-rect Dragon-nsi-meta))
;}}}
;{{{ Terdragon curve
(define Terdragon-meta '[3
  [[1 . 1] [2 . -1]]
  [#f #f #f]])
(define Terdragon-list (apply fractal-gen-hex Terdragon-meta))
(define Terdragon-draw (apply fractal-curve-gen-hex Terdragon-meta))
;}}}
;{{{ Sierpinski arrowhead curve
(define Sierpinski-meta '[2
  [[0 . 1] [1 . 1]]
  [#t #f #t]])
(define Sierpinski-list (apply fractal-gen-hex Sierpinski-meta))
(define Sierpinski-draw (apply fractal-curve-gen-hex Sierpinski-meta))
;}}}
;{{{ Koch snowflake curve
(define Snowflake-meta '[3
  [[1 . 0] [1 . 1] [2 . 0]]
  [#f #f #f #f]])
(define Snowflake-list (apply fractal-gen-hex Snowflake-meta))
(define Snowflake-draw (apply fractal-curve-gen-hex Snowflake-meta))
;}}}
;{{{ Gosper flowsnake curve
(define Flowsnake-meta '[7
  [[3 . -1] [4 . 1] [1 . 2] [-1 . 5] [2 . 4] [5 . 3]]
  [#f #t #t #f #f #f #t]])
(define Flowsnake-list (apply fractal-gen-hex Flowsnake-meta))
(define Flowsnake-draw (apply fractal-curve-gen-hex Flowsnake-meta))
;}}}
;{{{ Peano curve
(define Peano-meta '[3 [1/2 . 0]
  [[1 . 0] [1 . -1] [2 . -1] [2 . 0] [1 . 0] [1 . 1] [2 . 1] [2 . 0]]
  [#f #f #f #f #f #f #f #f #f]])
(define Peano-list (apply bfractal-gen-rect Peano-meta))
(define Peano-draw (apply bfractal-curve-gen-rect Peano-meta))
;}}}
;{{{ Hilbert curve
(define Hilbert-meta '[2 [1/2 . 1/2]
  [[0 . 1] [1 . 1] [2 . 1]]
  [#t #f #f #t]])
(define Hilbert-list (apply bfractal-gen-rect Hilbert-meta))
(define Hilbert-draw (apply bfractal-curve-gen-rect Hilbert-meta))
;}}}
;{{{ Triangle filling curve
(define Triangle-meta '[2 [1/3 . 1/3]
  [[1 . 0] [0 . 1] [1 . 1]]
  [#f #t #f #t]])
(define Triangle-list (apply bfractal-gen-hex Triangle-meta))
(define Triangle-draw (apply bfractal-curve-gen-hex Triangle-meta))
;}}}

(for-each
  (lambda (p)
    (let ([a (car p)] [b (cdr p)])
      (printf "~6$ ~6$~%"
              (+ a (* b 1/2))
              (+ (* b (/ (sqrt 3) 2))))))
  (Flowsnake-list 5))

#;(Flowsnake-draw
  (cons 640 0)
  (cons 320 (* (sqrt 3) 320))
  (lambda (a b)
    (printf "Draw a line from (~6$, ~6$) to (~6$, ~6$).~%"
      (car a) (cdr a) (car b) (cdr b)))
  6)
