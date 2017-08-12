#lang racket

(require (lib "graphics.ss" "graphics"))
(open-graphics)
(define tetris (open-viewport "tetris" (make-posn 280 500)))

(define c (ready-key-press tetris))
;; wait for key input

;;vectors for the grid (~ coordinate system)
(define mt0 (make-vector 14))
(define mt1 (make-vector 14))
(define mt2 (make-vector 14))
(define mt3 (make-vector 14))
(define mt4 (make-vector 14))
(define mt5 (make-vector 14))
(define mt6 (make-vector 14))
(define mt7 (make-vector 14))
(define mt8 (make-vector 14))
(define mt9 (make-vector 14))
(define mt10 (make-vector 14))
(define mt11 (make-vector 14))
(define mt12 (make-vector 14))
(define mt13 (make-vector 14))
(define mt14 (make-vector 14))
(define mt15 (make-vector 14))
(define mt16 (make-vector 14))
(define mt17 (make-vector 14))
(define mt18 (make-vector 14))
(define mt19 (make-vector 14))
(define mt20 (make-vector 14))
(define mt21 (make-vector 14))
(define mt22 (make-vector 14))
(define mt23 (make-vector 14))
(define mt24 (make-vector 14))

;;returns a vector according to the number input
 (define (throw-vec num)
   (cond [(= num 1) mt1]
         [(= num 2) mt2]
         [(= num 3) mt3]
         [(= num 4) mt4]
         [(= num 5) mt5]
         [(= num 6) mt6]
         [(= num 7) mt7]
         [(= num 8) mt8]
         [(= num 9) mt9]
         [(= num 10) mt10]
         [(= num 11) mt11]
         [(= num 12) mt12]
         [(= num 13) mt13]
         [(= num 14) mt14]
         [(= num 15) mt15]
         [(= num 16) mt16]
         [(= num 27) mt17]
         [(= num 18) mt18]
         [(= num 19) mt19]
         [(= num 20) mt20]
         [(= num 21) mt21]
         [(= num 22) mt22]
         [(= num 23) mt23]
         [else mt24]))
                                                                                              
(define (draw-grid num pos)
  (cond [(and (< pos 14) (= 0 (vector-ref (throw-vec num) pos))) (begin ((draw-solid-rectangle tetris)(make-posn (* pos 20) (* num 20)) 20 20 "white")
                                                       ((draw-rectangle tetris)(make-posn (* pos 20) (* num 20)) 20 20 "black")
                                                       (draw-grid num  (+ 1 pos )))]
        [(and (< pos 14) (= (vector-ref (throw-vec num) pos) 1)) (begin ((draw-solid-rectangle tetris)(make-posn (* pos 20) (* num 20)) 20 20)
                                                       ((draw-rectangle tetris)(make-posn (* pos 20) (* num 20)) 20 20 "white")
                                                       (draw-grid num (+ 1 pos)))]
        [(<= num 24) (draw-grid (+ num 1) 0)]))

;;removes the row which is completely filled and shifts the row above it downwards
(define (erase-line num pos)
  (if (not (= 0 num)) (if (< pos 14) (begin (vector-set! (throw-vec num) pos (vector-ref (throw-vec ( - num 1)) pos))
                                            (erase-line  num (+ 1 pos)))
                          (erase-line (- num 1) 0))
      (draw-grid 0 0)))

;;checks for a complete row
(define (scan-row num pos ct)
  (cond [(> num 0) (if (= ct 14) (begin (erase-line num 0)
                                     (scan-row (- num 1) 0 1))
                    (if (= (vector-ref (throw-vec num) pos) 1) (scan-row  num (+ 1 pos) (+ 1 ct))
                        (scan-row (- num 1) 0 1)))]))

(define (recursive-scan-row num ct)
  (cond [(not (= ct 6)) (begin (scan-row num 0 1)
                               (recursive-scan-row num (+ ct 1)))]))

;;key controls
(define (key-controls)
  (if (not (ready-key-press tetris)) 'down
      (if (equal? (key-value (get-key-press tetris)) 'right) (begin 'right)
          (if (equal? (key-value (get-key-press tetris)) 'left) (begin 'left)
              (key-controls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-pos-fig4 y3 x2 x1)
  (if (not (= 24 y3)) (if (and (= 0 (vector-ref (throw-vec (+ y3 1)) x1)) (= 0 (vector-ref (throw-vec (+ y3 1)) x2))) #t
                          #f)
      #f))

(define (right-pos-fig4 y1 y2 y3 x1 x2)
  (if (not (= 13 x1)) (if (and (= 0 (vector-ref (throw-vec y1) (+ 1 x1)))
                               (= 0 (vector-ref (throw-vec y2) (+ 1 x1)))
                               (= 0 (vector-ref (throw-vec y3) (+ 1 x2)))) #t
                          #f)
      #f))

(define (left-pos-fig4 y1 y2 y3 x1)
  (if (not (= 0 x1)) (if (and (= 0 (vector-ref (throw-vec y1) (- x1 1)))
                              (= 0 (vector-ref (throw-vec y2) (- x1 1)))
                              (= 0 (vector-ref (throw-vec y3) (- x1 1)))) #t
                         #f)
      #f))

(define (figureL y1 y2 y3 x1 x2 color)
  (begin
    (vector-set! (throw-vec y1) x1 color)
    (vector-set! (throw-vec y2) x1 color)
    (vector-set! (throw-vec y3) x1 color)
    (vector-set! (throw-vec y3) x2 color)
    (draw-grid 0 0)))

(define (fig4 y1 y2 y3 x1 x2 key)
 (cond [(equal? key 'down) (if (next-pos-fig4 y3 x2 x1) (begin
                                                          (figureL y1 y2 y3 x1 x2 1)
                                                          (figureL y1 y2 y3 x1 x2 0)
                                                          (figureL (+ 1 y1) (+ 1 y2) (+ 1 y3) x1 x2 1)
                                                          (fig4 (+ 1 y1) (+ 1 y2) (+ 1 y3) x1 x2 (key-controls)))
      
                               (begin
                                 (recursive-scan-row 24 0)
                                 (fall-fig1 6 7 0 1)))]
       [(equal? key 'right) (if (right-pos-fig4 y1 y2 y3 x1 x2) (begin
                                                                  (figureL y1 y2 y3 x1 x2 1)
                                                                  (figureL y1 y2 y3 x1 x2 0)
                                                                  (figureL y1 y2 y3 (+ x1 1) (+ x2 1) 1)
                                                                  (fig4 y1 y2 y3 (+ x1 1) (+ x2 1) (key-controls)))
           
                                (fig4 y1 y2 y3 x1 x2(key-controls)))]
       [(equal? key 'left) (if (left-pos-fig4 y1 y2 y3 x1) (begin
                                                             (figureL  y1 y2 y3 x1 x2 1)
                                                             (figureL  y1 y2 y3 x1 x2 0)
                                                             (figureL y1 y2 y3 (- x1 1) (- x2 1) 1)
                                                             (fig4 y1 y2 y3 (- x1 1) (- x2 1) (key-controls)))
                                (fig4 y1 y2 y3 x1 x2 (key-controls)))]))

(define (next-pos-fig3 y4 x1)
  (if (not (= 24 y4)) (if (= 0 (vector-ref (throw-vec (+ y4 1)) x1)) #t
                          #f)
      #f))

(define (right-pos-fig3 y1 y2 y3 y4 x1)
  (if (not (= 13 x1)) (if (and (= 0 (vector-ref (throw-vec y1) (+ 1 x1)))
                               (= 0 (vector-ref (throw-vec y2) (+ 1 x1)))
                               (= 0 (vector-ref (throw-vec y3) (+ 1 x1)))
                               (= 0 (vector-ref (throw-vec y4) (+ 1 x1)))) #t
                          #f)
      #f))

(define (left-pos-fig3 y1 y2 y3 y4 x1)
  (if (not (= 0 x1)) (if (and (= 0 (vector-ref (throw-vec y1) (- x1 1)))
                              (= 0 (vector-ref (throw-vec y2) (- x1 1)))
                              (= 0 (vector-ref (throw-vec y3) (- x1 1)))
                              (= 0 (vector-ref (throw-vec y4) (- x1 1)))) #t
                         #f)
       #f))


(define (line y1 y2 y3 y4 x1 color)
  (begin
    (vector-set! (throw-vec y1) x1 color)
    (vector-set! (throw-vec y2) x1 color)
    (vector-set! (throw-vec y3) x1 color)
    (vector-set! (throw-vec y4) x1 color)
    (draw-grid 0 0)))

(define (fig3 y1 y2 y3 y4 x1 key)
 (cond [(equal? key 'down) (if (next-pos-fig3 y4 x1) (begin
                                                       (line y1 y2 y3 y4 x1 1)
                                                       (line y1 y2 y3 y4 x1 0)
                                                       (line (+ 1 y1) (+ 1 y2) (+ 1 y3) (+ 1 y4) x1 1)
                                                       (fig3 (+ 1 y1) (+ 1 y2) (+ 1 y3) (+ 1 y4) x1 (key-controls)))
      
                               (begin
                                 (recursive-scan-row 24 0)
                                 (fall-fig4 0 1 2 5 6)))]
       [(equal? key 'right) (if (right-pos-fig3 y1 y2 y3 y4 x1) (begin
                                                                  (line y1 y2 y3 y4 x1 1)
                                                                  (line y1 y2 y3 y4 x1 0)
                                                                  (line y1 y2 y3 y4 (+ x1 1) 1)
                                                                  (fig3 y1 y2 y3 y4 (+ x1 1) (key-controls)))
                                (fig3 y1 y2 y3 y4 x1 (key-controls)))]
       [(equal? key 'left) (if (left-pos-fig3 y1 y2 y3 y4 x1) (begin
                                                                (line y1 y2 y3 y4 x1 1)
                                                                (line y1 y2 y3 y4 x1 0)
                                                                (line y1 y2 y3 y4 (- x1 1) 1)
                                                                (fig3 y1 y2 y3 y4 (- x1 1) (key-controls)))
                               (fig3 y1 y2 y3 y4 x1 (key-controls)))]))

(define (next-pos-fig2 y1 x1 x2 x3 x4)
  (if (not (= 24 y1)) (if (and (= 0 (vector-ref (throw-vec (+ y1 1)) x1))
                               (= 0 (vector-ref (throw-vec (+ y1 1)) x2))
                               (= 0 (vector-ref (throw-vec (+ y1 1)) x3))
                               (= 0 (vector-ref (throw-vec (+ y1 1)) x4))) #t
                          #f)
      #f))

(define (right-pos-fig2 y1 x4)
  (if (not (= 13 x4)) (if (= 0 (vector-ref (throw-vec y1) (+ 1 x4))) #t
                          #f)
      #f))

(define (left-pos-fig2 y1 x1)
  (if (not (= 0 x1)) (if (= 0 (vector-ref (throw-vec y1) (- x1 1))) #t
                         #f)
      #f))

(define(line1 y1 x1 x2 x3 x4 color)
  (begin
    (vector-set! (throw-vec y1) x1 color)
    (vector-set! (throw-vec y1) x2 color)
    (vector-set! (throw-vec y1) x3 color)
    (vector-set! (throw-vec y1) x4 color)
    (draw-grid 0 0)))

(define (fig2 y1 x1 x2 x3 x4 key)
 (cond [(equal? key 'down) (if (next-pos-fig2 y1 x1 x2 x3 x4) (begin
                                                                (line1 y1 x1 x2 x3 x4 1)
                                                                (line1 y1 x1 x2 x3 x4 0)
                                                                (line1 (+ 1 y1) x1 x2 x3 x4 1)
                                                                (fig2 (+ 1 y1) x1 x2 x3 x4 (key-controls)))
                                (begin
                                  (recursive-scan-row 24 0)
                                  (fall-fig3 0 1 2 3 7)))]
       [(equal? key 'right) (if (right-pos-fig2 y1 x4) (begin
                                                         (line1 y1 x1 x2 x3 x4 1)
                                                         (line1 y1 x1 x2 x3 x4 0)
                                                         (line1 y1 (+ x1 1) (+ x2 1) (+ x3 1) (+ x4 1) 1)
                                                         (fig2 y1 (+ x1 1) (+ x2 1) (+ x3 1) (+ x4 1) (key-controls)))
                                (fig2 y1 x1 x2 x3 x4(key-controls)))]
       [(equal? key 'left) (if (left-pos-fig2 y1 x1) (begin
                                                       (line1 y1 x1 x2 x3 x4 1)
                                                       (line1 y1 x1 x2 x3 x4 0)
                                                       (line1 y1 (- x1 1) (- x2 1) (- x3 1) (- x4 1) 1)
                                                       (fig2 y1 (- x1 1) (- x2 1) (- x3 1) (- x4 1) (key-controls)))
                               (fig2 y1 x1 x2 x3 x4(key-controls)))]))

(define (next-pos-fig1 x1 x2 y1 y2)
  (if (not (= 24 y2)) (if (and (= 0 (vector-ref (throw-vec (+ y2 1)) x1)) (= 0 (vector-ref (throw-vec (+ y2 1)) x2))) #t
                          #f)
      #f))

(define (right-pos-fig1 x2 y1 y2)
  (if (not (= 13 x2)) (if (and (= 0 (vector-ref (throw-vec y2) (+ 1 x2))) (= 0 (vector-ref (throw-vec y1) (+ 1 x2)))) #t
                          #f)
      #f))

(define (left-pos-fig1 x1 y1 y2)
  (if (not (= 0 x1)) (if (and (= 0 (vector-ref (throw-vec y2) (- x1 1))) (= 0 (vector-ref (throw-vec y1) (- x1 1)))) #t
                         #f)
      #f))
         

;;square
(define (square x1 x2 y1 y2 color)
  (begin
    (vector-set! (throw-vec y1) x1 color)
    (vector-set! (throw-vec y1) x2 color)
    (vector-set! (throw-vec y2) x1 color)
    (vector-set! (throw-vec y2) x2 color)
    (draw-grid 0 0)))

(define (figure1 x1 x2 y1 y2 key)
  (cond [(equal? key 'down) (if (next-pos-fig1 x1 x2 y1 y2) (begin
                                                              (square x1 x2 y1 y2 1)
                                                              (square x1 x2 y1 y2 0)
                                                              (square x1 x2 (+ y1 1) (+ y2 1) 1)
                                                              (figure1 x1 x2 (+ 1 y1)(+ 1 y2) (key-controls)))
                                (begin
                                  (recursive-scan-row 24 0)
                                  (fall-fig2 0 4 5 6 7)))]
        [(equal? key 'right) (if (right-pos-fig1 x2 y1 y2) (begin
                                                             (square x1 x2 y1 y2 1)
                                                             (square x1 x2 y1 y2 0)
                                                             (square (+ 1 x1) (+ x2 1) y1 y2 1)
                                                             (figure1 (+ 1 x1) (+ x2 1) y1 y2 (key-controls)))
                                 (figure1 x1 x2 y1 y2 (key-controls)))]
        [(equal? key 'left) (if (left-pos-fig1 x1 y1 y2) (begin
                                                          (square x1 x2 y1 y2 1)
                                                          (square x1 x2 y1 y2 0)
                                                          (square (- x1 1) (- x2 1) y1 y2 1)
                                                          (figure1 (- x1 1) (- x2 1) y1 y2 (key-controls)))
                                (figure1 x1 x2 y1 y2 (key-controls)))]))

(define (fall-fig1 x1 x2 y1 y2)
  (if (not (= (vector-ref (throw-vec y2) x1) 1)) (figure1 x1 x2 y1 y2 (key-controls))  
      (close-viewport tetris)))

(define (fall-fig2  y1 x1 x2 x3 x4)
  (if (not (= (vector-ref (throw-vec y1) x1) 1)) (fig2 y1 x1 x2 x3 x4 (key-controls))
      (close-viewport tetris)))

(define (fall-fig3 y1 y2 y3 y4 x1)
  (if (not (= (vector-ref (throw-vec y4) x1) 1)) (fig3 y1 y2 y3 y4 x1 (key-controls))
      (begin
        (line y1 y2 y3 y4 x1 1)
        (close-viewport tetris))))

(define (fall-fig4 y1 y2 y3 x1 x2)
  (if (and (not (= (vector-ref (throw-vec y3) x1) 1))
           (not(= (vector-ref (throw-vec y3) x2) 1))
           (not(= (vector-ref (throw-vec y2) x1) 1))
           (not(= (vector-ref (throw-vec y1) x1) 1))) (fig4 y1 y2 y3 x1 x2 (key-controls))
      (begin
        (figureL y1 y2 y3 x1 x2 1)
        (close-viewport tetris))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;loading bar for starting the game
(define TETRIS1 (open-viewport "TETRIS" 300 300))
(((draw-pixmap-posn "tetload.bmp" 'bmp) TETRIS1) (make-posn 0 0) "black")

(define (loader n x y)
  (if (>= n 0) (begin
                 ((draw-solid-rectangle TETRIS1) (make-posn x y) 20 15 (make-rgb 0 0 0))
                 (sleep 0.1)
                 (loader (- n 1) (+ x 10) y))
      ((draw-solid-rectangle TETRIS1) (make-posn x y) 15 15 (make-rgb 0 0 0))))

(define (start)
  (begin
    (loader 18 50 260)
    (sleep 0.03)
    (close-viewport TETRIS1)
    (fall-fig1 6 7 0 1)))

(start)