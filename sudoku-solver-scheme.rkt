#lang racket

(define easy-1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")

(define (build-sudoku l)
  (let ([sudoku-lst (parse-char-list (string->list l))])
    (define sudoku-vec (list->vector sudoku-lst))
    
    (define fill-board
      (lambda (l index)
        (println l)
        (displayln (square-index-with-least-possible-values sudoku-vec))
        (if (> (square-index-with-least-possible-values sudoku-vec) -1)
            (if (= (car l) 0)
                (try-each-possible-value l index)
                (if (false? (fill-board (cdr l) (square-index-with-least-possible-values sudoku-vec) ))
                    #f
                    (cons (car l ) (fill-board (cdr l) (square-index-with-least-possible-values sudoku-vec) ))))
            sudoku-vec))) ;must be re-written to loop while square index > 0, then return the sudoku vector

    (define try-each-possible-value
      (lambda (l i)
        (let loop ([pos-values (get-possible-values sudoku-vec i)])
          (if (empty? pos-values)
              (begin
                (vector-set! sudoku-vec i 0)
                #f)
              (begin
                (vector-set! sudoku-vec i (car pos-values))
                (let ([tail (fill-board (cdr l) (add1 i))])
                  (if (false? tail)
                      (loop (cdr pos-values))
                      (begin (cons (car pos-values) tail)))))))))
    
    (fill-board sudoku-lst (square-index-with-least-possible-values sudoku-vec))))

(define empty-square-indicies
  (lambda (sudoku-vec)
    (let loop ([i 0] [acc '()])
      (cond
        [(= i 80) acc]
        [(= (vector-ref sudoku-vec i) 0) (cons i (loop (add1 i) acc ))]
        [else (loop (add1 i) acc)]
        ))))

(define square-index-with-least-possible-values
  (lambda (sudoku-vec)
    (let loop (
               [indicies (empty-square-indicies sudoku-vec)]
               [smallest-index->value (cons -1 99)]
               )
      (define count-of-possible-values (length (get-possible-values sudoku-vec (car indicies))))
      (cond
        [(empty? indicies) smallest-index->value] ;end of loop, return smallest index value map
        [(= 1 (length (get-possible-values sudoku-vec (car indicies)))) ;if only one, return immediatly
         (car smallest-index->value)]
        [(< count-of-possible-values (cdr smallest-index->value)) ;if current index has smaller value count than previous smallest, replace smallest with current
         (loop (cdr indicies) (cons (car indicies) count-of-possible-values ))]
        [else (loop (cdr indicies) smallest-index->value)] ;continue loop
        ))))

;algorithm for getting next index to try values
;- collect indexes of empty squares
;- calculate number of possible values for each square
;- return the index of the square with smallest number of possible values

(define get-possible-values ;list of possible values for index
  (lambda (sudoku-vec i)
    (define by-row (complement (values-in-row sudoku-vec (row-number-from-index i))))
    (define by-col (complement (values-in-col sudoku-vec (col-number-from-index i))))
    
    (filter by-row (filter by-col '(1 2 3 4 5 6 7 8 9)))))

(define complement
  (lambda (lst)
    (lambda (x)
      (not (member x lst)))))

(define values-in-row
  (lambda (sudoku-vector row-index)
    (let loop ([i 0])
      (if (< i 9)
          (cons (vector-ref sudoku-vector (+ (* row-index 9) i)) (loop (add1 i)))
          '() ))))


(define values-in-col
  (lambda (sudoku-vector col-index)
    (let loop ([i 0])
      (if (< i 9)
          (cons (vector-ref sudoku-vector (+ (* i 9) col-index)) (loop (add1 i)))
          '() ))))

(define row-number-from-index
  (lambda (board-index)
    (quotient board-index 9)))

(define col-number-from-index
  (lambda (board-index)
    (remainder board-index 9)))

(define (parse-char-list l)
  (map (lambda (x) (- x 48)) (map char->integer l)))

(define (print-sudoku b)
  (for-each (lambda (l) (displayln (remove-0s l)))
   (get-rows b)))

(define (get-rows l)
  (reverse (get-rows-reversed l)))

(define (get-rows-reversed l)
  (if (empty? l)
      null 
      (cons (get-last-row l) (get-rows-reversed (drop-right l 9)) )))

(define (get-last-row l)
  (list-tail l (- (length l) 9)))

(define (remove-0s l)
  (map (lambda (x) (if (= x 0) " " x)) l))

;(println (square-index-with-least-possible-values (list->vector (parse-char-list (string->list easy-1)))))


(print-sudoku (parse-char-list (string->list easy-1)))
(println " ------------ ")
(print-sudoku '(4 8 3 9 2 1 6 5 7 9 6 7 3 4 5 8 2 1 2 5 1 8 7 6 4 9 3 5 4 8 1 3 2 9 7 6 7 2 9 5 6 4 1 3 8 1 3 6 7 9 8 2 4 5 3 7 2 6 8 9 5 1 4 8 1 4 2 5 3 7 6 9 6 9 5 4 1 7 3 8 2))

"done"


