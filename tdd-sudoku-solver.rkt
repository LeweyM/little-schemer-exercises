#lang racket

(define test
  (lambda (test-fn name expected . input)
    (if (equal? (apply test-fn input) expected)
        (fprintf (current-output-port) "\n~s OK." name)
        (fprintf (current-output-port) "\n~s: failed. Expected: ~a. Got: ~a"
                 name
                 expected
                 (apply test-fn input)))))

(define parse-sudoku-string
  (lambda (s)
    (map (lambda (x) (- x 48))
         (map char->integer
              (map (lambda (n) (if (eq? n #\.) #\0 n))
                   (string->list s))))))

(define solve
  (lambda (sudoku)
    (if
     (empty? (empty-square-indicies sudoku))
      sudoku
      (let ([i (most-constrained-square sudoku)])
        (if (false? i)
            #f 
            (let loop ([values (possible-values-for-square sudoku i)])
              (cond
                [(empty? values) #f]
                [else (or 
                 (solve (sudoku-set sudoku (index-to-x i) (index-to-y i) (car values)))
                 (loop (cdr values)))])))))))

(define empty-square-indicies
  (lambda (sudoku)
    (let loop ([sudoku sudoku] [i 0] [acc '()])
      (cond
        [(empty? sudoku) acc]
        [(= (car sudoku) 0) (loop (cdr sudoku) (add1 i) (append acc (list i)))]
        [else (loop (cdr sudoku) (add1 i) acc)]))))

(define most-constrained-square
  (lambda (sudoku)
    (let loop ([empty-square-index-lst (empty-square-indicies sudoku)] [acc-index #f] [acc-possible-values-count 99]) ;could keep pos values here
      ;(fprintf (current-output-port) "\nindex: ~s, possibles value count:~s " acc-index acc-possible-values-count)
      (cond
        [(empty? empty-square-index-lst) acc-index]
        [(eq? acc-possible-values-count 1) acc-index]
        [(and
          (< (length (possible-values-for-square sudoku (car empty-square-index-lst))) acc-possible-values-count)
          (> (length (possible-values-for-square sudoku (car empty-square-index-lst))) 0))
         (loop (cdr empty-square-index-lst) (car empty-square-index-lst) (length (possible-values-for-square sudoku (car empty-square-index-lst))))]
        [else
         (loop (cdr empty-square-index-lst) acc-index acc-possible-values-count)]
        ))))

(define possible-values-for-square
  (lambda (sudoku i)
    (define by-row (complement (row-by-square sudoku i)))
    (define by-col (complement (column-by-square sudoku i)))
    (define by-block (complement (block-by-square sudoku i)))

    (filter by-block (filter by-col (filter by-row '(1 2 3 4 5 6 7 8 9))))))

(define row-by-square
  (lambda (sudoku i)
    (let loop ([j 0] [acc '()])
      (cond
        [(= j 9) (remove* '(0) acc)]
        [else (loop
               (add1 j)
               (append acc (list (sudoku-get sudoku j (index-to-y i)))))]))))

(define column-by-square
  (lambda (sudoku i)
    (let loop ([j 0] [acc '()])
      (cond
        [(= j 9) (remove* '(0) acc)]
        [else (loop
               (add1 j)
               (append acc (list (sudoku-get sudoku (index-to-x i) j))))]))))

(define block-by-square
  (lambda (sudoku i)
    (let loop ([j 0] [acc '()])
      (cond
        [(= j 9) (remove* '(0) acc)]
        [else (loop
               (add1 j)
               (append acc (list (sudoku-get sudoku
                                             (+ (* 3 (quotient (index-to-x i) 3)) (remainder j 3))
                                             (+ (* 3 (quotient (index-to-y i) 3)) (quotient j 3))))))])) ))

(define complement
  (lambda (lst)
    (lambda (x)
      (not (member x lst)))))

(define index-to-x
  (lambda (i)
    (remainder i 9)))

(define index-to-y
  (lambda (i)
   (quotient i 9)))

(define x-y-to-index
  (lambda (x y)
    (+ x (* 9 y))))

(define sudoku-get
  (lambda (sudoku x y)
    (vector-ref (list->vector sudoku) (+ x (* 9 y)))))

(define sudoku-set
  (lambda (sudoku x y new-value)
    (let loop ([i 0] [s sudoku])
      (cond
        [(empty? s) '()]
        [(eq? (x-y-to-index x y) i) (cons new-value (loop (add1 i) (cdr s)))]
        [else (cons (car s) (loop (add1 i) (cdr s)))]))))

(define sudoku->string
  (lambda (sudoku)
    (string-join (map
                  (lambda (x) (if (string=? x "0") " " x))
                  (add-divider-lines (map number->string sudoku))))))

(define add-divider-lines
  (lambda (sudoku)
    (add-divider-lines-aux sudoku 0)))

(define add-divider-lines-aux
  (lambda (sudoku i)
      (cond
        [(empty? sudoku) '()]
        [(or (eq? i 27) (eq? i 54)) (if (is-string-number (car sudoku))
                                      (cons "\n-------+-------+-------\n" (cons (car sudoku) (add-divider-lines-aux (cdr sudoku) (add1 i))))
                                      (cons "\n-------+-------+-------\n" (cons (car sudoku) (add-divider-lines-aux (cdr sudoku) i))))]
        [(eq? (remainder i 9) 0) (if (is-string-number (car sudoku))
                                      (cons "\n" (cons (car sudoku) (add-divider-lines-aux (cdr sudoku) (add1 i))))
                                      (cons "\n" (cons (car sudoku) (add-divider-lines-aux (cdr sudoku) i))))]
        [(eq? (remainder i 3) 0) (if (is-string-number (car sudoku))
                                      (cons "|" (cons (car sudoku) (add-divider-lines-aux (cdr sudoku) (add1 i))))
                                      (cons "|" (cons (car sudoku) (add-divider-lines-aux (cdr sudoku) i))))]
        [else (if (is-string-number (car sudoku))
                  (cons (car sudoku) (add-divider-lines-aux (cdr sudoku) (add1 i)))
                  (cons (car sudoku) (add-divider-lines-aux (cdr sudoku) i)))])))
        

(define is-string-number
  (lambda (i)
    (not (not (member i '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" " "))))))

(define easy-1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")
(define easy-1-changed "503020600900305001001806400008102900700000008006708200002609500800203009005010300")
(define easy-1-solved '(4 8 3 9 2 1 6 5 7 9 6 7 3 4 5 8 2 1 2 5 1 8 7 6 4 9 3 5 4 8 1 3 2 9 7 6 7 2 9 5 6 4 1 3 8 1 3 6 7 9 8 2 4 5 3 7 2 6 8 9 5 1 4 8 1 4 2 5 3 7 6 9 6 9 5 4 1 7 3 8 2))
(define hard-1 "7..1523........92....3.....1....47.8.......6............9...5.6.4.9.7...8....6.1.")
(define hard-1-solved '(7 9 6 1 5 2 3 8 4 5 3 1 4 6 8 9 2 7 4 2 8 3 7 9 6 5 1 1 5 2 6 3 4 7 9 8 3 8 4 7 9 1 2 6 5 9 6 7 2 8 5 1 4 3 2 1 9 8 4 3 5 7 6 6 4 5 9 1 7 8 3 2 8 7 3 5 2 6 4 1 9))
(define mega-hard "7..15..........92....3.....1....47.8....................9...5.6.4.9.7...8....6.1.")

(test parse-sudoku-string "sudoku parser should parse to numbers" '(1 2 3) "123")
(test parse-sudoku-string "sudoku parser should replace '.' with 0s" '(0 1 0) ".1.")
(test empty-square-indicies "empty-square-indicies" '(2 3) '(5 2 0 0 1))
(test most-constrained-square "most-constrained-square" 41 (parse-sudoku-string easy-1))
(test possible-values-for-square "possible-values-for-square" '(4 5) (parse-sudoku-string easy-1) 0)
(test row-by-square "row-by-square at 0" '(3 2 6) (parse-sudoku-string easy-1) 0)
(test row-by-square "row-by-square at 8" '(3 2 6) (parse-sudoku-string easy-1) 8)
(test column-by-square "column-by-square at 0" '(9 7 8) (parse-sudoku-string easy-1) 0)
(test column-by-square "column-by-square at 9" '(9 7 8) (parse-sudoku-string easy-1) 9)
(test block-by-square "block-by-square at 0" '(3 9 1) (parse-sudoku-string easy-1) 0)
(test block-by-square "block-by-square at 3" '(2 3 5 8 6) (parse-sudoku-string easy-1) 3)
(test sudoku-set "sudoku-set" (parse-sudoku-string easy-1-changed) (parse-sudoku-string easy-1) 0 0 5)
(test sudoku-get "sudoku-get" 8 (parse-sudoku-string easy-1) 2 3)
(test solve "solve easy" easy-1-solved (parse-sudoku-string easy-1))
(test solve "solve hard" hard-1-solved (parse-sudoku-string hard-1))

(displayln "\n\n---------Demo---------")
(displayln (sudoku->string (parse-sudoku-string mega-hard)))
(displayln "\n--------Solution--------")
(displayln (sudoku->string (solve (parse-sudoku-string mega-hard))))





