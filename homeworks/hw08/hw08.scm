(define (rle s)
  (if (null? s)
    ()
    (begin ; The if clause will eval as one expression, so the define will evaluate to str
           ; then apply the str to argument (helper s 1)
           ; so we need begin here to eval all expressions in the clause
    (define (helper s count)
      (if (null? (cdr-stream s))
        (cons-stream `(,(car s) ,count) nil)
        (if (= (car s) (car (cdr-stream s)))
          (helper (cdr-stream s) (+ count 1))
          (cons-stream `(,(car s) ,count) (helper (cdr-stream s) 1)))))
    (helper s 1)))
)


; each iter will compute until the next cons-stream occur
(define (group-by-nondecreasing s)
  (if (null? s)
    ()
    (begin
    (define (helper s cur)
      (if (null? (cdr-stream s))
        (cons-stream cur nil)
        (if (not (> (car s) (car (cdr-stream s))))
          (helper (cdr-stream s) (append cur (list (car (cdr-stream s)))))
          (cons-stream cur (helper (cdr-stream s) (list (car (cdr-stream s))))))))
    (helper s (list (car s)))))
)

(define finite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 3
                (cons-stream 1
                    (cons-stream 2
                        (cons-stream 2
                            (cons-stream 1 nil))))))))

(define infinite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 2
                infinite-test-stream))))

