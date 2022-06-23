
(define-macro (def func args body)
  ; `(define ,func (lambda ,args ,body))
  `(define ,(cons func args) ,body)
  ; both are ok
)


(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

(define all-three-multiples
  (begin
  (define (natural n) (cons-stream n (natural (+ n 1))))
  (map-stream (lambda (x) (* x 3)) (natural 1)))
)


(define (compose-all funcs)
  (if (null? funcs)
    (lambda (x) x)
    (lambda (x) ((compose-all (cdr funcs)) ((car funcs) x))))
)


(define (partial-sums stream)
  (define (helper tot s)
    (if (null? s)
      ()
      (cons-stream (+ tot (car s)) (helper (+ tot (car s)) (cdr-stream s)))))
  (helper 0 stream)
)

