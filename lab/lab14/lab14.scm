(define (split-at lst n)
  (if (> n (length lst))
	`(,lst)
	(if (= n 0)
	  (cons nil lst)
	  (begin
	  	(define split (split-at (cdr lst) (- n 1)))
		(cons (cons (car lst) (car split)) (cdr split)))))
)


(define-macro (switch expr cases)
	(cons 'cond
		(map (lambda (case) (cons `(eq? ,expr ',(car case)) (cdr case))) ; (car case) should be a symbol
    			cases))
)

