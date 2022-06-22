(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (x) 
  (if (null? x)
    (list first)
    (append (list first) x)
  )) rests)
)

(define (zip pairs)
  (define (helper pairs first second)
    (if (null? pairs)
      (list first second)
      (helper (cdr pairs) (append first (list (caar pairs))) (append second (list (car (cdar pairs)))))))
  (helper pairs nil nil)
)

;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (helper s i)
    (if (null? s)
      ()
      (cons (list i (car s)) (helper (cdr s) (+ i 1)))))
  (helper s 0)  
)
  ; END PROBLEM 16

;; Problem 17
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 17
  (if (= total 0)
    '(()) ; (()) is a successful base case
    (if (eq? denoms nil)
      () ; () means return nothing
      (if (< total 0)
        ()
        (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
            (list-change total (cdr denoms))))))
)
  ; END PROBLEM 17

;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
          ;  (print form params (car body))
          (cons form (cons params (map let-to-lambda body))) ;the body is cdr which has one more parentheses than params
          ; (cons car cdr) == (append (list car) cdr)
          ; (cons form (cons params (map let-to-lambda expr)))
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (define tmp (zip values))
          ;  (append (cons (cons 'lambda (cons (car tmp) (map let-to-lambda body))) nil) (map let-to-lambda (cadr tmp)))
          (append `(,(cons 'lambda (cons (car tmp) (map let-to-lambda body)))) (map let-to-lambda (cadr tmp)))
           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
         (define first (car expr))
         (if (procedure? first)
          (cons (car expr) (map let-to-lambda (cdr expr)))
          (map let-to-lambda expr))
         ; END PROBLEM 18
         )))

(define let-to-lambda-code
  '(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
          ;  (print form params (car body))
          (cons form (cons params (map let-to-lambda body))) ;the body is cdr which has one more parentheses than params
          ; (cons car cdr) == (append (list car) cdr)
          ; (cons form (cons params (map let-to-lambda expr)))
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (define tmp (zip values))
           (append (cons (cons 'lambda (cons (car tmp) (map let-to-lambda body))) nil) (map let-to-lambda (cadr tmp)))
          ; (append `(,(cons 'lambda (cons (car tmp) (map let-to-lambda body)))) (map let-to-lambda (cadr tmp)))
           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
         (define first (car expr))
         (if (procedure? first)
          (cons (car expr) (map let-to-lambda (cdr expr)))
          (map let-to-lambda expr))
         ; END PROBLEM 18
         )))
)
(define let-to-lambda-without-let
  (let-to-lambda let-to-lambda-code))