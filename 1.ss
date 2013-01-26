(define atom?
	(lambda (x)
          (and (not (pair? x)) (not (null? x)))))

(list? '(atom))
(car '(a b c))

(car '((a b c) x y z ))

(cons 'a (car '((b) c d)))

(null? '(a b c))

(null? 'a)

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))
(lat? '(1 () 3))                     

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat))
                    (member? a (cdr lat)))))))














