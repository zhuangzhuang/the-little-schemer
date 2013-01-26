(define atom?
	(lambda (x)
          (and (not (pair? x)) (not (null? x)))))

(define rember
  (lambda (a lat)
    (cond ((null? a) '())
          (else (cond ((eq? (car lat) a) (cdr lat))
                      (else (cons (car lat) (rember a (cdr lat)))))))))
(define rember
  (lambda (a lat)
    (cond ((null? a) '())
          ((eq? (car lat) a) (cdr lat))
          (else (cons (car lat)
                      (rember a (cdr lat)))))))


(rember 'a '(a b c))

(define firsts
  (lambda (ls)
    (cond ((null? ls) '())
          (else (cons (car (car ls)) 
                     (firsts (cdr ls)))))))
(firsts '((1 2) (3 4) (5 6)))

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons old
                                     (cons new (cdr lat))))
          (else (cons (car lat)
                      (insertR new old (cdr lat)))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons new  lat))
          (else (cons (car lat) (insertL  new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons new (cdr lat)))
          (else (cons (car lat) (subst  new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) '())
          ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
          (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))
;删除多个
(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? a (car lat)) (multirember a (cdr lat)))
          (else (cons (car lat)
                      (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons old
                                     (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons new  (cons (car lat) (multiinsertL new old (cdr lat))))
          (else (cons (car lat) (multiinsertL  new old (cdr lat)))))))
  
(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst  new old (cdr lat)))))))


