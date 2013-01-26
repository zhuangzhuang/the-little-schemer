(define atom?
	(lambda (x)
          (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

;我的
(define o+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (o+ (add1 n) (sub1 m))))))

(define o-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (o- (sub1 n) (sub1 m))))))
;书上的
(define o+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (o+ n (sub1 m)))))))
(define o-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (o+ (car tup) (addtup (cdr tup)))))))

(addtup '(1 2 3))

(define o*
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (o+ n (o* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond ((and (null? tup2)(null? tup1)) '())
          (else (cons (o+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))





