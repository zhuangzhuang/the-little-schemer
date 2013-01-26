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
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (o+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2) '(3 4 5))

(define >
  (lambda (n m)
    (cond ((zero? n) #f) ;注意下顺序解决 2 > 2 -> #t 的问题
          ((zero? m) #t)
          (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond ((zero? m) (zero? n))
          ((zero? m) #f)
          (else (= (sub1 m) (sub1 n))))))
(define =
  (lambda (n m)
    (cond ((> n m) #f)
          ((< n m) #f)
          (else #t))))

(define exp
  (lambda (n m)
    (cond ((zero? m) 1)
          (o* (exp n (sub1 m))))))

;书上这边应该有个小错误  16 / 4 = 4 而按照书上 16 / 4 = 3 
(define /
  (lambda (n m)
    (cond ((< n m) 0) ;这里有问题
          (else (add1 (/ (o- n m) m))))))

(define length
  (lambda (lat)
    (cond ((null? lat) 0)
          (else (add1 (length (cdr lat)))))))

(define pick 
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(pick 1 '(12 34))
(pick 2 '(12 34))


(define rempick 
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (cons (car lat)
                      (rempick (sub1 n) (cdr lat)))))))
(define no-nums
  (lambda (lat)
    (cond ((null? lat) '())
          ((number? (car lat)) (no-nums (cdr lat)))
          (else (cons (car lat)
                      (no-nums (cdr lat)))))))
(no-nums '(9 9 abc 2 af))


(define all-nums
  (lambda (lat)
    (cond ((null? lat) '())
          ((number? (car lat)) (cons (car lat) 
                                     (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))

(all-nums '(9 9 abc 2 af))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1)
               (number? a2)) (= a1 a2))
          ((or (number? a1) 
               (number? a2)) #f)
          (else (eq? a1 a2)))))
 
(eqan? 12 34)
(eqan? 12 'a)
(eqan? 'a 'a)
(eqan? 'a 'b)

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eq? a (car lat)) (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))

(occur 'a '(a b c a d))

(define one?
  (lambda (n)
    (cond ((zero? n) #f)  ;不知道这句有个鸟用
          (else (zero? (sub1 n))))))

(define one?
  (lambda (n)
    (cond
      (else (= n 1)))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond ((one? n) (cdr lat))
          (else (cons (car lat)
                      (rempick (sub1 n)
                               (cdr lat)))))))
(rempick 3 '(1 2 3 4 5))

