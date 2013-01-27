(define atom?
	(lambda (x)
          (and (not (pair? x)) (not (null? x)))))

(define numered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+) (and (numberd? (car aexp))
                                      (numberd? (car (cdr (cdr (aexp)))))))
      ((eq? (car (cdr aexp)) '*) (and (numberd? (car aexp))
                                      (numberd? (car (cdr (cdr (aexp)))))))
      ((eq? (car (cdr aexp)) '^) (and (numberd? (car aexp))
                                      (numberd? (car (cdr (cdr (aexp))))))))))
      
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp)(number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))


(numbered? '(12 + (23 ^ (23 + a)))) 


(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*)
       (* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      (else (expt (value (car nexp))
                  (value (car (cdr (cdr nexp)))))))))

(value '(23 + (2 ^ 5)))

;有误
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car  nexp) '+)
       (+ (value (cdr nexp))
          (value (cdr (cdr nexp)))))
      ((eq? (car  nexp) '*)
       (* (value (cdr nexp))
          (value (cdr (cdr nexp)))))
      (else (expt (value (cdr nexp))
                  (value (cdr (cdr nexp))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '*)
       (* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      (else 
       (expt (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))

(value '(+ 34 (* 2 (^ 4 3))))

;1st-sub-exp 和 operator 定义改下就是第一种

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons ('() n))))

(define zub1
  (lambda (n)
    (cdr n)))
(define op+
  (lambda (n m)
    ((sero? m) n)
    (else (edd1
           (op+ n (zub1 m))))))

(define lat?
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


















