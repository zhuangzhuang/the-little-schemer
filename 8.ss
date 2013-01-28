(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     (else
      (cond
       ((test? (car l) a) (cdr l))
       (else
	(cons (car l)
	      (rember-f test? a
			(cdr l)))))))))


(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else
      (cons (car l)
	    (rember-f test? a (cdr l)))))))

(rember-f eq? 'a '(a b c))

(rember-f = 1 '(1 2 3))


(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

((eq?-c 'salad) 'salad)

(define eq?-salad (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else
	(cons (car l)
	      ((rember-f test?) a
	       (cdr l))))))))

((rember-f =) 1 '(1 2 3))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
	(seq new old (cdr l)))
       (else (cons (car l)
		   ((insert-g seq) new old
		    (cdr l))))))))

(define atom?
	(lambda (x)
          (and (not (pair? x)) (not (null? x)))))
    

(define insertL
  (insert-g seqL))

(insertL 'a 'b '(b c d))

(define insertR
  (insert-g seqR))

(insertR 'a 'b '(b c d))


(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))



(define atom-to-function
  (lambda (x)
    (cond 
     ((eq? x '+) +)
     ((eq? x '*) *)
     (else expt))))

(define operator
  (lambda (l)
    (car l)))

(atom-to-function (operator '(+ 5 3)))

(define 1st-sub-exp
  (lambda (l)
    (car (cdr l))))
(define 2nd-sub-exp
  (lambda (l)
    (car (cdr (cdr l)))))

(define value
  (lambda (nexp)
     (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function
	 (operator nexp))
	(value (1st-sub-exp nexp))
	(value (2nd-sub-exp nexp)))))))

(value '(+ 2 (* 3 6)))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? a (car lat))
	((multirember-f test?) a
	 (cdr lat)))
       (else
	(cons (car lat)
	      ((multirember-f test?) a
	       (cdr lat))))))))

((multirember-f =) 1 '(1 2 3 ))



(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else (cons (car lat)
		 (multiremberT test?
			       (cdr lat)))))))

(multiremberT eq?-tuna '(s x a tuna x))

;看着就恐怖
(define multirember-co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multirember-co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col newlat
			     (cons (car lat) seen)))))
     (else
      (multirember-co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col (cons (car lat) newlat)
			     seen))))))) 

(define a-friend
  (lambda (x y)
    (null? y)))

(multirember-co 'tuna '(strawberries tuna and swordfish) a-friend)


(define new-friend
  (lambda (newlat seen)
    (col newlat
	 (cons (car lat) seen))))


(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
	    (cons oldL
		  (multiinsertLR new oldL oldR
				 (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
	    (cons new
		  (multiinsertLR new oldL oldR
				 (cdr lat)))))
     (else
      (cons (car lat)
	    (multiinsertLR new oldL oldR
			   (cdr lat)))))))

(define multiinsertLR-co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR-co new oldL oldR
		       (cdr lat)
		       (lambda (newlat L R)
			 (col (cons (car lat) newlat) (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR-co new oldL oldR
			(cdr lat)
			(lambda (newlat L R)
			  (col (cons oldR (cons new newlat))
			       L (add1 R)))))
     (else
      (multiinsertLR-co new oldL oldR
			(cdr lat)
			(lambda (newlat L R)
			  (col (cons (car lat) newlat)
			       L R)))))))
	   
(multininsertLR-co 'new 'a 'b '(a b a c d a) col) => (col newlat 3 1)

;有点问题 --?
(define even?
  (lambda (n)
    (= (* (/ n 2) 2) n))) 

(even? 3)


(define evens-only*
  (lambda (l)
    ((null? l) '())
    ((atom? (car l))
     (cond 
      ((even? (car l))
       (cons (car l)
	     (evens-only* (cdr l))))
      (else
       (evens-only* (cdr l)))))
    (else
     (cons (evens-only* (car l))
	   (evens-only* (cdr l))))))

(define evens-only*-co
  (lambda (l col)
    (cond
     ((null? l)
      (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
	(evens-only*-co (cdr l)
			(lambda (newl p s)
			  (col (cons (car l) newl)
			       (* (car l) p) s))))
       (else (evens-only*-co (cdr l)
			     (lambda (newl p s)
			       (col (cons (car l) newl)
				    p (+ (car l) s)))))))
     (else (evens-only*-co (car l)
			   ...)))))

(lambda (al ap as)
  (evens-only*-co (cdr l)
		  (lambda (dl dp ds)
		    (col (cons al dl)
			 (*ap dp)
			 (+as ds)))))
;收集 odd 的sum 和 even的product 并塞到 even list 中
(define the-last-friend
  (lambda (newl product sum)
    (cons sum
	  (cons product
		newl))))






 








		   



