(define atom?
	(lambda (x)
          (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat))
                    (member? a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? a (car lat)) (multirember a (cdr lat)))
          (else (cons (car lat)
                      (multirember a (cdr lat)))))))


(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat))
       #f)
      (else (set? (cdr lat))))))

(set? '(a b c))

(set? '(a b a))


(define makeset
  (lambda (lat)
    (cond 
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else
       (cons (car lat)
             (makeset (cdr lat)))))))

(makeset '(a b c a c b))
             
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
                  (makeset
                   (multirember (car lat)
                                (cdr lat))))))))

(makeset '(a b c a c b))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (cond
         ((member? (car set1) set2)
          (subset? (cdr set1) set2))
         (else #f))))))


(subset? '(a b) '(c b a))
          
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
          (subset? (cdr set1) set2))
      (else #f))))

(subset? '(a b) '(c  a))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))



(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) #f)
          ((member? (car set1) set2) #t)
          (else
           (intersect? (cdr set1) set2)))))

(define interset
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2) 
           (cons (car set1)
                 (interset (cdr set1) set2)))
          (else
           (interset (car set1) set2)))))


(define union
  (lambda (set1 set2)
    (cond 
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
       (cons (car set1)
             (union (cdr set1) set2))))))
;set1 - set2
(define xxx
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (xxx (cdr set1) set2))
      (else
       (cons (car set1)
             (xxx (cdr set1) set2))))))

(xxx '(a b c) '(c d))

(define insersectall 
  (lambda (l-set)
    (cond 
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (insersectall (cdr l-set)))))))
            

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 '()))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cons (car (car l))
             (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cons (car (cdr (car l)))
             (seconds (cdr l)))))))


(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build
                   (second (car rel))
                   (first (car rel)))
                  (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))
;one-to-one?
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one
  (lambda (fun)
    (fun? (revrel fun))))













