#lang mzscheme
;; all (test-foo) calls should return zeros based on correct output on number of tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Q1
(define checker
  (lambda (xs ys)
    (cond
      ((null? xs) #t)
      ((null? ys) #f)
      ((equal? (car xs) (car ys)) (checker (cdr xs) (cdr ys)))
      (else #f)
      )))

(define before-seq
  (lambda (xs ys)
    (cond
      ((null? xs) ys)
      ((null? ys) '())
      ((checker xs (cdr ys)) (cons (car ys) (before-seq xs (cdr ys))))
      ((before-seq xs (cdr ys))))))

(define x-zero
  (lambda (e)
    (if (and (equal? (car e) '*)
             (or (and (equal? (cadr e) 'x)
                      (equal? (caddr e) 0))
                 (and (equal? (cadr e) 0)
                      (equal? (caddr e) 'x))))
        #t
        #f)))

(define test-before-seq
  (lambda ()
    (print "before-seq test case results: ")
    (print(my-test (before-seq '(a b) '(x y z a b 1 2 3 4 a b c d a a b)) '(z 4 a)))
    (print(my-test (before-seq '(a b) '(a b c d)) '()))
    (print(my-test (before-seq '() '(j k l m n)) '(j k l m n)))
    (print(my-test (before-seq '(t) '(a b t u v t t)) '(b v t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Q2
;;This should remove (* x 0) or (* 0 x) from appearing where you have (* number x) as input

(define ddx
  (λ (e)
    (if (number? e)
	0
	(if (equal? e 'x)
	    1
	    (if (equal? (car e) '+)
		(list '+
		      (ddx (cadr e))
		      (ddx (caddr e)))
                (if (equal? (car e) '*)
                    (cond ((or (x-zero (list '*
                                             (cadr e)
                                             (ddx (caddr e))))
                               (x-zero (list '*
                                             (ddx (cadr e))
                                             (caddr e))))
                           (if (x-zero (list '*
                                             (cadr e)
                                             (ddx (caddr e))))
                               (list '*
                                     (ddx (cadr e))
                                     (caddr e))
                               (list '*
                                     (cadr e)
                                     (ddx (caddr e)))))
                          (list '+
                                (list '*
                                      (cadr e)
                                      (ddx (caddr e)))
                                (list '*
                                      (ddx (cadr e))
                                      (caddr e)))
                          (error 'oops))))))))
(define test-ddx
  (lambda ()
    (print "ddx test case results: ")
    (print(my-test (ddx '(+ (* x 3) x)) '(+ (* 1 3) 1)))
    (print(my-test (ddx '(+ (* x 5) (* 1 x))) '(+ (* 1 5) (* 1 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Q3
(define cf
  (lambda (c n)
    (c= (lambda (nez)
          (if nez
              (c 1)
              (c= (lambda (ne1)
                    (if  ne1
                         (c 1)
                         (c- (lambda (nm1)
                               (cf (lambda (fnm1)
                                     (c- (lambda (nm2)
                                           (cf (lambda (fnm2)
                                                 (c+ c fnm1 fnm2))
                                               nm2))
                                         n 2))
                                   nm1))
                             n 1)))
                  n 1)))
        n 0)))

(define c-ify2(λ (f) (λ (c x y) (c (f x y)))))
(define c= (c-ify2 =))
(define c- (c-ify2 -))
(define c+ (c-ify2 +))

(define test-cf
  (lambda ()
    (print "cf test case results: ")
    (print(my-test (cf (lambda (x) x) 10) '89))
    (print(my-test (cf (lambda (x) x) 20) '10946))
    (print(my-test (cf (lambda (x) x) 30) '1346269))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Q4

;abomination
(define expand-or
 (lambda (e)
    (cond
      ((and (equal? (length e) 1)(equal? (car e) 'or)) (eval e))
      ((and (equal? (length e) 2)(equal? (car e) 'or)) (cadr e))
      ((and (equal? (length e) 3)(equal? (car e) 'or)) (cons 'cond (cons(cons(cadr e) '( => (lambda (x) x))) (list(cons 'else (cons(caddr e) null))))))
      ((and (> (length e) 2) (not (equal? (car e) 'or))) (cons (cons (car e) '( => (lambda (x) x))) (expand-or (cdr e))))
      ((equal? (length e) 2) (cons(cons(car e) '( => (lambda (x) x))) (list(cons 'else (cons(cadr e) null)))))
      (else(cons 'cond(cons (cons (cadr e) '( => (lambda (x) x))) (expand-or (cddr e))))))))

(define test-expand-or
  (lambda ()
    (print "expand-or test case results: ")
    (print(my-test (expand-or '(or ONE)) 'ONE))
    (print(my-test (expand-or '(or)) '#f))
    (print(my-test (expand-or '(or ONE TWO THREE)) '(cond (ONE => (lambda (x) x))
                                                          (TWO => (lambda (x) x))
                                                          (else THREE))))))


(define expand-and
  (lambda (e)
    (cond
      ((and (equal? (length e) 1)(equal? (car e) 'and)) (eval e))
      ((and (equal? (length e) 2)(equal? (car e) 'and)) (cadr e))
      ((equal? (length e) 4) (cons 'if  (cons(cadr e) (list (list 'if (caddr e) (cadddr e)) '#f))))
      (else (cons 'if  (cons (cadr e) (list(expand-and (cons 'and (cddr e))) '#f)))))))

(define test-expand-and
  (lambda ()
    (print "expand-and test case results: ")
    (print(my-test (expand-and '(and ONE)) 'ONE))
    (print(my-test (expand-and '(and)) '#t))
    (print(my-test (expand-and '(and ONE TWO THREE FOUR)) '(if ONE (if TWO (if THREE FOUR) #f) #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Q5
(define grovel-add
  (lambda (pred lst)
    (cond
      ((null? lst)0)
      ((atom? lst)
       (if (and (number? lst) (pred lst)) lst 0))
      (else (+ (grovel-add pred (car lst)) (grovel-add pred (cdr lst)))))))
   
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define test-grovel-add
  (lambda ()
    (print "grovel-add test case results: ")
    (print (my-test(grovel-add (λ (x) #t) '(a b (5 x y (z 2)))) '7))
    (print (my-test(grovel-add (λ (x) (< x 4)) '(a b (5 x y (z 2)))) '2))
    (print (my-test(grovel-add (λ (x) (> x 4)) '(a b (5 x y (z 2)))) '5))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Test function
  (define my-test
    (lambda (a b)
      (if (equal? a b)
          0
          1)))