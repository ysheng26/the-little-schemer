#lang r5rs
; ------------------------------------------------------------
; The Little Schemer – Starter Environment for Racket
; ------------------------------------------------------------
; Use this file in DrRacket or any Racket setup.
; Just save it as "little-schemer-starter.rkt" and hit Run.
; Then you can type examples from the book directly below.
;
; Notes:
; - 'atom?' is not part of R5RS; it's defined here.
; - 'displayln' is not in R5RS, so we define a helper for printing.
; ------------------------------------------------------------

; --- atom? ---------------------------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

; --- displayln (for convenience) -----------------------------
(define (displayln x)
  (display x)
  (newline))

; --- sample examples -----------------------------------------
; Try these to confirm it works!

;; (displayln "Testing environment...")
;; (displayln (atom? 'turkey))     ; #t
;; (displayln (atom? '(turkey)))   ; #f
;; (displayln (null? '()))         ; #t
;; (displayln (pair? '(a b c)))    ; #t

; --- Example definitions from the book -----------------------
; Define lat? (list of atoms?)
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

;; (displayln (lat? '(Jack Sprat could eat no chicken fat))) ; #t
;; (displayln (lat? '((Jack) Sprat could eat no chicken fat))) ; #f

; ------------------------------------------------------------
; From here, you can paste and experiment with examples from
; The Little Schemer, chapter by chapter.
; ------------------------------------------------------------

(quote atom)
(quote turkey)
(quote 1492)
(quote u)
'(*abc$)

;; ((quote atom)) wrong
;; quote is like "quoteing" the arg
(quote (atom))
;; or
'(atom)

;; s-expressions is an atom or a (x y) where x and y are s-expressions
;; (((how) are) ((you) (doing so)) far) 3 s-expressions
;; ((how) are)
;; ((you) (doing so))
;; far

;; () is not an atom, it is just a list

;; atom, list, car, cdr

;; (define xs '((a b c)))
;; (display (cons xs 'a))
;; why does this output (((a b c)) . a)
;; it's an in practice thing. (car (cons xs 'a)) would be xs
;; (cdr (cons xs 'a)) would be 'a


;; null? only returns true when it's an empty list
;; null? shouldn't take atoms
;; but in practice if you pass an atom to null? it's always false

;; why is (atom? null?) true?

;; eq? in the book does not take lists
;; in practice it returns true if it's the exact same list
;; (eq? '(a) '(a)) is false because these are two different lists with the same content
;;
;; where as (define xs '(a))
;; (eq? xs xs) returns true because these are the *exact* lists


;; chapter one summary
;; atom
;; list
;; s-expresssion
;; car
;; cdr
;; cons
;; null?
;; atom?
;; eq?


;; chapter two summary
;; lat?
;; member?


;; chapter three summary
;; rember
;; cons
;; firsts
;; insertR
;; insertL
;; subst
;; subst2
;; multirember
;; multiinsertR
;; multiinsertL


;; chapter four summary
;; zero?
;; add1
;; sub1
;; plus
;; minus
;; addtup
;; mul
;; tup+
;; >
;; <
;; =
;; pow
;; div
;; length
;; pick
;; rempick
;; number?
;; no-nums
;; all-nums
;; eqan?
;; occur
;; one?


;; Write the function lat? using some, but not
;; necessarily all, of the following functions:
;; car cdr cons null? atom ? and eq?
;; 
;; (define mylat?
;;     (lambda (xs)
;;       (cond
;;         ((null? xs) #t)
;;         ((atom? (car xs)) (mylat? (cdr xs)))
;;         (else #f))))


;; (define mymember?
;;   (lambda (x xs)
;;     (cond
;;       ((null? xs) #f)
;;       ((eq? x (car xs)) #t)
;;       (else (member? x (cdr xs))))))


(define member?
  (lambda (x xs)
    (cond
      ((null? xs) #f)
      (else (or (eq? x (car xs))
                (member? x (cdr xs)))))))



;; (define myrember
;;   (lambda (x xs)
;;     (cond
;;       ((null? xs) '())
;;       ((eq? (car xs) x) (cdr xs))
;;       (else (rember x (cdr xs))))))
       

(define rember
  (lambda (x xs)
    (cond
      ((null? xs) '())
      ((eq? (car xs) x) (cdr xs))
      (else (cons (car xs) (rember x (cdr xs)))))))
          

(define firsts
  (lambda (xxs)
    (cond
      ((null? xxs) '())
      (else (cons (car (car xxs)) (firsts (cdr xxs)))))))


(define insertR
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      (else (cond
              ((eq? (car xs) old) (cons old (cons new (cdr xs))))
              (else (cons (car xs) (insertR new old (cdr xs)))))))))


(define insertLOld
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      (else (cond
             ((eq? (car xs) old) (cons new (cons old (cdr xs))))
             (else (cons (car xs) (insertL new old (cdr xs)))))))))



(define insertL
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      (else (cond
             ((eq? (car xs) old) (cons new xs))
             (else (cons (car xs) (insertL new old (cdr xs)))))))))


(define subst
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      (else
       (cond
         ((eq? (car xs) old) (cons new (cdr xs)))
         (else (cons (car xs) (subst new old (cdr xs)))))))))


(define subst2
  (lambda (new o1 o2 xs)
    (cond
      ((null? xs) '())
      (else
       (cond
         ((or (eq? (car xs) o1) (eq? (car xs) o2)) (cons new (cdr xs)))
         (else
          (cons (car xs) (subst2 new o1 o2 (cdr xs)))))))))


(define multirember
  (lambda (a xs)
    (cond
      ((null? xs) '())
      (else
       (cond
         ((eq? (car xs) a) (multirember a (cdr xs)))
         (else (cons (car xs) (multirember a (cdr xs)))))))))


(define multiinsertR
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      (else
       (cond
         ((eq? (car xs) old) (cons old (cons new (multiinsertR new old (cdr xs)))))
         (else (cons (car xs) (multiinsertR new old (cdr xs)))))))))


(define multiinsertL
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      (else
       (cond
         ((eq? (car xs) old) (cons new (cons old (multiinsertL new old (cdr xs)))))
         (else (cons (car xs) (multiinsertL new old (cdr xs)))))))))


(define add1
  (lambda (x)
    (+ x 1)))


(define sub1
  (lambda (x)
    (- x 1)))


(define myplus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (myplus (add1 a) (sub1 b))))))


(define plus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (plus a (sub1 b)))))))


(define minus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (minus a (sub1 b)))))))


(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))


(define mul
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (plus m (mul m (sub1 n)))))))


(define tup+
  (lambda (xs ys)
    (cond
      ((and (null? xs) (null? ys)) '())
      ((null? ys) xs)
      ((null? xs) ys)
      (else (cons (plus (car xs) (car ys)) (tup+ (cdr xs) (cdr ys)))))))


(define >
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else (> (sub1 a) (sub1 b))))))


(define <
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else (< (sub1 a) (sub1 b))))))



(define notusingltorgt=
  (lambda (a b)
    (cond
      ((zero? a) (zero? b))
      ((zero? b) (zero? a))
      (else (= (sub1 a) (sub1 b))))))


(define my=
  (lambda (a b)
    (cond
      ((and (not (< a b)) (not (> a b))))
      (else #f))))


(define =
  (lambda (a b)
    (cond
      ((< a b) #f)
      ((> a b) #f)
      (else #t))))


(define pow
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else
       (mul x (pow x (sub1 y)))))))

(define div
  (lambda (x y)
    (cond
      ((< x y) 0)
      (else
       (add1 (div (minus x y) y))))))


(define length
  (lambda (xs)
    (cond
      ((null? xs) 0)
      (else
       (add1 (length (cdr xs)))))))
      

(define mypick
  (lambda (i xs)
    (cond
      ((eq? i 1) (car xs))
      (else
       (mypick (sub1 i) (cdr xs))))))
       

(define pick
  (lambda (i xs)
    (cond
      ((zero? (sub1 i)) (car xs))
      (else
       (pick (sub1 i) (cdr xs))))))


(define old_rempick
  (lambda (i xs)
    (cond
      ((zero? (sub1 i)) (cdr xs))
      (else
       (cons (car xs) (rempick (sub1 i) (cdr xs)))))))


(define no-nums
  (lambda (xs)
    (cond
      ((null? xs) '())
      (else
       (cond
         ((number? (car xs)) (no-nums (cdr xs)))
         (else
          (cons (car xs) (no-nums (cdr xs)))))))))


(define all-nums
  (lambda (xs)
    (cond
      ((null? xs) '())
      (else
       (cond
         ((number? (car xs)) (cons (car xs) (all-nums (cdr xs))))
         (else
          (all-nums (cdr xs))))))))


(define myeqan?
  (lambda (x y)
    (cond
      ((and (number? x) (number? y)) (= x y))
      (else (eq? x y)))))


(define eqan?
  (lambda (x y)
    (cond
      ((and (number? x) (number? y)) (= x y))
      ((or (number? x) (number? y)) #f)
      (else (eq? x y)))))


(define occur
  (lambda (x xs)
    (cond
      ((null? xs) 0)
      (else
       (cond
         ((eq? (car xs) x) (add1 (occur x (cdr xs))))
         (else (occur x (cdr xs))))))))


(define myone?
  (lambda (x)
    (cond
      ((and (number? x) (eq? x 1)) #t)
      (else #f))))


(define long_one?
  (lambda (x)
    (cond
      (else (= x 1)))))


(define one?
  (lambda (x)
    (= x 1)))



(define rempick
  (lambda (i xs)
    (cond
      ((one? i) (cdr xs))
      (else
       (cons (car xs) (rempick (sub1 i) (cdr xs)))))))























