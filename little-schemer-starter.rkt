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

(define insertL
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      (else (cond
             ((eq? (car xs) old) (cons new (cons old (cdr xs))))
             (else (cons (car xs) (insertL new old (cdr xs)))))))))




