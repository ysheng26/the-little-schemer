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


;; chapter five summary
;; rember*
;; insertR*
;; occur*
;; subst*
;; insertL*
;; member*
;; leftmost
;; eqlist?
;; equal?
;; equal? using eqlist? and eqlist? using equal?
;; rember using equal? to remove member of s-expressions


;; chater six summary
;; set?
;; makeset, makeset2
;; subset?
;; eqset?
;; intersect?
;; intersect
;; union
;; setdiff
;; intersectall
;; a-pair?
;; first
;; second
;; build
;; third
;; fun?
;; revrel
;; fullfun?
;; seconds
;; one-to-one?


;; chater six summary
;; rember-f


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (lambda (xss)
    (cond
      ((null? xss) '())
      (else (cons (car (car xss)) (firsts (cdr xss)))))))


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


(define rember*
  (lambda (x xs)
    (cond
      ((null? xs) '())
      ((atom? (car xs))
       (cond
         ((eq? x (car xs)) (rember* x (cdr xs)))
         (else (cons (car xs) (rember* x (cdr xs))))))
      (else (cons (rember* x (car xs)) (rember* x (cdr xs)))))))


(define insertR*
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      ((atom? (car xs))
       (cond
         ((eq? (car xs) old)
          (cons old (cons new (insertR* new old (cdr xs)))))
         (else
          (cons (car xs) (insertR* new old (cdr xs))))))
      (else
       (cons (insertR* new old (car xs)) (insertR* new old (cdr xs)))))))


(define occur*
  (lambda (x xs)
    (cond
      ((null? xs) 0)
      ((atom? (car xs))
       (cond
         ((eq? (car xs) x) (add1 (occur* x (cdr xs))))
         (else (occur* x (cdr xs)))))
      (else
       (plus (occur* x (car xs)) (occur* x (cdr xs)))))))


(define subst*
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      ((atom? (car xs))
       (cond
         ((eq? (car xs) old) (cons new (subst* new old (cdr xs))))
         (else (cons (car xs) (subst* new old (cdr xs))))))
      (else
       (cons (subst* new old (car xs)) (subst* new old (cdr xs)))))))


(define insertL*
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      ((atom? (car xs))
       (cond
         ((eq? (car xs) old) (cons new (cons old (insertL* new old (cdr xs)))))
         (else (cons (car xs) (insertL* new old (cdr xs))))))
      (else
       (cons (insertL* new old (car xs)) (insertL* new old (cdr xs)))))))


(define mymember*
  (lambda (x xs)
    (cond
      ((null? xs) #f)
      ((atom? (car xs))
       (cond
         ((eq? (car xs) x) #t)
         (else (mymember* x (cdr xs)))))
      (else
       (or (mymember* x (car xs)) (mymember* x (cdr xs)))))))


(define member*
  (lambda (x xs)
    (cond
      ((null? xs) #f)
      ((atom? (car xs))
       (or (eq? (car xs) x) (member* x (cdr xs))))
      (else
       (or (member* x (car xs)) (member* x (cdr xs)))))))

;; (define xs '((potato) (chips ((with) fish) (chips))))
;; (displayln (member* 'chips xs))

;; The function leftmost finds the leftmost
;; atom in a non-empty list of S-expressions
;; that does not contain the empty list.
(define leftmost
  (lambda (xs)
    (cond
      ((atom? (car xs)) (car xs))
      (else (leftmost (car xs))))))

;; xs, ys
;; empty, empty
;; empty, atom
;; empty, non atom
;; atom, empty
;; atom, atom
;; atom, non atom
;; non atom, empty
;; non atom, atom
;; non atom, non atom
(define eqlistdetailed?
  (lambda (xs ys)
    (cond
      ;; empty, emtpy
      ((and (null? xs) (null? ys)) #t)
      ;; empty, atom
      ((and (null? xs) (atom? (car ys))) #f)
      ;; empty, non atom
      ((null? xs) #f)
      ;; atom, empty
      ((and (atom? (car xs)) (null? ys)) #f)
      ;; atom, atom
      ((and (atom? (car xs)) (atom? (car ys)))
       (and (eqan? (car xs) (car ys)) (eqlistdetailed? (cdr xs) (cdr ys))))
      ;; atom, non atom
      ((atom? (car xs)) #f)
      ;; non atom, empty
      ((null? ys) #f)
      ;; non atom, atom
      ((atom? ys) #f)
      ;; non atom, non atom
      (else
       (and (eqlistdetailed? (car xs) (car ys)) (eqlistdetailed? (cdr xs) (cdr ys)))))))

;; xs, ys
;; empty, empty
;; empty, atom
;; empty, non atom
;; atom, empty
;; atom, atom
;; atom, non atom
;; non atom, empty
;; non atom, atom
;; non atom, non atom
(define eqlist?
  (lambda (xs ys)
    (cond
      ;; empty, empty
      ((and (null? xs) (null? ys)) #t)
      ;; empty, atom
      ;; empty, non atom
      ;; atom, empty
      ;; non atom, empty
      ((or (null? xs) (null? ys)) #f)
      ;; atom, atom
      ((and (atom? (car xs)) (atom? (car ys)))
       (and (eqan? (car xs) (car ys)) (eqlist? (cdr xs) (cdr ys))))
      ;; atom, non atom
      ;; non atom, atom
      ;; NOTE: the (atom, empty) and (empty, atom) was covered above
      ((or (atom? (car xs)) (atom? (car ys))) #f)
      (else
       (and (eqlist? (car xs) (car ys)) (eqlist? (cdr xs) (cdr ys)))))))

;; atom or list
;; x, y
;; atom, atom
;; atom, list
;; list, atom
;; list, list
(define equal?
  (lambda (x y)
    (cond
      ((and (atom? x) (atom? y)) (eqan? x y))
      ((or (atom? x) (atom? y)) #f)
      (else (eqlist2? x y)))))
    

(define eqlist2?
  (lambda (xs ys)
    (cond
      ((and (null? xs) (null? ys)) #t)
      ((or (null? xs) (null? ys)) #f)
      (else
       (and (equal? (car xs) (car ys))
            (eqlist2? (cdr xs) (cdr ys)))))))


(define rember_s
  (lambda (s l)
    (cond
      ((null? l) '())
      (else
       (cond
         ((equal? (car l) s) (cdr l))
         (else (cons (car l) (rember_s s (cdr l)))))))))

         
 
(define rember_s2
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember_s2 s (cdr l)))))))


;; the book used symbols but I used names
;; plus
;; minus
;; mul
;; pow
;; div

;; numbered? is a function that determines whether a
;; representation of an arithmetic expression
;; contains only numbers besides the plus, mul and pow
;; a plus b
;; a mul b
;; a pow b

(define numbereddetailed?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) 'plus)
       (and (numbereddetailed? (car aexp))
            (numbereddetailed? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'mul)
       (and (numbereddetailed? (car aexp))
            (numbereddetailed? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'pow)
       (and (numbereddetailed? (car aexp))
            (numbereddetailed? (car (cdr (cdr aexp))))))
      (else #f))))

;; (display (numbereddetailed? '(2 pow ((2 mul 3) plus 2))))

;; this doesn't check the operator though
;; the book says
;;   Since aexp was already understood to be an
;;   arithmetic expression, could we have written
;;   numbered? in a simpler way?
;; I guess it's a given
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))



(define myvalue
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((and (numbered? (car nexp))
            (numbered? (car (cdr (cdr nexp)))))
       (cond
         ((eq? (car (cdr nexp)) 'plus)
          (plus (myvalue (car nexp)) (myvalue (car (cdr (cdr nexp))))))
         ((eq? (car (cdr nexp)) 'mul)
          (mul (myvalue (car nexp)) (myvalue (car (cdr (cdr nexp))))))
         ((eq? (car (cdr nexp)) 'pow)
          (pow (myvalue (car nexp)) (myvalue (car (cdr (cdr nexp)))))))))))
         

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) 'plus)
       (plus (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'mul)
       (mul (value (car nexp)) (value (car (cdr (cdr nexp))))))
      (else
       (pow (value (car nexp)) (value (car (cdr (cdr nexp)))))))))
         

;; (+ 1 2)
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))


(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) 'plus)
       (plus (value2 (1st-sub-exp nexp)) (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'mul)
       (mul (value2 (1st-sub-exp nexp)) (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'pow)
       (pow (value2 (1st-sub-exp nexp)) (value2 (2nd-sub-exp nexp)))))))


(define set?
  (lambda (xs)
    (cond
      ((null? xs) #t)
      ((member? (car xs) (cdr xs)) #f)
      (else (set? (cdr xs))))))


;; xs is '(a b a a)
(define makeset
  (lambda (xs)
    (cond
      ((null? xs) '())
      ((member? (car xs) (cdr xs)) (makeset (cdr xs)))
      (else (cons (car xs) (makeset (cdr xs)))))))


;; write makeset with multirember
(define makeset2
  (lambda (xs)
    (cond
      ((null? xs) '())
      (else
       (cons (car xs)
             (makeset2 (multirember (car xs) (cdr xs))))))))



(define subset?
  (lambda (xs ys)
    (cond
      ((null? xs) #t)
      (else
       (cond
         ((member? (car xs) ys) (subset? (cdr xs) ys))
         (else #f))))))


(define subset?_shorter
  (lambda (xs ys)
    (cond
      ((null? xs) #t)
      ((member? (car xs) ys) (subset?_shorter (cdr xs) ys))
      (else #f))))
       


(define subset?_withand
  (lambda (xs ys)
    (cond
      ((null? xs) #t)
      (else
       (and (member? (car xs) ys) (subset?_withand (cdr xs) ys))))))



(define myeqset?
  (lambda (xs ys)
    (cond
      ((and (null? xs) (null? ys)) #t)
      ((null? xs) #f)
      ((null? ys) #f)
      (else
       (and
        (member? (car xs) ys)
        (myeqset? (cdr xs) (rember (car xs) ys)))))))


;; (eqset? '(b a b) '(b a)) returns #t
;; but I guess this is because the params are supposed to be sets
(define eqset?
  (lambda (xs ys)
    (cond
      ((and (subset? xs ys)
            (subset? ys xs))))))



(define intersect?
  (lambda (xs ys)
    (cond
      ((null? xs) #t)
      (else
       (or (member? (car xs) ys) (intersect? (cdr xs) ys))))))


(define intersect
  (lambda (xs ys)
    (cond
      ((null? xs) '())
      ((member? (car xs) ys) (cons (car xs) (intersect (cdr xs) ys)))
      (else (intersect (cdr xs) ys)))))


(define union
  (lambda (xs ys)
    (cond
      ((null? xs) ys)
      ((member? (car xs) ys) (union (cdr xs) ys))
      (else
       (cons (car xs) (union (cdr xs) ys))))))


;; returns elements in xs but not in ys
(define setdiff
  (lambda (xs ys)
    (cond
      ((null? xs) '())
      ((member? (car xs) ys) (setdiff (cdr xs) ys))
      (else
       (cons (car xs) (setdiff (cdr xs) ys))))))


(define intersectall
  (lambda (xss)
    (cond
      ((null? (cdr xss)) (car xss)) ;; if last set then return itself
      (else
       (intersect (car xss) (intersectall (cdr xss)))))))

;; (displayln (intersectall '((a b) (b c) (b d))))


(define a-pair?
  (lambda (xs)
    (cond
      ((atom? xs) #f)
      ((null? xs) #f)
      ((null? (cdr xs) #f))
      ((null? (cdr (cdr xs)) #f))
      (else #t))))


(define first
  (lambda (p)
    (cond
      (else (car p)))))


(define second
  (lambda (p)
    (cond
      (else (car (cdr p))))))


(define build
  (lambda (x y)
    (cond
      (else (cons x (cons y '()))))))


(define third
  (lambda (xs)
    (cond
      (else (car (cdr (cdr xs)))))))


;; a relation is a set of pairs
;; a fun is a function
;; gemini: A relation in which the first element of each pair is unique.
;; gemini: fun stands for finite function

;; write fun? with set? and firsts
(define fun?
  (lambda (rel)
    (set? (firsts rel))))
;; (displayln (fun? '((2 1) (2 3))))

;; revrel reverse pairs in the relation
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (build (second (car rel)) (first (car rel)))
             (revrel (cdr rel)))))))

;; (displayln (revrel '((1 2) (2 3) (3 4))))

(define revpair
  (lambda (p)
    (cond
      (else (build (second p) (first p))))))


(define revrel2
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (revpair (car rel)) (revrel2 (cdr rel)))))))
;; (displayln (revrel2 '((1 2) (2 3) (3 4))))


;; gemini: A fullfun is a fun where the second element of every pair is unique.
(define myfullfun?
  (lambda (fun)
    (and (fun? fun) (fun? (revrel fun)))))

;; (displayln (myfullfun? '((1 2) (2 3))))
;; (displayln (myfullfun? '((1 2) (3 2))))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (cons (second (car rel)) (seconds (cdr rel))))))

;; don't really need to check if it's a fun or not
;; if it's not a fullfun it can't be a fun
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

;; (displayln (myfullfun? '((1 2) (3 2))))

;; another name of fullfun? is one-to-one?
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))


(define rember-f
  (lambda (test? x xs)
    (cond
      ((null? xs) '())
      ((test? x (car xs)) (cdr xs)) ;; no recursion here means it stops after first remove
      (else (cons (car xs) (rember-f test? x (cdr xs)))))))

;; (displayln (rember-f equal? '(1 2) '(1 2 3 (1 2) 4)))
;; (displayln (rember-f equal? '1 '(1 2 3 (1 2) 4)))


(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))


(define eq?-salad (eq?-c 'salad))

((eq?-c 'salad) 'tuna)


(define rember-f2
  (lambda (test?)
    (lambda (x xs)
      (cond
        ((null? xs) '())
        ((test? (car xs) x) (cdr xs))
        (else (cons (car xs) ((rember-f2 test?) x (cdr xs))))))))

;; (displayln ((rember-f2 eq?) 'a '(b a c)))


(define insertL-f
  (lambda (test?)
    (lambda (new old xs)
      (cond
        ((null? xs) '())
        ((test? (car xs) old) (cons new (cons old (cdr xs))))
        (else (cons old ((insertL-f test?) new old (cdr xs))))))))

;; (displayln ((insertL-f eq?) 'z 'a '(b a c)))

(define insertR-f
  (lambda (test?)
    (lambda (new old xs)
      (cond
        ((null? xs) '())
        ((test? (car xs) old) (cons old (cons new (cdr xs))))
        (else (cons (car xs) ((insertR-f test?) new old (cdr xs))))))))

;; (displayln ((insertR-f eq?) 'z 'b '(a b c)))


(define my-insert-g
  (lambda (test?)
    (lambda (left-or-right)
      (lambda (new old xs)
        (cond
          ((null? xs) '())
          ((test? (car xs) old)
           (cond
             ((eq? left-or-right 'left)
              (cons new (cons old (cdr xs))))
             (else ;; right
              (cons old (cons new (cdr xs))))))
          (else
           (cons (car xs)
                 (((my-insert-g test?) left-or-right) new old (cdr xs)))))))))


(define seqL
  (lambda (new old xs)
    (cons new (cons old xs))))


(define seqR
  (lambda (new old xs)
    (cons old (cons new xs))))


(define insert-g
  (lambda (seq)
    (lambda (new old xs)
      (cond
        ((null? xs) '())
        ((eq? (car xs) old) (seq new old (cdr xs)))
        (else (cons (car xs) ((insert-g seq) new old (cdr xs))))))))

;; (displayln ((insert-g seqL) 'z 'a '(d b c a)))
;; (displayln ((insert-g seqR) 'z 'a '(d b c a)))

(define insertL2 (insert-g seqL))
(define insertR2 (insert-g seqR))

(define insertL3
  (insert-g (lambda (new old xs)
              (cons new old xs))))

(define insertR3
  (insert-g (lambda (new old xs)
              (cons old new xs))))


(define subst3
  (lambda (new old xs)
    (cond
      ((null? xs) '())
      ((eq? (car xs) old) (cons new (cdr xs)))
      (else (cons (car xs) (subst3 new old (cdr xs)))))))


(define seqS ;; S for substitute
  (lambda (new old xs)
    (cons new xs)))

(define subst4 (insert-g seqS))


(define seqrem
  (lambda (new old xs) xs))

(define yyy
  (lambda (x xs)
    ((insert-g seqrem) #f x xs)))

;; #f is the placeholder for unused args


(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x 'plus) plus)
      ((eq? x 'mul) mul)
      (else pow))))

;; (displayln (operator '(plus 5 3)))
;; (displayln (atom-to-function (operator '(plus 5 3))))



(define value3
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value3 (1st-sub-exp nexp)) (value3 (2nd-sub-exp nexp)))))))

;; (displayln (value3 '(plus (mul 2 3) (pow 2 3))))


(define multirember-f
  (lambda (test?)
    (lambda (a xs)
      (cond
        ((null? xs) '())
        ((test? (car xs) a) ((multirember-f test?) a (cdr xs)))
        (else (cons (car xs) ((multirember-f test?) a (cdr xs))))))))

;; (displayln ((multirember-f eq?) 'x '(a x b x x c d x x)))


;; (define eq?-x (lambda (x) (eq? x 'x)))
(define eq?-x (eq?-c 'x))


(define multiremberT
  (lambda (test? xs)
    (cond
      ((null? xs) '())
      ((test? (car xs)) (multiremberT test? (cdr xs)))
      (else (cons (car xs) (multiremberT test? (cdr xs)))))))

;; (displayln (multiremberT eq?-x '(a x b x x c d x x)))


(define a-friend
  (lambda (x y)
    (null? y)))

;; page 138
;; not crystal clear

;; It looks at every atom of the lat to see
;; whether it is eq? to a. Those atoms that are
;; not are collected in one list ls1; the others
;; for which the answer is true are collected in a
;; second list ls2 . Finally, it determines the
;; value of (f ls1 ls2).
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

(displayln (multirember&co 'tuna '() a-friend))
(displayln (multirember&co 'tuna '(tuna) a-friend))

(define last-friend
  (lambda (x y)
    (length x)))

;; (last-friend '(strawberries and swordfish) '(tuna))
;;
;; (last-friend '() '())
;;    (last-friend '(strawberries and swordfish) '(tuna))
(displayln
 (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend))
           


(define multiinsertLR
  (lambda (new oldL oldR xs)
    (cond
      ((null? xs) '())
      ((eq? (car xs) oldL)
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr xs)))))
      ((eq? (car xs) oldR)
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr xs)))))
      (else
       (cons (car xs) (multiinsertLR new oldL oldR (cdr xs)))))))


;; base case is the trigger, calls col with base case
;; newlat is the list prepared by one level below and passed to current col (which is you)
;; do what you need to do at this level and call col with your result
;; final result comes out from tier-0 collector
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat) L R)))))))

(displayln
 (multiinsertLR&co 'x 'a 'b '(c c a c b a)
                   (lambda (newlat L R) newlat)))













