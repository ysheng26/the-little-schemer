; (define Y
;   (lambda (f)
;     ((lambda (g) (g g))
;       (lambda (g)
;         (f (lambda (x) ((g g) x)))))))



;; f is a function that takes in function "length" and returns the function which is the logic
(define mk-length
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; no delay version, this won't work in scheme but its easier to understand
;; the (lamdba (x) ((g g) x)) is to delay the evaluation of g

(define Y
  (lambda (f)
    ((lambda (g) (g g))
     (lambda (g) (f (g g))))))

;; (Y mk-length)
;; replace f with mk-length
((lambda (g) (g g))
 (lambda (g) (mk-length (g g))))

;; g is
(lambda (g) (mk-length (g g)))

;; replace g
((lambda (g) (mk-length (g g)))
 (lambda (g) (mk-length (g g))))

;; replace g again
(mk-length
 ((lambda (g) (mk-length (g g)))
  (lambda (g) (mk-length (g g)))))

;; replace g again and again
(mk-length
 (mk-length ((lambda (g) (mk-length (g g)))
             (lambda (g) (mk-length (g g))))))

;; replace g again and again and again
(mk-length
 (mk-length
  (mk-length ((lambda (g) (mk-length (g g)))
              (lambda (g) (mk-length (g g)))))))

;; replace g again and again and again and again
(mk-length
 (mk-length
  (mk-length
   (mk-length ((lambda (g) (mk-length (g g)))
               (lambda (g) (mk-length (g g))))))))
