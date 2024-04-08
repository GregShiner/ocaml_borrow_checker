(let ([x 5])
    (+ x 1))

((lambda (x) (+ x 1)) 5)

(let ([x 5])
    (begin
        (+ x 1)
        (+ x 2)))

(let ([x (box 5)])
    (begin
        (let ([y x])
            (set-box! x 6))))

(let ([x 5])
    (set! x 6))

(let [(x 5) (y 6)]
    (+ x y)
    (- x y))

(let [(x 5)]
    (let [(y 6)]
        (begin
            (+ x y)
            (- x y))))

(let-mut [(x 5)]
    (let [(f (lambda (a) (set! a (+ a 1))))] ; ERROR: a is not mutable
        (f x)
        (f x)))

(let-mut [(x 5)]
    (let [(f (lambda (!a) (set! a (+ a 1))))] ; Allowed
        (f !x)
        (f !x)))

;; ! operator creates a mutable reference to a variable
;; ! when used in a lambda formal, means that the variable that is passed, must be a mutable reference

(let-mut [(x 5)]
    (let [(f (lambda (!a) (set! a (+ a 1))))] ; Allowed
        (let [(y !x)]
            (let [(z y)]
            (f y)
            (f y))))

(let ((x (box 5)))
    (let ((my-ref (! x)))
        (set! my-ref 6)
        my-ref))

(let ((x 5))
    (let ((my-ref (! x))) ; ERROR: x is not mutable
        (set! my-ref 6)
        my-ref))

(let ((x (box 5)))
    (let ((my-ref (! x)) (my-ref2 (! x))) ; ERROR: x cannot create multiple mutable references
        (set! my-ref 6)
        (set! my-ref2 7)
        (+ my-ref my-ref2)))

(let ((x (box 5)))
    (let ((my-ref (! x)) (my-ref2 (& x))) ; ERROR: x cannot immutably borrow and mutably borrow at the same time
        (set! my-ref 6)
        (set! my-ref2 7)
        (+ my-ref my-ref2)))

(let ((x (box 5)))
    (set! (! x) 6))

(@ (box 5)) ; 5
(@ (! (box 5))) ; 5
(let ((x 5))
    (let ((my-ref (& x)))
        (@ my-ref)) ; 5
(@ (& (box 5))) ; 5

(@ (& (& 5))) ; (& 5)

; using gregs bad garbage collection
(let ((x (box 5)))
    (let ((my-ref (! x)))
        (set! my-ref 6)) ; all references to x are deallocated, x is deallocated
    (@ x)) ; HOLY SHIT DEREFERENCED A NULL POINTER

; When a reference goes out of scope, check if there are any other references to the same variable
; If there are no other references, then the variable is deallocated
