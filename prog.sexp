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

(let-mut [(f (lambda (a) (+ 1 a)))]

