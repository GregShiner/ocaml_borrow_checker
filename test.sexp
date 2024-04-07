(let ((x (box 5)))
    (let ((my-ref (! x)))
        (begin
            (:= my-ref 6)
            (@ my-ref))))

