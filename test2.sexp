(let ((x 5))
    (let ((my-ref (! x)))
        (let ((my-ref2 (& x))) ; this should throw an error
            (begin
                (:= my-ref 6)
                (@ my-ref2)))))

