(let ((x (box 5)))
    (let ((my-ref (! x)))
        (begin
            (:= my-ref 6)
            (@ my-ref))))

; (list (:= my-ref 6) (@ my-ref))
; (cons (:= my-ref 6)(A) (cons (@ my-ref)(B) []))
; foldl (fun (fun Num0 (A)) (B))
; (interp (interp A env) B env)
; so the way to fix this is to have them share the same enviroment
; 198 in main.ml
; we need to change implementation of begin to something else instead of this 
; | Exp.Begin b -> List.fold_left (fun _ e -> interp e env) (Value.Num 0) b

; propose change (pseudo code)
#; (
    begin (list) (env)
        val, changed-env = interp (fst list) env in
            match list with
                | fst :: rst -> begin (rst list) changed-env
                | fst :: []  -> val
                | _ -> impossible
)

; or possible working foldl with interp changed to return env
; | Exp.Begin b -> List.fold_left (fun (_, e) exp -> interp exp e) ((Value.Num 0), env) b
; then in Exp.LET
; we need to check if rhs (the value we are binding with) are of type Box (owned value) if it is,
; remove it from the current env and return it
; note we also need to use the updated env in the sub interp call on body
