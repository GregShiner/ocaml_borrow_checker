#lang plait

(define-type-alias Location Number)

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boxV [l : Location])
  (recV [ns : (Listof Symbol)]
        [vs : (Listof Value)]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (boxE [arg : Exp])
  (unboxE [arg : Exp])
  (setboxE [bx : Exp]
           [val : Exp])
  (recordE [ns : (Listof Symbol)]
           [args : (Listof Exp)])
  (rgetE [rec : Exp]
        [n : Symbol])
  (rsetE [rec : Exp]
        [n : Symbol]
        [val : Exp])
  (beginE [exps : (Listof Exp)]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  (cell [location : Location] 
        [val : Value]))

(define-type-alias Store (Listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  (v*s [v : Value] [s : Store]))

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{box ANY} s)
     (boxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{unbox ANY} s)
     (unboxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{set-box! ANY ANY} s)
     (setboxE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{begin ANY ANY ...} s)
     (beginE (map (lambda (expr) (parse expr)) (rest (s-exp->list s))))]
    [(s-exp-match? `{record {SYMBOL ANY} ...} s)
     (recordE (map (lambda (l) (s-exp->symbol (first (s-exp->list l))))
                   (rest (s-exp->list s)))
              (map (lambda (l) (parse (second (s-exp->list l))))
                   (rest (s-exp->list s))))]
    [(s-exp-match? `{get ANY SYMBOL} s)
     (rgetE (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    [(s-exp-match? `{set! ANY SYMBOL ANY} s)
     (rsetE (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test (parse `{box 0})
        (boxE (numE 0)))
  (test (parse `{unbox b})
        (unboxE (idE 'b)))
  (test (parse `{set-box! b 0})
        (setboxE (idE 'b) (numE 0)))
  #| (test (parse `{begin 1 2})
        (beginE (numE 1) (numE 2))) |#
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
    body)
  (type-case Result call
    [(v*s v-id sto-id) body]))

(define-syntax-rule
  (interp-value a env sto)
  (with [(bruh1 bruh2) (interp a env sto)] bruh1))

(define-syntax-rule
  (with-cell [(l-id v-id) sto]
    body)
  (type-case Storage sto
    [(cell l-id v-id) body]))


;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env] [sto : Store]) : Result
  (type-case Exp a
    [(numE n) (v*s (numV n) sto)]
    [(idE s) (v*s (lookup s env) sto)]
    [(plusE l r)
     (with [(v-l sto-l) (interp l env sto)]
       (with [(v-r sto-r) (interp r env sto-l)]
         (v*s (num+ v-l v-r) sto-r)))]
    [(multE l r)
     (with [(v-l sto-l) (interp l env sto)]
       (with [(v-r sto-r) (interp r env sto-l)]
         (v*s (num* v-l v-r) sto-r)))]
    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
       (interp body
               (extend-env
                (bind n v-rhs)
                env)
               sto-rhs))]
    [(lamE n body)
     (v*s (closV n body env) sto)]
    [(appE fun arg)
     (with [(v-f sto-f) (interp fun env sto)]
       (with [(v-a sto-a) (interp arg env sto-f)]
         (type-case Value v-f
           [(closV n body c-env)
            (interp body
                    (extend-env
                     (bind n v-a)
                     c-env)
                    sto-a)]
           [else (error 'interp "not a function")])))]
    [(boxE a)
     (with [(v sto-v) (interp a env sto)]
       (let ([l (new-loc sto-v)])
         (v*s (boxV l) 
              (override-store (cell l v) 
                              sto-v))))]
    [(unboxE a)
     (with [(v sto-v) (interp a env sto)]
       (type-case Value v
         [(boxV l) (v*s (fetch l sto-v) 
                        sto-v)]
         [else (error 'interp "not a box")]))]
    [(setboxE bx val)
     (with [(v-b sto-b) (interp bx env sto)]
       (with [(v-v sto-v) (interp val env sto-b)]
         (type-case Value v-b
           [(boxV l)
            (v*s v-v (update-store (cell l v-v) sto-v))]
           [else (error 'interp "not a box")])))]
    [(recordE ns as)
     (fold-record ns as env sto)]
    #| [(rgetE a n)
     (with [(v-a sto-a) (interp a env sto)]
       (v*s (type-case Value v-a
                       [(recV ns vs) (find n ns vs)]
                       [else (error 'interp "not a record")])
            sto-a))] |#
    [(rgetE a n)
     (with [(v-a sto-a) (interp a env sto)]
     (type-case Value v-a
                       ;; [(recV ns vs) (find n ns vs)]
                       [(recV ns vs) (type-case Value (find n ns vs)
                                                [(boxV l)
                                                 (v*s (fetch l sto-a) sto-a)]
                                                [else (error 'interp "record is not boxed")])]
                       [else (error 'interp "not a record")]))]
    [(rsetE a n val)
     (with [(v-a sto-a) (interp a env sto)]
       (with [(v-v sto-v) (interp val env sto-a)]
         (type-case Value v-a
           [(recV ns vs)
            (type-case  Value (find n ns vs)
                        [(boxV l)
                         (v*s v-v (update-store (cell l v-v) sto-v))]
                        [else (error 'interp "record is not boxed")])]
           [else (error 'interp "not a record")])))]
    [(beginE exprs) (begin-helper exprs env sto)]))

(define (fold-record [ns : (Listof Symbol)] [as : (Listof Exp)] [env : Env] [sto : Store]) : Result
  (cond
    [(empty? ns) (v*s (recV empty empty) sto)]
    [else (with [(v-a sto-a) (interp (first as) env sto)]
           (with [(v-r sto-r) (fold-record (rest ns) (rest as) env sto-a)]
              (v*s (recV (cons (first ns) (recV-ns v-r))
                          (cons (boxV (new-loc sto-r)) (recV-vs v-r)))
                    (update-store (cell (new-loc sto-r) v-a) sto-r))))]))

(define (begin-helper [exprs : (Listof Exp)] [env : Env] [sto : Store]) : Result
  (cond
    [(empty? (rest exprs)) (interp (first exprs) env sto)]
    [else (with [(v-f sto-f) (interp (first exprs) env sto)]
                (begin-helper (rest exprs) env sto-f))]))

(define (interp-expr [a : Exp]) : S-Exp
  (with [(v sto) (interp a mt-env mt-store)]
    (type-case Value v
      [(numV n) (number->s-exp n)]
      [(closV n body env) `function]
      [(boxV l) `box]
      [(recV ns vs) `record])))

(define (interp-seq [seq : (Listof Exp)] [env : Env] [sto : Store]) : Result
  (begin-helper seq env sto))

(define (interp-record [ns : (Listof Symbol)] [vs : (Listof Exp)] [env : Env] [sto : Store]) : Result
  (interp (recordE ns vs) env sto))


;; find ----------------------------------------

;; Takes a name and two parallel lists, returning an item from the
;; second list where the name matches the item from the first list.
(define (find [n : Symbol] [ns : (Listof Symbol)] [vs : (Listof Value)])
  : Value
  (cond
   [(empty? ns) (error 'interp "no such field")]
   [else (if (symbol=? n (first ns))
             (first vs)
             (find n (rest ns) (rest vs)))]))

(module+ test
  (test (interp (parse `2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env)
                mt-store)
        (v*s (numV 9)
             mt-store))
  (test (interp (parse `{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse `{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusE (idE 'x) (idE 'x)) mt-env)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             mt-store))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             mt-store))
  (test (interp (parse `{box 5})
                mt-env
                mt-store)
        (v*s (boxV 1)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{unbox {box 5}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  #| (test (interp (parse `{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store)))) |#
  (test (interp (parse `{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             mt-store)))

  (test (interp (parse `{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  #| (test (interp (parse `{let {[b (box 5)]}
                          {begin
                            {set-box! b 6}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store)))) |#
  (test (interp (parse `{let {[b (box 5)]}
                            {begin
                              {set-box! b 6}
                              {unbox b}}})
                  mt-env
                  mt-store)
          (v*s (numV 6)
               (override-store (cell 1 (numV 6))
                               mt-store)))
  (test (interp (parse `{let {[b {box 1}]}
                       {begin
                       {set-box! b 2}
                       {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 2)
             (override-store (cell 1 (numV 2))
                             mt-store)))
  (test/exn (interp (parse `{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse `{unbox 1}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{set-box! 1 2}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
            "free variable"))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))

;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (type-case (Listof Storage) sto
   [empty 0]
   [(cons c rst-sto) (max (cell-location c)
                          (max-address rst-sto))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (type-case (Listof Storage) sto
   [empty (error 'interp "unallocated location")]
   [(cons c rst-sto) (if (equal? l (cell-location c))
                         (cell-val c)
                         (fetch l rst-sto))]))

(define (update-store [new-cell : Storage] [sto : Store]) : Store
  (with-cell [(l v) new-cell]
    (if (empty? (filter (lambda (s) (= (cell-location s) l))
                        sto))
        (append sto (list (cell l v)))
        (map (lambda (s)
               (type-case Storage s
                          [(cell cl cv) (if (= cl l)
                                          (cell l v)
                                          (cell cl cv))]))
             sto))))

(module+ test
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)

  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (override-store (cell 2 (numV 10))
                                 (override-store (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (override-store (cell 2 (numV 10))
                                 (override-store (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location"))

;; Tests

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test (parse `{box 0})
        (boxE (numE 0)))
  (test (parse `{unbox b})
        (unboxE (idE 'b)))
  (test (parse `{set-box! b 0})
        (setboxE (idE 'b) (numE 0)))
  (test (parse `{begin 1 2})
        (beginE (list (numE 1) (numE 2))))
  (test (parse `{record {x 2} {y 3}})
        (recordE (list 'x 'y)
                 (list (numE 2) (numE 3))))
  (test (parse `{get {+ 1 2} a})
        (rgetE (plusE (numE 1) (numE 2)) 'a))
  (test (parse `{set! {+ 1 2} a 7})
        (rsetE (plusE (numE 1) (numE 2)) 'a (numE 7)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (interp (parse `2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env)
                mt-store)
        (v*s (numV 9)
             mt-store))
  (test (interp (parse `{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse `{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusE (idE 'x) (idE 'x)) mt-env)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             mt-store))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             mt-store))
  (test (interp (parse `{box 5})
                mt-env
                mt-store)
        (v*s (boxV 1)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{unbox {box 5}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             mt-store)))
  (test (interp (parse `{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{begin 1 2 3})
                mt-env
                mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse `{unbox {begin {box 5} {box 6}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 2 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))
  (test (interp (parse `{let {[b (box 5)]}
                          {begin
                            {set-box! b 6}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             mt-store)))
  
  (test/exn (interp (parse `{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
            "free variable")
  (test/exn (interp (parse `{unbox 1}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{set-box! 1 1}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{get 1 x}) mt-env mt-store)
            "not a record")
  (test/exn (interp (parse `{set! 1 x 2}) mt-env mt-store)
            "not a record")
  (test (interp-expr (parse `{+ 1 4}))
        `5)
  (test (interp-expr (parse `{lambda {x} x}))
        `function)
  (test (interp-expr (parse `{box 4}))
        `box)
  (test (interp-expr (parse `{record {a 10} {b {+ 1 2}}}))
        `record)
  (test (interp-expr (parse `{get {record {a 10} {b {+ 1 0}}} b}))
        `1)
  (test/exn (interp-expr (parse `{get {record {a 10}} b}))
            "no such field")
  (test (interp-expr (parse `{get {record {r {record {z 0}}}} r}))
        `record)
  (test (interp-expr (parse `{get {get {record {r {record {z 0}}}} r} z}))
        `0)

  (test (interp-expr (parse `{let {[z {box 1}]}
                              {begin
                               {record {x {set-box! z 2}}}
                               {unbox z}}}))
        `2)
  (test (interp-expr (parse `{let {[z {box 0}]}
                              {begin
                               {record
                                {x {set-box! z {+ {unbox z} 1}}}
                                {y {set-box! z {+ {unbox z} 2}}}}
                               {unbox z}}}))
        `3)

  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {get r x}}))
        `1)

  (test (interp-expr (parse `{let {[z {box 1}]}
                              {let {[r {record {x 1}}]}
                               {begin
                                {get {begin {set-box! z 2} r} x}
                                {unbox z}}}}))
        `2)

  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {begin
                                 {set! r x 5}
                                 {get r x}}}))
        `5)
  
  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {let {[get-r {lambda {d} r}]}
                                 {begin
                                   {set! {get-r 0} x 6}
                                   {get {get-r 0} x}}}}))
        `6)

  (test (interp-expr (parse `{let {[g {lambda {r} {get r a}}]}
                               {let {[s {lambda {r} {lambda {v} {set! r b v}}}]}
                                 {let {[r1 {record {a 0} {b 2}}]}
                                   {let {[r2 {record {a 3} {b 4}}]}
                                     {+ {get r1 b}
                                        {begin
                                          {{s r1} {g r2}}
                                          {+ {begin
                                               {{s r2} {g r1}}
                                               {get r1 b}}
                                             {get r2 b}}}}}}}}))
        `5)

  (test (interp-expr (parse `{let {[r1 {record {x 1}}]}
                               {let {[r2 r1]}
                                 {begin
                                   {set! r1 x 2}
                                   {get r2 x}}}}))
        `2)
  (test (interp-seq (list (numE 1)) mt-env mt-store)
        (v*s (numV 1) mt-store))
  (test (interp-seq (list (boxE (numE 1)) (numE 2)) 
                    mt-env 
                    mt-store)
        (v*s (numV 2) (override-store (cell 1 (numV 1))
                                      mt-store)))
  (test (interp-record empty empty mt-env mt-store)
        (v*s (recV empty empty) mt-store))
  (test (interp-record (list 'x 'y) (list (numE 7) (numE 8)) mt-env mt-store)
        (v*s (recV (list 'x 'y) (list (boxV 2) (boxV 1)))
             (override-store (cell 1 (numV 8))
                             (override-store (cell 2 (numV 7))
                                             mt-store))))
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)
  
  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (override-store (cell 2 (numV 10))
                                 (override-store (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (override-store (cell 2 (numV 10))
                                 (override-store (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location")

  (test (update-store (cell 1 (numV 2)) mt-store)
        (override-store (cell 1 (numV 2)) mt-store))
  (test (update-store (cell 1 (numV 3))
                      (override-store (cell 1 (numV 2)) 
                                      mt-store))
        (override-store (cell 1 (numV 3)) mt-store))
  (test (update-store (cell 2 (numV 3))
                      (override-store (cell 1 (numV 1))
                                      (override-store (cell 2 (numV 2)) 
                                                      (override-store (cell 3 (numV 3)) 
                                                                      mt-store))))
        (override-store (cell 1 (numV 1))
                        (override-store (cell 2 (numV 3)) 
                                        (override-store (cell 3 (numV 3)) 
                                                        mt-store))))
  (test (update-store (cell 1 (numV 3))
                      (override-store (cell 2 (numV 2)) 
                                      mt-store))
        (override-store (cell 2 (numV 2))
                        (override-store (cell 1 (numV 3)) 
                                        mt-store)))
  ;; Tests from assignment pdf
  (test (interp (parse `{let {[b {box 1}]}
                       {begin
                       {set-box! b 2}
                       {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 2)
             (override-store (cell 1 (numV 2))
                             mt-store)))

  (test (interp (parse `{let {[b {box 1}]}
                       {begin
                       {set-box! b {+ 2 {unbox b}}}
                       {set-box! b {+ 3 {unbox b}}}
                       {set-box! b {+ 4 {unbox b}}}
                       {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 10)
             (override-store (cell 1 (numV 10))
                             mt-store)))

  (test (interp-expr (parse `{+ 1 4}))
        `5)
  (test (interp-expr (parse `{record {a 10} {b {+ 1 2}}}))
        `record)
  (test (interp-expr (parse `{get {record {a 10} {b {+ 1 0}}}
                            b}))
        `1)
  (test/exn (interp-expr (parse `{get {record {a 10}} b}))
            "no such field")
  (test (interp-expr (parse `{get {record {r {record {z 0}}}}
                            r}))
        `record)
  (test (interp-expr (parse `{get {get {record {r {record {z
                            0}}}} r} z}))
        `0)
  (test (interp-expr (parse `{let {[b {box 0}]}
                            {let {[r {record {a {unbox
                                     b}}}]}
                            {begin
                            {set-box! b 1}
                            {get r a}}}}))
        `0)

  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                            {get r x}}))
        `1)
  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                            {begin
                            {set! r x 5}
                            {get r x}}}))
        `5)
  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                            {let {[get-r {lambda {d} r}]}
                            {begin
                            {set! {get-r 0} x 6}
                            {get {get-r 0} x}}}}))
        `6)
  (test (interp-expr (parse `{let {[g {lambda {r} {get r a}}]}
                            {let {[s {lambda {r} {lambda {v}
                                     {set! r b v}}}]}
                            {let {[r1 {record {a 0} {b
                                      2}}]}
                            {let {[r2 {record {a 3} {b
                                      4}}]}
                            {+ {get r1 b}
                            {begin
                            {{s r1} {g r2}}
                            {+ {begin
                            {{s r2} {g r1}}
                            {get r1 b}}
                            {get r2
                            b}}}}}}}}))
        `5))
