(ns tp-scheme-99714.core-test
  (:require [clojure.test :refer :all]
            [tp-scheme-99714.core :refer :all]
            [clojure.string :refer [includes?]]))

(deftest fnc-sumar-test
  (testing "sumar 4 más 3 debe dar 7")
  (is (= 7 (fnc-sumar '(3 4))))

  (testing "sumar cadena vacía debe dar 0")
  (is (= 0 (fnc-sumar ())))

  (testing "sumar solo 3 debe dar 3")
  (is (= 3 (fnc-sumar '(3))))

  (testing "sumar 3, 4, 5 debe dar 12")
  (is (= 12 (fnc-sumar '(3 4 5))))

  (testing "sumar 0.5, 2 debe dar 2.5")
  (is (= 2.5 (fnc-sumar '(0.5 2))))

  (testing "sumar 3, 4, 5, 6 debe dar 18")
  (is (= 18 (fnc-sumar '(3 4 5 6))))

  (testing "sumar A, 4, 5, 6 debe devolver un error: +: Wrong type in arg1 A")
  (is (= true (includes? (fnc-sumar '(A 4 5 6)) ";ERROR: +: Wrong type in arg1 A")))

  (testing "sumar 3, A, 5, 6 debe devolver un error: +: Wrong type in arg2 A")
  (is (= true (includes? (fnc-sumar '(3 A 5 6)) ";ERROR: +: Wrong type in arg2 A")))

  (testing "sumar 3, 4, A, 6 debe devolver un error: +: Wrong type in arg2 A")
  (is (= true (includes? (fnc-sumar '(3 4 A 6)) ";ERROR: +: Wrong type in arg2 A")))

  (testing "sumar B, C debe devolver un error: +: Wrong type in arg1 B")
  (is (= true (includes? (fnc-sumar '(B C)) ";ERROR: +: Wrong type in arg1 B"))))

(deftest fnc-append-test
  (testing "concatenar (1 2) y (3 4) debe devovler (1 2 3 4)")
  (is (= '(1 2 3 4) (fnc-append '((1 2) (3 4)))))

  (testing "concatenar (1 2) y () debe devovler (1 2)")
  (is (= '(1 2) (fnc-append '((1 2) ()))))

  (testing "concatenar () debe devovler ()")
  (is (= '() (fnc-append '(()))))

  (testing "concatenar (1 2), (3), (4 5), (6 7) debe devovler (1 2 3 4 5 6 7)")
  (is (= '(1 2 3 4 5 6 7) (fnc-append '((1 2) (3) (4 5) (6 7)))))

  (testing "concatenar (1 2) 3 (4 5) (6 7) debe devovler ERROR: append: Wrong type in arg 3")
  (is (= true (includes? (fnc-append '((1 2) 3 (4 5) (6 7))) ";ERROR: append: Wrong type in arg 3")))

  (testing "concatenar (1 2) A (4 5) (6 7) debe devovler ERROR: append: Wrong type in arg A")
  (is (= true (includes? (fnc-append '((1 2) A (4 5) (6 7))) ";ERROR: append: Wrong type in arg A"))))

(deftest fnc-restar-test
  (testing "restar 3 menos 4 debe devovler -1")
  (is (= -1 (fnc-restar '(3 4))))

  (testing "restar (3) debe devolver -3")
  (is (= -3 (fnc-restar '(3))))

  (testing "restar (3 4 5) debe devolver -6")
  (is (= -6 (fnc-restar '(3 4 5))))

  (testing "restar (3 4 5 6) debe devolver -12")
  (is (= -12 (fnc-restar '(3 4 5 6))))

  (testing "restar (A 4 5 6) debe devolver (;ERROR: -: Wrong type in arg1 A)")
  (is (= true (includes? (fnc-restar '(A 4 5 6)) ";ERROR: -: Wrong type in arg1 A")))

  (testing "restar (3 A 5 6) debe devolver (;ERROR: -: Wrong type in arg2 A)")
  (is (= true (includes? (fnc-restar '(3 A 5 6)) ";ERROR: -: Wrong type in arg2 A")))

  (testing "restar (3 4 A 6) debe devolver (;ERROR: -: Wrong type in arg2 A)")
  (is (= true (includes? (fnc-restar '(3 4 A 6)) ";ERROR: -: Wrong type in arg2 A")))
  
  (testing "restar lista vacia debe devolver ;ERROR: -: Wrong number of args given")
  (is (= true (includes? (fnc-restar ()) ";ERROR: -: Wrong number of args given"))))

(deftest actualizar-amb-test
  (testing "actualizar ambiente () con clave 'b y valor 7 debe devolver (b 7)")
  (is (= '(b 7) (actualizar-amb () 'b 7)))

  (testing "actualizar ambiente (a 1 b 2 c 3) con clave 'd y valor 4 debe devolver (a 1 b 2 c 3 d 4)")
  (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))

  (testing "actualizar ambiente (a 1 b 2 c 3) con clave 'b y valor 4 debe devolver (a 1 b 4 c 3)")
  (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))

  (testing "actualizar ambiente (a 1 b 2 c 3) con error cuya clave existe, no actualiza el ambiente")
  (let [amb-inicial '(a 1 b 2 c 3), error (list (symbol ";ERROR:") 'mal 'hecho)]
    (is (= amb-inicial (actualizar-amb amb-inicial 'b error))))

  (testing "actualizar ambiente (a 1 b 2 c 3) con un error cuya clave no existe, no modifica el ambiente")
  (let [amb-inicial '(a 1 b 2 c 3), error (list (symbol ";ERROR:") 'mal 'hecho)]
    (is (= amb-inicial (actualizar-amb amb-inicial 'w error)))))

(deftest error?-test
  (testing "lista que comienza con ;ERROR: debe devolver verdadero")
  (is (= true (error? (list (symbol ";ERROR:") 'mal 'hecho))))

  (testing "lista que comienza con ;WARNING: debe devolver verdadero")
  (is (= true (error? (list (symbol ";WARNING:") 'mal 'hecho))))

  (testing "lista (mal hecho) debe devolver verdadero")
  (is (= false (error? (list 'mal 'hecho))))

  (testing "argumento no es una lista debe devolver falso")
  (is (= false (error? 10))))

(deftest buscar-test
  (testing "buscar dada la clave 'c en el ambiente '(a 1 b 2 c 3 d 4 e 5) devuelve 3")
  (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))

  (testing "buscar 'f en el ambiente '(a 1 b 2 c 3 d 4 e 5) devuelve un error: unbound-variable")
  (let [error-esperado (list (symbol ";ERROR:") 'unbound (symbol "variable:") 'f)]
    (is (= error-esperado (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))))

(deftest igual?-test
  (testing "es igual 'if a 'IF debe ser verdadero")
  (is (= true (igual? 'if 'IF)))

  (testing "es igual 'if a 'IF debe ser verdadero")
  (is (= true (igual? 'if 'if)))

  (testing "es igual 'IF a 'IF debe ser verdadero")
  (is (= true (igual? 'IF 'IF)))

  (testing "es igual 6 a 6 debe ser verdadero")
  (is (= true (igual? 6 6)))

  (testing "es igual 'IF a 'IF' debe ser falso")
  (is (= false (igual? 'IF "IF")))

  (testing "es igual 'if a 'IF debe ser falso")
  (is (= false (igual? 6 "6"))))

(deftest lower-case-arg-test
  (testing "lower case de 'IF debe devolver 'if")
  (is (= "if" (lower-case-arg 'IF)))

  (testing "lower case de 6 debe devolver 6")
  (is (= 6 (lower-case-arg 6))))

(deftest boolean-to-symbol-test
  (testing "true debe parsearse como #t")
  (is (= (symbol "#t") (boolean-to-symbol true)))

  (testing "false debe parsearse como #f")
  (is (= (symbol "#f") (boolean-to-symbol false))))

(deftest symbol-to-boolean-test
  (testing "#t debe dar true")
  (is (= true (symbol-to-boolean (symbol "#t"))))

  (testing "#f debe dar false")
  (is (= false (symbol-to-boolean (symbol "#f")))))

(deftest fnc-equal?-test
  (let [true-sym (symbol "#t"), false-sym (symbol "#f")]
    (testing "equal (A a) debe devolver #t")
    (is (= true-sym (fnc-equal? '(A a))))

    (testing "equal (A a A) debe devolver #t")
    (is (= true-sym (fnc-equal? '(A a A))))

    (testing "equal (A a A a) debe devolver #t")
    (is (= true-sym (fnc-equal? '(A a A a))))

    (testing "equal (A a a A B) debe devolver #f")
    (is (= false-sym (fnc-equal? '(A a a A B))))

    (testing "equal (1 1 1 1 1) debe devolver #t")
    (is (= true-sym (fnc-equal? '(1 1 1 1 1))))

    (testing "equal (1 1 2 1) debe devolver #f")
    (is (= false-sym (fnc-equal? '(1 1 2 1))))

    (testing "equal (A) debe devolver #t")
    (is (= true-sym (fnc-equal? '(A))))

    (testing "equal () debe devolver #t")
    (is (= true-sym (fnc-equal? ())))))

(deftest fnc-menor-test
  (let [true-sym (symbol "#t")
        false-sym (symbol "#f")
        error-arg1 (list (symbol ";ERROR:") (symbol "<:") 'Wrong 'type 'in 'arg1 'A)
        error-arg2 (list (symbol ";ERROR:") (symbol "<:") 'Wrong 'type 'in 'arg2 'A)]
    (testing "es menor () debe devolver #t")
    (is (= true-sym (fnc-menor ())))

    (testing "es menor (1) debe devolver #t")
    (is (= true-sym (fnc-menor '(1))))

    (testing "es menor (1 2) debe devolver #t")
    (is (= true-sym (fnc-menor '(1 2))))

    (testing "es menor (1 2 3) debe devolver #t")
    (is (= true-sym (fnc-menor '(1 2 3))))

    (testing "es menor (1 2 3 4) debe devolver #t")
    (is (= true-sym (fnc-menor '(1 2 3 4))))

    (testing "es menor (1 2 2 4) debe devolver #f")
    (is (= false-sym (fnc-menor '(1 2 2 4))))

    (testing "es menor (A 1 2 4) debe devolver (;ERROR: <: Wrong type in arg1 A)")
    (is (= error-arg1 (fnc-menor '(A 1 2 3 4))))

    (testing "es menor (1 A 1 4) debe devolver (;ERROR: <: Wrong type in arg2 A)")
    (is (= error-arg2 (fnc-menor '(1 A 1 4))))

    (testing "es menor (1 2 A 4) debe devolver (;ERROR: <: Wrong type in arg2 A)")
    (is (= error-arg2 (fnc-menor '(1 2 A 4))))))

(deftest fnc-mayor-test
  (let [true-sym (symbol "#t")
        false-sym (symbol "#f")
        error-arg1 (list (symbol ";ERROR:") (symbol ">:") 'Wrong 'type 'in 'arg1 'A)
        error-arg2 (list (symbol ";ERROR:") (symbol ">:") 'Wrong 'type 'in 'arg2 'A)]
    (testing "es mayor () debe devolver #t")
    (is (= true-sym (fnc-mayor ())))

    (testing "es mayor (1) debe devolver #t")
    (is (= true-sym (fnc-mayor '(1))))

    (testing "es mayor (2 1) debe devolver #t")
    (is (= true-sym (fnc-mayor '(2 1))))

    (testing "es mayor (3 2 1) debe devolver #t")
    (is (= true-sym (fnc-mayor '(3 2 1))))

    (testing "es mayor (4 3 2 1) debe devolver #t")
    (is (= true-sym (fnc-mayor '(4 3 2 1))))

    (testing "es mayor (4 2 2 1) debe devolver #f")
    (is (= false-sym (fnc-mayor '(4 2 2 1))))

    (testing "es mayor (4 2 1 4) debe devolver #f")
    (is (= false-sym (fnc-mayor '(4 2 1 4))))

    (testing "es mayor (A 3 2 1) debe devolver (;ERROR: >: Wrong type in arg1 A)")
    (is (= error-arg1 (fnc-mayor '(A 3 2 1))))

    (testing "es mayor (3 A 2 1) debe devolver (;ERROR: >: Wrong type in arg2 A)")
    (is (= error-arg2 (fnc-mayor '(3 A 2 1))))

    (testing "es mayor (3 2 A 1) debe devolver (;ERROR: >: Wrong type in arg2 A)")
    (is (= error-arg2 (fnc-mayor '(3 2 A 1))))))

(deftest fnc-mayor-o-igual-test
  (let [true-sym (symbol "#t")
        false-sym (symbol "#f")
        error-arg1 (list (symbol ";ERROR:") (symbol ">=:") 'Wrong 'type 'in 'arg1 'A)
        error-arg2 (list (symbol ";ERROR:") (symbol ">=:") 'Wrong 'type 'in 'arg2 'A)]
    (testing "es mayor o igual () debe devolver #t")
    (is (= true-sym (fnc-mayor-o-igual ())))

    (testing "es mayor o igual (1) debe devolver #t")
    (is (= true-sym (fnc-mayor-o-igual '(1))))

    (testing "es mayor o igual (2 1) debe devolver #t")
    (is (= true-sym (fnc-mayor-o-igual '(2 1))))

    (testing "es mayor o igual (3 2 1) debe devolver #t")
    (is (= true-sym (fnc-mayor-o-igual '(3 2 1))))

    (testing "es mayor o igual (4 3 2 1) debe devolver #t")
    (is (= true-sym (fnc-mayor-o-igual '(4 3 2 1))))

    (testing "es mayor o igual (4 2 2 1) debe devolver #t")
    (is (= true-sym (fnc-mayor-o-igual '(4 2 2 1))))

    (testing "es mayor o igual (4 2 1 4) debe devolver #f")
    (is (= false-sym (fnc-mayor-o-igual '(4 2 1 4))))

    (testing "es mayor o igual (A 3 2 1) debe devolver (;ERROR: >=: Wrong type in arg1 A)")
    (is (= error-arg1 (fnc-mayor-o-igual '(A 3 2 1))))

    (testing "es mayor o igual (3 A 2 1) debe devolver (;ERROR: >=: Wrong type in arg2 A)")
    (is (= error-arg2 (fnc-mayor-o-igual '(3 A 2 1))))

    (testing "es mayor o igual (3 2 A 1) debe devolver (;ERROR: >=: Wrong type in arg2 A)")
    (is (= error-arg2 (fnc-mayor-o-igual '(3 2 A 1))))))

(deftest evaluar-escalar-test
  (let [amb '(x 6 y 11 z "hola")]
    (testing "evaluar escalar 32 '(x 6 y 11 z 'hola') devuelve (32 (x 6 y 11 z 'hola'))")
    (let [escalar 32]
      (is (= (list escalar amb) (evaluar-escalar escalar amb))))

    (testing "evaluar escalar 'chau' '(x 6 y 11 z 'hola') devuelve ('chau' (x 6 y 11 z 'hola'))")
    (let [escalar "chau"]
      (is (= (list escalar amb) (evaluar-escalar escalar amb))))

    (testing "evaluar escalar 'y '(x 6 y 11 z 'hola') devuelve (11 (x 6 y 11 z 'hola'))")
    (let [escalar 'y]
      (is (= (list 11 amb) (evaluar-escalar escalar amb))))

    (testing "evaluar escalar 'z '(x 6 y 11 z 'hola') devuelve ('hola' (x 6 y 11 z 'hola'))")
    (let [escalar 'z]
      (is (= (list "hola" amb) (evaluar-escalar escalar amb))))

    (testing "evaluar escalar 'n '(x 6 y 11 z 'hola') devuelve ((;ERROR: unbound variable: n) (x 6 y 11 z 'hola'))")
    (let [escalar 'n, error (list (symbol ";ERROR:") 'unbound (symbol "variable:") 'n)]
      (is (= (list error amb) (evaluar-escalar escalar amb))))))

(deftest proteger-bool-en-str-test
  (testing "proteger bool en str para '(or #F #f #t #T)' debe devolver '(or %F %f %t %T)'")
  (is (= "(or %F %f %t %T)" (proteger-bool-en-str "(or #F #f #t #T)")))

  (testing "proteger bool en str para '(and (or #F #f #t #T) #T)' debe devolver '(and (or %F %f %t %T) %T)'")
  (is (= "(and (or %F %f %t %T) %T)" (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))

  (testing "proteger bool en str para '' debe devolver ''")
  (is (= "" (proteger-bool-en-str ""))))

(deftest restaurar-bool-test
  (testing "restaurar bool (and (or %F %f %t %T) %T) debe devolver (and (or #F #f #t #T) #T)")
  (let [expected-exp (list 'and (list 'or (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))]
    (is (= expected-exp (restaurar-bool (read-string "(and (or %F %f %t %T) %T)")))))

  (testing "restaurar bool (and (or %F (or %f %f)) %T) debe devolver (and (or #F (or #f #f)) #T)")
  (let [expected-exp (list 'and (list 'or (symbol "#F") (list 'or (symbol "#f") (symbol "#f"))) (symbol "#T"))]
    (is (= expected-exp (restaurar-bool (restaurar-bool (read-string "(and (or %F (or %f %f)) %T)")))))))

(deftest evaluar-define-test
  (testing "define (f x) (+ x 1) con ambiente (x 1) devuelve (#<unspecified> (x 1 f (lambda (x) (+ x 1))))")
  (let [expected-result (cons (symbol "#<unspecified>") '((x 1 f (lambda (x) (+ x 1)))))]
    (is (= expected-result (evaluar-define '(define (f x) (+ x 1)) '(x 1)))))

  (testing "define (sumar a b) (+ a b) con ambiente (x 1) devuelve (#<unspecified> (x 1 sumar (lambda (a b) (+ a b))))")
  (let [expected-result (cons (symbol "#<unspecified>") '((x 1 sumar (lambda (a b) (+ a b)))))]
    (is (= expected-result (evaluar-define '(define (sumar a b) (+ a b)) '(x 1)))))

  (testing "define (f) 1 con ambiente (x 1) devuelve (#<unspecified> (x 1 f (lambda () 1)))")
  (let [expected-result (cons (symbol "#<unspecified>") '((x 1 f (lambda () 1))))]
    (is (= expected-result (evaluar-define '(define (f) 1) '(x 1)))))

  (testing "define (f x) (display x) (+ x 1) con ambiente (x 1) devuelve (#<unspecified> (x 1 f (lambda (x) (display x) (+ x 1))))")
  (let [expected-result (cons (symbol "#<unspecified>") '((x 1 f (lambda (x) (display x) (+ x 1)))))]
    (is (= expected-result (evaluar-define '(define (f x) (display x) (+ x 1)) '(x 1)))))

  (testing "define (w 2) con amiente (x 1) debe actualizar el ambiente (x 2 w 2)")
  (let [expected-result (cons (symbol "#<unspecified>") '((x 2 w 2)))]
    (is (= expected-result (evaluar-define '(define w 2) '(x 2)))))

  (testing "define (x 2) con ambiente (x 1) debe actualizar el ambiente (x 2)")
  (let [expected-result (cons (symbol "#<unspecified>") '((x 2)))]
    (is (= expected-result (evaluar-define '(define x 2) '(x 1)))))

  (testing "define () con ambiente (x 1) deve devolver ((;ERROR: define: missing or extra expression (define)) (x 1))")
  (let [expected-result (list (list (symbol ";ERROR:") (symbol "define:") 'missing 'or 'extra 'expression (list 'define)) '(x 1))]
    (is (= expected-result (evaluar-define '(define) '(x 1)))))

  (testing "define (x) con ambiente (x 1) deve devolver ((;ERROR: define: missing or extra expression (define x)) (x 1))")
  (let [expected-result (list (list (symbol ";ERROR:") (symbol "define:") 'missing 'or 'extra 'expression '(define x)) '(x 1))]
    (is (= expected-result (evaluar-define '(define x) '(x 1)))))

  (testing "define (x 2 3) con ambiente (x 1) deve devolver ((;ERROR: define: missing or extra expression (define x 2 3)) (x 1))")
  (let [expected-result (list (list (symbol ";ERROR:") (symbol "define:") 'missing 'or 'extra 'expression '(define x 2 3)) '(x 1))]
    (is (= expected-result (evaluar-define '(define x 2 3) '(x 1)))))

  (testing "define (()) con ambiente (x 1) deve devolver ((;ERROR: define: missing or extra expression (define ())) (x 1))")
  (let [expected-result (list (list (symbol ";ERROR:") (symbol "define:") 'missing 'or 'extra 'expression '(define ())) '(x 1))]
    (is (= expected-result (evaluar-define '(define ()) '(x 1)))))

  (testing "define (() 2) con ambiente (x 1) deve devolver ((;ERROR: define: bad  variable (define () 2)) (x 1))")
  (let [expected-result (list (list (symbol ";ERROR:") (symbol "define:") 'bad 'variable '(define () 2)) '(x 1))]
    (is (= expected-result (evaluar-define '(define () 2) '(x 1)))))

  (testing "define (2 x) con ambiente (x 1) deve devolver ((;ERROR: define: bad  variable (define 2 x)) (x 1))")
  (let [expected-result (list (list (symbol ";ERROR:") (symbol "define:") 'bad 'variable '(define 2 x)) '(x 1))]
    (is (= expected-result (evaluar-define '(define 2 x) '(x 1))))))


(deftest evaluar-set!-test
  (testing "evaluar set '(set! x 1) '(x 0) debe actualizar el ambiente (x 1)")
  (is (= (list (symbol "#<unspecified>") '(x 1)) (evaluar-set! '(set! x 1) '(x 0))))

  (testing "evaluar set '(set! x 1) '() debe devolver ((;ERROR: unbound variable: x) ())")
  (is (= (list (list (symbol ";ERROR:") 'unbound (symbol "variable:") 'x) '()) (evaluar-set! '(set! x 1) '())))

  (testing "evaluar set '(set! x) '(x 0) debe devolver ((;ERROR: set!: missing or extra expression (set! x)) (x 0))")
  (let [expected-result (list (list (symbol ";ERROR:") (symbol "set!:") 'missing 'or 'extra 'expression (list 'set! 'x)) '(x 0))]
    (is (= expected-result (evaluar-set! '(set! x) '(x 0)))))

  (testing "evaluar set '(set! x 1 2) '(x 0) debe devolver ((;ERROR: set!: missing or extra expression (set! x 1 2)) (x 0))")
  (let [expected-result (list (list (symbol ";ERROR:") (symbol "set!:") 'missing 'or 'extra 'expression (list 'set! 'x '1 '2)) '(x 0))]
    (is (= expected-result (evaluar-set! '(set! x 1 2) '(x 0)))))

  (testing "evaluar set '(set! 1 2) '(x 0) debe devolver ((;ERROR: set!: bad variable 1) (x 0))")
  (let [expected-result (list (list (symbol ";ERROR:") (symbol "set!:") 'bad 'variable '1) '(x 0))]
    (is (= expected-result (evaluar-set! '(set! 1 2) '(x 0)))))

  (testing "evaluar set '(set! x (+ 2 x)) '(+ + x 0) debe actualizar el ambiente (+ + x 2)")
  (is (= (list (symbol "#<unspecified>") '(+ + x 2)) (evaluar-set! '(set! x (+ 2 x)) '(+ + x 0)))))

(deftest evaluar-or-test
  (let [false-sym (symbol "#f"), true-sym (symbol "#t"), amb (list false-sym false-sym true-sym true-sym)]
    (testing "evaluar or () debe devolver #f")
    (is (= (list false-sym amb) (evaluar-or '(or) amb)))

    (testing "evaluar or (#t) debe devolver #t")
    (is (= (list true-sym amb) (evaluar-or (list 'or true-sym) amb)))

    (testing "evaluar or (#f) debe devolver #f")
    (is (= (list false-sym amb) (evaluar-or (list 'or false-sym) amb)))

    (testing "evaluar or (7) debe devolver 7")
    (is (= (list 7 amb) (evaluar-or (list 'or 7) amb)))

    (testing "evaluar or (#t 7) debe devolver #t")
    (is (= (list true-sym amb) (evaluar-or (list 'or true-sym 7) amb)))

    (testing "evaluar or (#f 7) debe devolver 7")
    (is (= (list 7 amb) (evaluar-or (list 'or false-sym 7) amb)))

    (testing "evaluar or (#f (< 0 10)) debe devolver #t")
    (let [amb (list '< '< false-sym false-sym true-sym true-sym)]
      (is (= (list true-sym amb) (evaluar-or (list 'or false-sym '(< 0 10)) amb))))))

(deftest verificar-parentesis-test
  (testing "verificar parentesis de () debe devolver 0")
  (is (= 0 (verificar-parentesis "()")))

  (testing "verificar parentesis de '(hola 'mundo' debe devolver -1")
  (is (= 1 (verificar-parentesis "(hola 'mundo")))

  (testing "verificar parentesis de ()) debe devolver -1")
  (is (= -1 (verificar-parentesis "())")))

  (testing "verificar parentesis de (hola '(mundo) () 6) 7) debe devolver 0")
  (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))

  (testing "verificar parentesis de (hola '(mundo) () 6) 7) 9) debe devolver -1")
  (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))

  (testing "verificar parentesis de (hola '(mundo) ) debe devolver 0")
  (is (= 0 (verificar-parentesis "(hola '(mundo) )"))))

(deftest leer-entrada-test
  (testing "leer entrada de 123 debe devolver '123'")
  (is (= "123" (with-in-str "123" (leer-entrada))))

  (testing "leer entrada multilinea (hola mundo) debe devolver (hola mundo) ")
  (let [input "(hola
 mundo)"]
    (is (= "(hola mundo)" (with-in-str input (leer-entrada))))))

(deftest fnc-read-test
  (testing "read () con (hola mundo) multilinea debe devolver (hola mundo)")
  (let [input "(hola
 mundo)"]
    (is (= '(hola mundo) (with-in-str input (fnc-read ())))))

  (testing "read (1) devolver (;ERROR: read: Use of I/O ports not implemented)")
  (let [error (list (symbol ";ERROR:") (symbol "read:") 'Use 'of 'I/O 'ports 'not 'implemented)]
    (is (= error (fnc-read '(1)))))

  (testing "read (1 2) debe devolver (;ERROR: Wrong number of args given #<primitive-procedure read>)")
  (let [error (list (symbol ";ERROR:") 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") (symbol "read>"))]
    (is (= error (fnc-read '(1 2)))))

  (testing "read (1 2 3) debe devolver (;ERROR: Wrong number of args given #<primitive-procedure read>)")
  (let [error (list (symbol ";ERROR:") 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") (symbol "read>"))]
    (is (= error (fnc-read '(1 2 3))))))

(deftest evaluar-if-test


; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 9 #f #f))
; user=> (evaluar-if '(if) '(n 7))
; ((;ERROR: if: missing or extra expression (if)) (n 7))
; user=> (evaluar-if '(if 1) '(n 7))
; ((;ERROR: if: missing or extra expression (if 1)) (n 7))

  (testing "evaluar if (1 2) amb (n 7) debe devolver (2 (n 7))")
  (is (= '(2 (n 7)) (evaluar-if '(if 1 2) '(n 7))))

  (testing "evaluar if (1 n 8) amb (n 7) debe devolver (2 (n 7))")
  (is (= '(7 (n 7)) (evaluar-if '(if 1 n 8) '(n 7))))
  
  (testing "evaluar if (#f n 8) amb (n 7 #f #f) debe devolver (8 (n 7 #f #f))")
  (is (= (list 8 (list 'n 7 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))))
  
  (testing "evaluar if (#f n) amb (n 7 #f #f) debe devolver (#<unspecified> (n 7 #f #f))")
  (is (= (list (symbol "#<unspecified>") (list 'n '7 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n) (list 'n '7 (symbol "#f") (symbol "#f")))))
  
  (testing "evaluar if (#f n) amb (n 9 #f #f) debe devolver (#<unspecified> (n 9 #f #f))")
  (is (= (list (symbol "#<unspecified>") (list 'n '9 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))))
  )
