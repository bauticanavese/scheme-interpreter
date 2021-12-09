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
  (is (= true (includes? (fnc-sumar '(A 4 5 6)) ";ERROR: +: Wrong type in arg1 A" )))
  
  (testing "sumar 3, A, 5, 6 debe devolver un error: +: Wrong type in arg2 A")
  (is (= true (includes? (fnc-sumar '(3 A 5 6)) ";ERROR: +: Wrong type in arg2 A" )))
  
  (testing "sumar 3, 4, A, 6 debe devolver un error: +: Wrong type in arg2 A")
  (is (= true (includes? (fnc-sumar '(3 4 A 6)) ";ERROR: +: Wrong type in arg2 A" )))
  
  (testing "sumar B, C debe devolver un error: +: Wrong type in arg1 B")
  (is (= true (includes? (fnc-sumar '(B C)) ";ERROR: +: Wrong type in arg1 B" )))
)

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
  (is (= true (includes? (fnc-append '((1 2) A (4 5) (6 7))) ";ERROR: append: Wrong type in arg A")))
)

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
  (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4))))