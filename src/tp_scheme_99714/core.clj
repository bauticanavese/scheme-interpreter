(ns tp-scheme-99714.core
  (:gen-class)
  (:require [clojure.walk :refer [prewalk-replace]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(require '[clojure.string :as st :refer [blank? starts-with? ends-with? lower-case]]
         '[clojure.java.io :refer [delete-file reader]]
         '[clojure.walk :refer [postwalk postwalk-replace]])

(defn spy
  ([x] (do (prn x) x))
  ([msg x] (do (print msg) (print ": ") (prn x) x)))

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-load)
(declare evaluar-set!)
(declare evaluar-quote)
(declare evaluar-define)
(declare evaluar-lambda)
(declare evaluar-escalar)

; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-car)
(declare fnc-cdr)
(declare fnc-env)
(declare fnc-not)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-list?)
(declare fnc-read)
(declare fnc-mayor)
(declare fnc-menor)
(declare fnc-null?)
(declare fnc-sumar)
(declare fnc-append)
(declare fnc-equal?)
(declare fnc-length)
(declare fnc-restar)
(declare fnc-display)
(declare fnc-newline)
(declare fnc-reverse)
(declare fnc-mayor-o-igual)

; Funciones auxiliares

(declare buscar)
(declare error?)
(declare igual?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare leer-entrada)
(declare actualizar-amb)
(declare restaurar-bool)
(declare generar-nombre-arch)
(declare nombre-arch-valido?)
(declare controlar-aridad-fnc)
(declare proteger-bool-en-str)
(declare verificar-parentesis)
(declare generar-mensaje-error)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-de-cond)
(declare evaluar-secuencia-en-cond)


; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra > y lee una expresion y la evalua. El resultado es una lista con un valor y un ambiente. 
; Si la 2da. posicion del resultado es nil, devuelve 'Goodbye! (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
  "Inicia el REPL de Scheme."
  ([]
   (println "Interprete de Scheme en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2021") (prn)
   (println "Inspirado en:")
   (println "  SCM version 5f2.")                        ; https://people.csail.mit.edu/jaffer/SCM.html
   (println "  Copyright (C) 1990-2006 Free Software Foundation.") (prn) (flush)
   (repl (list 'append 'append 'car 'car 'cdr 'cdr 'cond 'cond 'cons 'cons 'define 'define
               'display 'display 'env 'env 'equal? 'equal? 'eval 'eval 'exit 'exit
               'if 'if 'lambda 'lambda 'length 'length 'list 'list 'list? 'list? 'load 'load
               'newline 'newline 'nil (symbol "#f") 'not 'not 'null? 'null? 'or 'or 'quote 'quote
               'read 'read 'reverse 'reverse 'set! 'set! (symbol "#f") (symbol "#f")
               (symbol "#t") (symbol "#t") '+ '+ '- '- '< '< '> '> '>= '>=)))
  ([amb]
   (print "> ") (flush)
   (try
     (let [renglon (leer-entrada)]                       ; READ
       (if (= renglon "")
         (repl amb)
         (let [str-corregida (proteger-bool-en-str renglon)
               cod-en-str (read-string str-corregida)
               cod-corregido (restaurar-bool cod-en-str)
               res (evaluar cod-corregido amb)]     ; EVAL
           (if (nil? (second res))              ;   Si el ambiente del resultado es `nil`, es porque se ha evaluado (exit)
             'Goodbye!                        ;   En tal caso, sale del REPL devolviendo Goodbye!.
             (do (imprimir (first res))       ; PRINT
                 (repl (second res)))))))     ; LOOP (Se llama a si misma con el nuevo ambiente)
     (catch Exception e                                  ; PRINT (si se lanza una excepcion)
       (imprimir (generar-mensaje-error :error (get (Throwable->map e) :cause)))
       (repl amb)))))                        ; LOOP (Se llama a si misma con el ambiente intacto)


(defn evaluar
  "Evalua una expresion `expre` en un ambiente. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb]
  (if (and (seq? expre) (or (empty? expre) (error? expre))) ; si `expre` es () o error, devolverla intacta
    (list expre amb)                                      ; de lo contrario, evaluarla
    (cond
      (not (seq? expre))             (evaluar-escalar expre amb)
      (igual? (first expre) 'define) (evaluar-define expre amb)

         ;
         ;
         ;
         ; Si la expresion no es la aplicacion de una funcion (es una forma especial, una macro...) debe ser evaluada
         ; por una funcion de Clojure especifica debido a que puede ser necesario evitar la evaluacion de los argumentos
         ;
         ;
         ;

      :else (let [res-eval-1 (evaluar (first expre) amb)
                  res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x))] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
              (aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2))))))


(defn aplicar
  "Aplica la funcion `fnc` a la lista de argumentos `lae` evaluados en el ambiente dado."
  ([fnc lae amb]
   (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb))
  ([resu1 resu2 fnc lae amb]
   (cond
     (error? resu1) (list resu1 amb)
     (error? resu2) (list resu2 amb)
     (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb) amb)
     :else (aplicar-lambda fnc lae amb))))


(defn aplicar-lambda
  "Aplica la funcion lambda `fnc` a `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  (cond
    (not= (count lae) (count (second fnc))) (list (generar-mensaje-error :wrong-number-args fnc) amb)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb)
    :else (aplicar-lambda-multiple fnc lae amb)))


(defn aplicar-lambda-simple
  "Evalua un lambda `fnc` con un cuerpo simple"
  [fnc lae amb]
  (let [lae-con-quotes (map #(if (or (number? %) (string? %) (and (seq? %) (igual? (first %) 'lambda)))
                               %
                               (list 'quote %)) lae)
        nuevos-pares (reduce concat (map list (second fnc) lae-con-quotes))
        mapa (into (hash-map) (vec (map vec (partition 2 nuevos-pares))))
        cuerpo (first (nnext fnc))
        expre (if (and (seq? cuerpo) (seq? (first cuerpo)) (igual? (ffirst cuerpo) 'lambda))
                (cons (first cuerpo) (postwalk-replace mapa (rest cuerpo)))
                (postwalk-replace mapa cuerpo))]
    (evaluar expre amb)))


(defn aplicar-lambda-multiple
  "Evalua una funcion lambda `fnc` cuyo cuerpo contiene varias partes."
  [fnc lae amb]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
           lae
           (second (aplicar-lambda-simple fnc lae amb))))


(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  (cond
    (= fnc '<) (fnc-menor lae)
    (= fnc '+) (fnc-sumar lae)
    ;
    ;
    ; Si la funcion primitiva esta identificada por un simbolo, puede determinarse mas rapido que hacer con ella
    ;
    ;


    (igual? fnc 'append)  (fnc-append lae)

    ;
    ;
    ; Si la funcion primitiva esta identificada mediante una palabra reservada, debe ignorarse la distincion entre mayusculas y minusculas 
    ;
    ;

    :else (generar-mensaje-error :wrong-type-apply fnc)))


(defn fnc-car
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'car), arg1 (first lae)]
    (cond
      (error? ari) ari
      (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'car arg1)
      :else (first arg1))))


(defn fnc-cdr
  "Devuelve una lista sin su 1ra. posicion."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'cdr), arg1 (first lae)]
    (cond
      (error? ari) ari
      (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'cdr arg1)
      :else (rest arg1))))


(defn fnc-cons
  "Devuelve el resultado de insertar un elemento en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 2 'cons), arg1 (first lae), arg2 (second lae)]
    (cond
      (error? ari) ari
      (not (seq? arg2)) (generar-mensaje-error :only-proper-lists-implemented 'cons)
      :else (cons arg1 arg2))))


(defn fnc-display
  "Imprime un elemento por la termina/consola y devuelve #<unspecified>."
  [lae]
  (let [cant-args (count lae), arg1 (first lae)]
    (case cant-args
      1 (do (print arg1) (flush) (symbol "#<unspecified>"))
      2 (generar-mensaje-error :io-ports-not-implemented 'display)
      (generar-mensaje-error :wrong-number-args-prim-proc 'display))))


(defn fnc-env
  "Devuelve el ambiente."
  [lae amb]
  (let [ari (controlar-aridad-fnc lae 0 'env)]
    (if (error? ari)
      ari
      amb)))


(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'length), arg1 (first lae)]
    (cond
      (error? ari) ari
      (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'length arg1)
      :else (count arg1))))


(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1)
    ()
    lae))


(defn fnc-list?
  "Devuelve #t si un elemento es una lista. Si no, #f."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'list?), arg1 (first lae)]
    (if (error? ari)
      ari
      (if (seq? arg1)
        (symbol "#t")
        (symbol "#f")))))


(defn fnc-newline
  "Imprime un salto de linea y devuelve #<unspecified>."
  [lae]
  (let [cant-args (count lae)]
    (case cant-args
      0 (do (newline) (flush) (symbol "#<unspecified>"))
      1 (generar-mensaje-error :io-ports-not-implemented 'newline)
      (generar-mensaje-error :wrong-number-args-prim-proc 'newline))))


(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'not)]
    (if (error? ari)
      ari
      (if (igual? (first lae) (symbol "#f"))
        (symbol "#t")
        (symbol "#f")))))


(defn fnc-null?
  "Devuelve #t si un elemento es ()."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'null?)]
    (if (error? ari)
      ari
      (if (= (first lae) ())
        (symbol "#t")
        (symbol "#f")))))


(defn fnc-reverse
  "Devuelve una lista con los elementos de `lae` en orden inverso."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'reverse), arg1 (first lae)]
    (cond
      (error? ari) ari
      (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'reverse arg1)
      :else (reverse arg1))))


(defn controlar-aridad-fnc
  "Si la `lae` tiene la longitud esperada, se devuelve este valor (que es la aridad de la funcion).
   Si no, devuelve una lista con un mensaje de error."
  [lae val-esperado fnc]
  (if (= val-esperado (count lae))
    val-esperado
    (generar-mensaje-error :wrong-number-args-prim-proc fnc)))


(defn imprimir
  "Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas
  con comillas) y devuelve su valor. Muestra errores sin parentesis."
  ([elem]
   (cond
     (= \space elem) elem    ; Si es \space no lo imprime pero si lo devuelve
     (and (seq? elem) (starts-with? (apply str elem) ";")) (imprimir elem elem)
     :else (do (prn elem) (flush) elem)))
  ([lis orig]
   (cond
     (nil? lis) (do (prn) (flush) orig)
     :else (do (pr (first lis))
               (print " ")
               (imprimir (next lis) orig)))))


(defn revisar-fnc
  "Si la `lis` representa un error lo devuelve; si no, devuelve nil."
  [lis] (if (error? lis) lis nil))


(defn revisar-lae
  "Si la `lis` contiene alguna sublista que representa un error lo devuelve; si no, devuelve nil."
  [lis] (first (remove nil? (map revisar-fnc (filter seq? lis)))))


(defn evaluar-cond
  "Evalua una expresion `cond`."
  [expre amb]
  (if (= (count expre) 1) ; si es el operador solo
    (list (generar-mensaje-error :bad-or-missing 'cond expre) amb)
    (let [res (drop-while #(and (seq? %) (not (empty? %))) (next expre))]
      (if (empty? res)
        (evaluar-clausulas-de-cond expre (next expre) amb)
        (list (generar-mensaje-error :bad-or-missing 'cond (first res)) amb)))))


(defn evaluar-clausulas-de-cond
  "Evalua las clausulas de cond."
  [expre lis amb]
  (if (nil? lis)
    (list (symbol "#<unspecified>") amb) ; cuando ninguna fue distinta de #f
    (let [res-eval (if (not (igual? (ffirst lis) 'else))
                     (evaluar (ffirst lis) amb)
                     (if (nil? (next lis))
                       (list (symbol "#t") amb)
                       (list (generar-mensaje-error :bad-else-clause 'cond expre) amb)))]
      (cond
        (error? (first res-eval)) res-eval
        (igual? (first res-eval) (symbol "#f")) (recur expre (next lis) (second res-eval))
        :else (evaluar-secuencia-en-cond (nfirst lis) (second res-eval))))))


(defn evaluar-secuencia-en-cond
  "Evalua secuencialmente las sublistas de `lis`. Devuelve el valor de la ultima evaluacion."
  [lis amb]
  (if (nil? (next lis))
    (evaluar (first lis) amb)
    (let [res-eval (evaluar (first lis) amb)]
      (if (error? (first res-eval))
        res-eval
        (recur (next lis) (second res-eval))))))


(defn evaluar-eval
  "Evalua una expresion `eval`."
  [expre amb]
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
    (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE <anon> ...")) amb)
    (let [arg (second expre)]
      (if (and (seq? arg) (igual? (first arg) 'quote))
        (evaluar (second arg) amb)
        (evaluar arg amb)))))


(defn evaluar-exit
  "Sale del interprete de Scheme."
  [expre amb]
  (if (> (count expre) 2) ; si son el operador y mas de 1 argumento
    (list (generar-mensaje-error :wrong-number-args-prim-proc 'quit) amb)
    (list nil nil)))


(defn evaluar-lambda
  "Evalua una expresion `lambda`."
  [expre amb]
  (cond
    (< (count expre) 3) ; si son el operador solo o con 1 unico argumento
    (list (generar-mensaje-error :bad-body 'lambda (rest expre)) amb)
    (not (seq? (second expre)))
    (list (generar-mensaje-error :bad-params 'lambda expre) amb)
    :else (list expre amb)))


(defn evaluar-load
  "Evalua una expresion `load`. Carga en el ambiente un archivo `expre` de codigo en Scheme."
  [expre amb]
  (if (= (count expre) 1) ; si es el operador solo
    (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE scm:load ...")) amb)
    (list (symbol "#<unspecified>") (cargar-arch amb (second expre)))))


(defn cargar-arch
  "Carga y devuelve el contenido de un archivo."
  ([amb arch]
   (let [res (evaluar arch amb)
         nom-original (first res)
         nuevo-amb (second res)]
     (if (error? nom-original)
       (do (imprimir nom-original) nuevo-amb)                 ; Mostrar el error
       (let [nom-a-usar (generar-nombre-arch nom-original)]
         (if (error? nom-a-usar)
           (do (imprimir nom-a-usar) nuevo-amb)          ; Mostrar el error
           (let [tmp (try
                       (slurp nom-a-usar)
                       (catch java.io.FileNotFoundException _
                         (generar-mensaje-error :file-not-found)))]
             (if (error? tmp)
               (do (imprimir tmp) nuevo-amb)        ; Mostrar el error
               (do (spit "scm-temp" (proteger-bool-en-str tmp))
                   (let [ret (with-open [in (java.io.PushbackReader. (reader "scm-temp"))]
                               (binding [*read-eval* false]
                                 (try
                                   (imprimir (list (symbol ";loading") (symbol nom-original)))
                                   (cargar-arch (second (evaluar (restaurar-bool (read in)) nuevo-amb)) in nom-original nom-a-usar)
                                   (catch Exception e
                                     (imprimir (generar-mensaje-error :end-of-file 'list))))))]
                     (do (delete-file "scm-temp" true) ret))))))))))
  ([amb in nom-orig nom-usado]
   (try
     (cargar-arch (second (evaluar (restaurar-bool (read in)) amb)) in nom-orig nom-usado)
     (catch Exception _
       (imprimir (list (symbol ";done loading") (symbol nom-usado)))
       amb))))


(defn generar-nombre-arch
  "Dada una entrada la convierte en un nombre de archivo .scm valido."
  [nom]
  (if (not (string? nom))
    (generar-mensaje-error :wrong-type-arg1 'string-length nom)
    (let [n (lower-case nom)]
      (if (nombre-arch-valido? n)
        n
        (str n ".scm")))))    ; Agrega '.scm' al final


(defn nombre-arch-valido?
  "Chequea que el string sea un nombre de archivo .scm valido."
  [nombre] (and (> (count nombre) 4) (ends-with? nombre ".scm")))


(defn evaluar-quote
  "Evalua una expresion `quote`."
  [expre amb]
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
    (list (generar-mensaje-error :missing-or-extra 'quote expre) amb)
    (list (second expre) amb)))


(defn generar-mensaje-error
  "Devuelve un mensaje de error expresado como lista."
  ([cod]
   (case cod
     :file-not-found (list (symbol ";ERROR:") 'No 'such 'file 'or 'directory)
     :warning-paren (list (symbol ";WARNING:") 'unexpected (symbol "\")\"#<input-port 0>"))
     ()))
  ([cod fnc]
   (cons (symbol ";ERROR:")
         (case cod
           :end-of-file (list (symbol (str fnc ":")) 'end 'of 'file)
           :error (list (symbol (str fnc)))
           :io-ports-not-implemented (list (symbol (str fnc ":")) 'Use 'of 'I/O 'ports 'not 'implemented)
           :only-proper-lists-implemented (list (symbol (str fnc ":")) 'Only 'proper 'lists 'are 'implemented)
           :unbound-variable (list 'unbound (symbol "variable:") fnc)
           :wrong-number-args (list 'Wrong 'number 'of 'args 'given fnc)
           :wrong-number-args-oper (list (symbol (str fnc ":")) 'Wrong 'number 'of 'args 'given)
           :wrong-number-args-prim-proc (list 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") (symbol (str fnc '>)))
           :wrong-type-apply (list 'Wrong 'type 'to 'apply fnc)
           ())))
  ([cod fnc nom-arg]
   (cons (symbol ";ERROR:") (cons (symbol (str fnc ":"))
                                  (case cod
                                    :bad-body (list 'bad 'body nom-arg)
                                    :bad-else-clause (list 'bad 'ELSE 'clause nom-arg)
                                    :bad-or-missing (list 'bad 'or 'missing 'clauses nom-arg)
                                    :bad-params (list 'Parameters 'are 'implemented 'only 'as 'lists nom-arg)
                                    :bad-variable (list 'bad 'variable nom-arg)
                                    :missing-or-extra (list 'missing 'or 'extra 'expression nom-arg)
                                    :wrong-type-arg (list 'Wrong 'type 'in 'arg nom-arg)
                                    :wrong-type-arg1 (list 'Wrong 'type 'in 'arg1 nom-arg)
                                    :wrong-type-arg2 (list 'Wrong 'type 'in 'arg2 nom-arg)
                                    ())))))

; FUNCIONES QUE DEBEN SER IMPLEMENTADAS PARA COMPLETAR EL INTERPRETE DE SCHEME (ADEMAS DE COMPLETAR `EVALUAR` Y `APLICAR-FUNCION-PRIMITIVA`):

; LEER-ENTRADA:
; user=> (leer-entrada)
; (hola
; mundo)
; "(hola mundo)"
; user=> (leer-entrada)
; 123
; "123"
;; (defn leer-entrada
;;   "Lee una cadena desde la terminal/consola. Si los parentesis no estan correctamente balanceados al presionar Enter/Intro,
;;    se considera que la cadena ingresada es una subcadena y el ingreso continua. De lo contrario, se la devuelve completa."
;; )

; user=> (verificar-parentesis "(hola 'mundo")
; 1
; user=> (verificar-parentesis "(hola '(mundo)))")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) )")
; 0
(defn verificar-parentesis
  "Cuenta los parentesis en una cadena, sumando 1 si `(`, restando 1 si `)`. Si el contador se hace negativo, para y retorna -1."
  [arg]
  (reduce
   (fn [a b]
     (let [b-eval (or ({\( 1, \) -1} b) 0), total (+ a b-eval)]
       (if (< total 0) (reduced -1) total)))
   0 arg))

(defn actualizar-amb
  "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor. 
  Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza la nueva informacion."
  [amb clave valor]
  (cond
    (not (symbol? clave)) (throw (AssertionError. "Worng key type."))
    (error? valor) amb
    :else (apply concat (assoc (apply array-map amb) clave valor))))


(defn buscar
  "Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
   y devuelve el valor asociado. Devuelve un error :unbound-variable si no la encuentra."
  [clave amb]
  (let [valor ((apply array-map amb) clave)]
    (if (nil? valor) (generar-mensaje-error :unbound-variable clave) valor)))

(defn error?
  "Devuelve true o false, segun sea o no el arg. una lista con `;ERROR:` o `;WARNING:` como primer elemento."
  [arg]
  (and (seq? arg)
       (let [primer-elemento (first arg)]
         (or (= (symbol ";ERROR:") primer-elemento) (= (symbol ";WARNING:") primer-elemento)))))

(defn proteger-bool-en-str
  "Cambia, en una cadena, #t por %t y #f por %f (y sus respectivas versiones en mayusculas), para poder aplicarle read-string."
  [arg]
  (clojure.string/replace arg #"#" "%"))

(defn restaurar-bool
  "Cambia, en un codigo leido con read-string, %t por #t y %f por #f (y sus respectivas versiones en mayusculas)."
  [arg]
  (prewalk-replace {'%T (symbol "#T"), '%t (symbol "#t"), '%f (symbol "#f"), '%F (symbol "#F")} arg))

(defn lower-case-arg
  "Devuelve el lower-case en caso que arg sea un string, symbol. En otro caso devuelve arg."
  [arg]
  (if (or (symbol? arg) (string? arg)) (lower-case arg) arg))

(defn igual?
  "Verifica la igualdad entre dos elementos al estilo de Scheme (case-insensitive)"
  [arg1 arg2]
  (and (= (type arg1) (type arg2)) (= (lower-case-arg arg1) (lower-case-arg arg2))))

(defn fnc-append
  "Devuelve el resultado de fusionar listas."
  [lista]
  (let [invalid-args (filter (fn [x] (not (list? x))) lista)]
    (cond
      (seq invalid-args) (generar-mensaje-error :wrong-type-arg 'append (first invalid-args))
      :else (apply concat lista))))

(defn boolean-to-symbol
  "Traduce los booleanos a simbolos. true -> #t, false -> #f
  Devuelve el arg si no es booleano."
  [arg]
  (let [map-result ({true (symbol "#t"), false (symbol "#f")} arg)]
    (if (nil? map-result) arg map-result)))

(defn symbol-to-boolean
  "Traduce los simbolos a booleanos. #t -> true, #f -> false.
   Devuelve el arg si no es booleano."
  [arg]
  (let [map-result ({(symbol "#t") true , (symbol "#f") false} arg)]
    (if (nil? map-result) arg map-result)))

(defn fnc-equal?
  "Compara elementos. Si son iguales, devuelve #t. Si no, #f."
  [lista]
  (let [primer-elemento (first lista)]
    (boolean-to-symbol
     (apply = true (map (fn [x] (igual? x primer-elemento)) (rest lista))))))

; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read '(1))
; (;ERROR: read: Use of I/O ports not implemented)
; user=> (fnc-read '(1 2))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
; user=> (fnc-read '(1 2 3))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
;; (defn fnc-read
;;   "Devuelve la lectura de un elemento de Scheme desde la terminal/consola."
;; )


(defn fnc-sumar
  "Suma los elementos de una lista."
  [lista]
  (let [arg1 (first lista), invalid-args (filter (fn [n] (not (number? n))) lista)]
    (cond
      (empty? lista) 0
      (not (number? arg1)) (generar-mensaje-error :wrong-type-arg1 '+ arg1)
      (seq invalid-args) (generar-mensaje-error :wrong-type-arg2 '+ (first invalid-args))
      :else (reduce + lista))))

(defn fnc-restar
  "Resta los elementos de un lista."
  [lista]
  (let [arg1 (first lista), invalid-args (filter (fn [n] (not (number? n))) lista)]
    (cond
      (empty? lista) (generar-mensaje-error :wrong-number-args-oper '-)
      (not (number? arg1)) (generar-mensaje-error :wrong-type-arg1 '- arg1)
      (seq invalid-args) (generar-mensaje-error :wrong-type-arg2 '- (first invalid-args))
      (empty? (rest lista)) (- arg1)
      :else (reduce - lista))))

(defn evaluar-lista-numerica
  "Aplica la funcion f tomando los elementos de la lista como argumentos.
   Devuelve #t en el caso que la lista este vacía.
   Valida los valores de la lista sean numéricos."
  [lista f]
  (let [arg1 (first lista), invalid-args (filter (fn [n] (not (number? n))) lista)]
    (cond
      (empty? lista) (boolean-to-symbol true)
      (not (number? arg1)) (generar-mensaje-error :wrong-type-arg1 f arg1)
      (seq invalid-args) (generar-mensaje-error :wrong-type-arg2 f (first invalid-args))
      :else (boolean-to-symbol (apply (resolve f) lista)))))

(defn fnc-menor
  "Devuelve #t si los numeros de una lista estan en orden estrictamente creciente; si no, #f."
  [lista]
  (evaluar-lista-numerica lista '<))

(defn fnc-mayor
  "Devuelve #t si los numeros de una lista estan en orden estrictamente decreciente; si no, #f."
  [lista]
  (evaluar-lista-numerica lista '>))

(defn fnc-mayor-o-igual
  "Devuelve #t si los numeros de una lista estan en orden decreciente; si no, #f."
  [lista]
  (evaluar-lista-numerica lista '>=))

(defn evaluar-escalar
  "Evalua una expresion escalar. Devuelve una lista con el resultado y un ambiente."
  [escalar amb]
  (if (symbol? escalar) (list (buscar escalar amb) amb) (list escalar amb)))

(defn resultado-evaluar
  "Devuelve el resultado de evaluar exp amb."
  [amb exp]
  (first (evaluar exp amb)))

(defn evaluar-define
  "Evalua una expresion `define`. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
  [exp amb]
  (try
    (cond
      (< (count exp) 3) (list (generar-mensaje-error :missing-or-extra 'define exp) amb)
      (seq? (second exp)) ; syntactic sugar for lambda.
      (let [f-args (rest (second exp)), f-cuerpo (drop 2 exp), f-nombre (first (second exp))]
        (list (symbol "#<unspecified>") (actualizar-amb amb f-nombre (concat (list 'lambda f-args) f-cuerpo))))
      (not= (count exp) 3) (list (generar-mensaje-error :missing-or-extra 'define exp) amb)
      :else (list (symbol "#<unspecified>") (actualizar-amb amb (second exp) (nth exp 2))))
    (catch AssertionError e
      (list (generar-mensaje-error :bad-variable 'define exp) amb))))

; user=> (evaluar-if '(if 1 2) '(n 7))
; (2 (n 7))
; user=> (evaluar-if '(if 1 n) '(n 7))
; (7 (n 7))
; user=> (evaluar-if '(if 1 n 8) '(n 7))
; (7 (n 7))
; user=> (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))
; (8 (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 9 #f #f))
; user=> (evaluar-if '(if) '(n 7))
; ((;ERROR: if: missing or extra expression (if)) (n 7))
; user=> (evaluar-if '(if 1) '(n 7))
; ((;ERROR: if: missing or extra expression (if 1)) (n 7))
;; (defn evaluar-if
;;   "Evalua una expresion `if`. Devuelve una lista con el resultado y un ambiente eventualmente modificado."
;; )


(defn evaluar-or
  "Evalua una expresion `or`.  Devuelve una lista con el resultado y un ambiente."
  [exp amb]
  (let [args (map symbol-to-boolean (map (partial resultado-evaluar amb) (rest exp)))]
    (list (boolean-to-symbol (reduce #(or %1 %2) false args)) amb)))

(defn evaluar-set!
  "Evalua una expresion `set!`. Devuelve una lista con el resultado y un ambiente actualizado con la redefinicion."
  [exp amb]
  (let [clave (second exp), valor (last exp)]
    (cond
      (not= 3 (count exp)) (list (generar-mensaje-error :missing-or-extra 'set! exp) amb)
      (not (symbol? clave))  (list (generar-mensaje-error :bad-variable 'set! clave) amb)
      (error? (buscar (second exp) amb)) (list (generar-mensaje-error :unbound-variable clave) amb)
      :else (list (symbol "#<unspecified>") (actualizar-amb amb clave (resultado-evaluar amb valor))))))

; Al terminar de cargar el archivo en el REPL de Clojure, se debe devolver true.

