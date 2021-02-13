(ns testing_functions)

(defn eliminar-cero-entero [n]
  (println (type n))
  (cond
    (integer? n ) (n)
    (float? n ) (if (zero? (n - int (Math/floor (n))))) (int (Math/floor (n)))
  )
)

(defn eliminar-cero-entero [n]
  (cond
    (nil? n) nil
    (symbol? n) (str n)
    (integer? n ) (srt n)
    (zero? n ) (str n)
    (and (> n 0) (< n 1)) (apply str (nthrest (seq (str n)) 1)) ;sacar esto
    (>= n 1) (str n)
    (and (neg? n) (> n -1)) (apply str "-" (nthrest (seq (str n)) 2))
    :else (str n)
    )
)

(defn eliminar-cero-decimal [n]
  (cond
    (nil? n) nil
    (symbol? n) (str n)
    (zero? n) (str n)
    (>= n 1) (read-string (apply str (seq (str n))))
  )
)

(def testVMap
  [ {:name "AAA" :rate 100 }
   {:name "GEICO" :rate 120 }
   {:name "PROGRESSIVE" :rate 118} ] )

(defn aridad [token]

)

;;;;; pruebas
(eliminar-cero-entero 3)

(defn eliminar-cero-entero [n]
  (println (type n))
  (cond
    (integer? n ) (n)
    (float? n ) (if (zero? (n - int (Math/floor (n))))) (int (Math/floor (n)))
    )
  )

(defn eliminar-cero-entero [n]
  (cond
    (nil? n) nil
    (symbol? n) (str n)
    (= n 0) (str n)
    (and (> n 0) (< n 1)) (apply str (nthrest (seq (str n)) 1))
    (>= n 1) (str n)
    (and (neg? n) (> n -1)) (apply str "-" (nthrest (seq (str n)) 2))
    :else (str n)
    )
  )
; => #'user/eliminar-cero-entero
(eliminar-cero-entero 3)
; => "3"
(eliminar-cero-entero 3.4)
; => "3.4"
(eliminar-cero-entero 3.00)
; => "3.0"
(eliminar-cero-entero 3.000000)
;=> "3.0"
(eliminar-cero-entero 'alfkndl)
;=> "alfkndl"
(eliminar-cero-entero 0.5)
;=> ".5"
(seq 0.50000)

(seq (str 0.50000))
; => (\0 \. \5)
(apply str(seq (str 0.50000)))

(read-string (apply str(seq (str 0.50000))))

(def testVMap
  [ {:name "AAA" :rate 100 }
   {:name "GEICO" :rate 120 }
   {:name "PROGRESSIVE" :rate 118} ] )

(some (comp (partial = "AAA") :name) testVMap)
(some #(if (= "GEICO" (:name %)) %) testVMap)
(map #(if (= "GEICO" (:name %))
        (update % :rate inc)
        %)
     testVMap)

;({:name "AAA", :rate 100} {:name "GEICO", :rate 121} {:name "PROGRESSIVE", :rate 118})
(def operador
  [ {:name "*" }
   {:name "+" }
   {:name "/" } ] )

(some (comp (partial = "+") :name) operador)
(if (some (comp (partial = "+") :name) operador) (str 2))
(if (some (comp (partial = "+") :name) operador) (str 2))
(if (some (comp (partial = "+") :name) operador) (str 2))
(if (some (comp (partial = "+") :name) operador) (str 2) :else str "none")
(if (some (comp (partial = "+") :name) operador) (str 2) )

   (if (some (comp (partial = "+") :name) operador) (str 2) )
   (some (comp (= "+") :name) operador)
   (some ((partial = "+") :name) operador)
   (some (comp (partial = "+") :name) operador)



;Definir la función tercer-angulo que reciba los valores de dos de los ángulos interiores de un triángulo y devuelva el valor del restante.
(defn tercer-ang [primer segundo]
  (if (and (number? primer) (number? segundo))
    (- 180 (+ primer segundo))
    (+ 0))
  )

; ej 7
(defn invertir [nro]
  (if (number? nro)
    ( (apply str(reverse (str nro))))
    (nro))
  )

; ej 5
(defn capicua? [x] (= (seq (str x)) (reverse (str x))))
; con validaciones
(defn capicua? [numero]
  (cond
    (not (int? numero)) "ingrese un numero entero"
    (not (pos? numero))  "ingrese un numero positivo"
    (> numero 99999) "el numero debe ser menor a 5 digitos"
    :else (= (str numero) (apply str (reverse (str numero))))
    ))

; ej 9
(defn cant-dig [nro]
  (if (number? nro)
    (count (seq (str nro)))
    ("no es un numero"))
)

;con validacion
(defn cant-dig [n] (cond
  (not (integer? n)) "Error: el numero no es entero"
  (= n 0) 1
  (not (pos? n)) (- (count (str n)) 1)
  :else (count (str n)))
)

; ej 10
(defn pot? [x y](cond
                  (= x y) true
                  (> x y) (pot? (/ x y) y)
                  (< x y) false
                  :else "Error"
                  )
  )
; ej 11
(defn digs [n] (cond
                 (not (integer? n)) "Error: el numero no es entero"
                 (not (pos? n)) (seq (str (- 0 n)))
                 :else (seq (str n))
                 )
  )

;15
(defn elim [dato l ] ( cond
   ( empty? l) l
   ( coll? (first l) ) (cons (elim dato (first l)) ( elim dato (rest l)))
   ( = dato (first l)) (elim dato (rest l))
   :else (cons (first l) (elim dato (rest l)))
   )
)

;21 y 22
;21
(map drop (range) matriz)

;22
(map nth matriz (range))