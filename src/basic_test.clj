(require '[clojure.test :refer [is deftest run-tests]])

(load-file "basic.clj")

(deftest test-palabra-reservada?
  (is (= true (palabra-reservada? 'REM)))
  (is (= false (palabra-reservada? 'SPACE)))
  )

(deftest test-operador?
  (is (= true (operador? '+)))
  (is (= true (operador? (symbol "+"))))
  (is (= false (operador? (symbol "%"))))
  )

(deftest palabra-reservada?
  (is true (palabra-reservada? 'REM))
  (is false (palabra-reservada? 'SPACE))
)


(t/is true (operador? '+))

(deftest add-nums
  (is (= 2 (+ 1 1)))
  (is (= 3 (+ 1 2)))
  )

(deftest buscar-lineas-restantes
  (t/is (= (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])))
  )


(run-tests)
