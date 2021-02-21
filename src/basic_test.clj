(require '[clojure.test :as t])
(use 'clojure.test)
(ns-imports 'basic)
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
(is (= 5 (abs 5)))
(use 'clojure.test)
(use 'basic :reload)
(deftest eg-tests (is (= 1 1)))
(run-tests)