(ns code-katas-2.core)


(defn unpartial
  "Escribir una funcion que acepte una funcion parcial con cantidad de argumentos desconocida,
   retornar una funcion equivalente de n argumentos"
  [f]
  (fn [& args]
    (let [r (f (first args))]
      (if (fn? r)
        (apply (unpartial r) (rest args))
        r))))


(defn search
  "Dado un numero cualquiera de secuencias, cada una ya ordenada de menor a mayor, encontrar el numero
   mas chico que aparezca en todas las secuencias, las secuencias pueden ser infinitas."
  [& seqs]
  (letfn [(lazy-search [xs]
            (let [s (map first xs)
                  m (apply min s)]
              (if (apply = s)
                m
                (lazy-search (map #(if (= (first %) m) (rest %) %) xs)))))]
    (lazy-search seqs)))


(defn intercalar
  "Escriba una funcion que tome un predicado de 2 argumentos, un valor y una coleccion, y
   retorne una nueva coleccion donde el valor es insertado intercalado cada dos argumentos
   que cumplan el predicado"
  ;;  [predicado valor secuencia]
  [pred value col]
  (letfn [(looper [s head]
            (lazy-seq
             (if-let [sec (first s)]
               (if (pred head sec)
                 (cons head (cons value (looper (rest s) sec)))
                 (cons head (looper (rest s) sec)))
               (if head [head] []))))]
    (looper (rest col) (first col))))



(defn tartamudeo
  "Escriba una funcion que retorne una secuencia lazy que comprima el tartamudeo de una secuencia de numeros.
   Comprimir el tartamudeo se refiere a que [1 1 1] se exprese como [3 1] y a su vez [3 1] se exprese como [1 3 1 1].

   La funcion debe aceptar una secuencia inicial de numeros, y devolver una secuencia infinita de compresiones, donde
   cada nuevo elemento es el elemento anterior comprimido."
  ;[secuencia]
  [s]
  (letfn [(pronuntiate [l]
            (apply concat (map (juxt count first) (partition-by identity l))))]
    (iterate pronuntiate (pronuntiate s))))
