(ns code-katas-1.core)

(defn filter-odd
  "Escribir una funcion que retorne solamente los numeros impares de
   una secuencia"
  [s]
  (filter odd? s))

(defn nil-key
  "Escribir una funcion que dada una clave y un mapa, devuelva true, solamente si el mapa
   contiene una entrada con esa clave, y su valor es nil"
  [k m]
  (and (contains? m k) (nil? (get m k))))

(defn range
  "Escribir una funcion que cree una lista de enteros en un rango dado.
   Restricciones: range"
  [start end]
  (if (= start end)
    '()
    (cons start (range (+ start 1) end))))

(defn compress-sequence
  "Escribir una funcion que elimine los duplicados consecutivos
   de una secuencia"
  [s]
  (reduce (fn [x, y] (if (= (last x) y)
                       x
                       (conj x y)))
          []
          s))

(defn max-value
  "Escribir una funcion que reciba un numero variable de parametros
   y retorne el que tenga el valor mayor
   Restricciones: max y max-key"
  [& args]
  (first (sort > args))
  ;o (reduce #(if (> %1 %2) %1 %2) args)
  )

(defn split-two
  "Escribir una funcion que parta una secuencia en dos partes
   Restricciones: split-at"
  [length s]
  [(take length s) (drop length s)])

(defn inter-two
  "Escribir una funcion que reciba dos secuencias y retorne el primero de cada una,
   luego el segundo de cada una, luego el tercero, etc.
   Restricciones: interleave"
  [s1 s2]
  (when (and (seq s1) (seq s2))
    (cons (first s1)
          (cons (first s2) (inter-two (rest s1) (rest s2))))))

(defn retrieve-caps
  "Escribir una funcion que reciba un string y devuelva un nuevo string conteniendo
   solamente las mayusculas."
  [text]
  (apply str (filter #(Character/isUpperCase %) text)))

(defn find-truth
  "Escribir una funcion que tome un numero variable de booleans, y devuelva true
   solamente si alguno de los parametros son true, pero no todos son true. En otro
   caso debera retornar false"
  [& xs]
  (and (some true? xs) (some false? xs)))

(defn zip-map
  "Escribir una funcion que reciba un vector de claves y un vector de valores, y
   construya un mapa a partir de ellos.
   Restricciones: zipmap"
  [k v]
  (reduce #(assoc %1 (%2 0) (%2 1)) {} (map vector k v)))
