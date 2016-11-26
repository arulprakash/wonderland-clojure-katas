(ns alphabet-cipher.coder)

(def letters (vec (map str '[a b c d e f g h i j k l m n o p q r s t u v w x y z])))

(defn get-codes
  [letter]
  (let [number (.indexOf letters letter)]
    (vec (flatten (conj (take number letters) (drop number letters))))))

(defn encode-letter
  [k m]
  (get (get-codes k) (.indexOf letters m)))

(defn decode-letter
  [k m]
  (get letters (.indexOf (get-codes k) m)))

(defn encode [keyword message]
  (let [kseq (cycle (map str (vec keyword)))
        mseq (map str (vec message))]
    (reduce str (map encode-letter kseq mseq))))

(defn decode [keyword message]
  (let [kseq (cycle (map str (vec keyword)))
        mseq (map str (vec message))]
    (reduce str (map decode-letter kseq mseq))))

(defn decipher [cipher message]
  (let [kw (decode message cipher)]
    (reduce str (take (first (filter #(= (take % kw) (take % (drop % kw))) (range 1 (count kw)))) kw))))

