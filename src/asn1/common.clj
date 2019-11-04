(ns asn1.common)

;; get all the bit numbering right

(defn bit-7?
  "Returns true if the 7th bit of the byte is set to 1."
  [b]
  (= 2r10000000 (bit-and 2r10000000 b)))

(bit-7? 2r00000001)
(bit-7? 2r10010010)
(bit-7? 0x48)

(bit-7? 0x82)
;; => true

(defn to-hex
  [coll]
  (map #(Long/toHexString %) coll))

(to-hex '(42 134 72 134 247 13 1 1 11))
;; => ("2a" "86" "48" "86" "f7" "d" "1" "1" "b")

