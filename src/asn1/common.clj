(ns asn1.common)

(defn bit-7?
  "Returns true if the 7th bit of the byte is set to 1."
  [b]
  (= 2r10000000 (bit-and 2r10000000 b)))

(defn to-hex
  [coll]
  (map #(Long/toHexString %) coll))

(comment
  (bit-7? 0x82)
  (to-hex '(42 134 72 134 247 13 1 1 11)))
