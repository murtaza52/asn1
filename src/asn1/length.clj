(ns asn1.length)

(defn add-big-endians
  [v1 v2]
  (bit-or (bit-shift-left v1 8) v2))

(defn short-form-length?
  "If the 8th bit is 0 then it is short form."
  [v]
  (= 0 (bit-shift-right v 7)))

(defn long-form-length?
  "If the 8th bit is 1 then its long form."
  [v]
  (= 1 (bit-shift-right v 7)))

(defn long-form-num-of-octets
  "The 0-6 bits represent the num of octets"
  [v]
  (bit-and v 2r01111111))

(defn calculate-long-form-length
  [length-octets]
  (reduce add-big-endians length-octets))

(defn get-length
  "Returns the length (number of content-octets) and number of length-contents. Does not support indefinite length."
  [[f & r]]
  (cond
    (short-form-length? f) {:length-octets 1
                            :length f}
    (long-form-length? f) (let [num-of-octets (long-form-num-of-octets f)
                                length (calculate-long-form-length (take num-of-octets r))]
                            {:length-octets (+ 1 num-of-octets)
                             :length length})))

(comment
  (get-length [0x82 0x03 0x0D])
  (calculate-long-form-length [0x03 0x0D])
  (long-form-num-of-octets 0x82)
  (long-form-length? 0x82)
  (add-big-endians 0x03 0x0d))
