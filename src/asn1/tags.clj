(ns asn1.tags
  (:require [asn1.common :refer [to-hex]])
  (:import java.time.Instant
           java.time.Year))

(def basic-types
  {0x03 :bit-string
   0x01 :boolean
   0x02 :integer
   0x05 :null
   0x06 :object-identifier
   0x04 :octet-string})

(def string-types
  {0x1E :bmp-string
   0x16 :ia5-string
   0x13 :printable-string
   0x14 :teletex-string
   0x0C :utf8-string})

(def constructed-types
  {0x30 :sequence
   0x31 :set})

(def misc
  {0x17 :utc-time})

(defn context-specific-tag?
  [v]
  (= 2r10 (bit-shift-right v 6)))

(context-specific-tag? 0xA0)
;; => true

(context-specific-tag? 0x06)
;; => false

(defn get-tag-type
  [v]
  (if (context-specific-tag? v)
    :context-specific-tag
    (get
     (merge basic-types string-types constructed-types misc)
     v
     {:tag-not-found-for-hex (Long/toHexString v)})))

(get-tag-type 0x30)
;; => :sequence

(get-tag-type 0xA0)
;; => :context-specific-tag

(def constructed-type? #{:sequence :set :context-specific-tag})

(defn add-big-endians
  [v1 v2]
  (bit-or (bit-shift-left v1 8) v2))

(add-big-endians 0x03 0x0d)

(defn short-form-length?
  "If the 8th bit is 0 then it is short form."
  [v]
  (= 0 (bit-shift-right v 7)))

(defn long-form-length?
  "If the 8th bit is 1 then its long form."
  [v]
  (= 1 (bit-shift-right v 7)))

(long-form-length? 0x82)

(defn long-form-num-of-octets
  "The 0-6 bits represent the num of octets"
  [v]
  (bit-and v 2r01111111))

(long-form-num-of-octets 0x82)
;; => 2

(defn calculate-long-form-length
  [length-octets]
  (reduce add-big-endians length-octets))

(calculate-long-form-length [0x03 0x0D])

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

(get-length [0x82 0x03 0x0D])
;; => {:length-octets 3, :length 781}

(defn parse-integer
  [coll]
  (biginteger (byte-array coll)))

(def integer-data [0x29 0x42 0xde 0x86 0xc2 0x0a 0xcc 0xfb 0x96 0x9f 0x24 0x5f 0xa6 0x8c 0x65 0x1b 0x5d 0x1f 0x88 0xf1])

(parse-integer integer-data)
;; => 235559855570120771496712017082493683142938822897

(def printable-string-data [0x55 0x47])

(defn parse-string
  [coll]
  (String. (byte-array coll)))

(parse-string printable-string-data);; => "UG"

(def utf8-string-data '(75 97 109 112 97 108 97))

(parse-string utf8-string-data)
;; => "Kampala"

(defn parse-bit-string
  [coll]
  (apply str (map #(Long/toBinaryString %) coll)))

(defn get-this-year
  []
  (Integer/parseInt (.toString (Year/now))))

;; code taken from https://github.com/dankreek/clj-asn1/blob/master/src/clj_asn1/core.clj
(defn calculate-century
  [year-end]
  (let [this-year  (get-this-year)
        high-bound (+ this-year 49)
        this-cent  (Integer/parseInt (.substring (str this-year) 0 2))]
    (if (< high-bound (+ (* 100 this-cent) year-end))
      (dec this-cent)
      this-cent)))

(calculate-century 19)

(defn format-date
  [coll]
  (let [year-end (Integer/parseInt (first coll))
        century (calculate-century year-end)]
    (apply format "%s%s-%s-%sT%s:%s:%s.Z" (cons century coll))))

(defn parse-utc-time
  [coll]
  (->> coll
       (partition 2)
       (map #(parse-string %))
       format-date
       (Instant/parse)))

(def utc-time-data '(50 48 49 48 50 51 49 52 49 53 52 49 90))

(parse-utc-time utc-time-data)

(defn parse-octet-string
  [coll]
  (->> coll
       to-hex
       (apply str)))

(def octet-string-data '(87 241 49 245 166 29 26 131 118 204 195 116 144 85 15 4 120 55 28 195 29 18 181 73 238 181 186 205 123 193 94 227))

(parse-octet-string octet-string-data);; => "57f131f5a61d1a8376ccc3749055f478371cc31d12b549eeb5bacd7bc15ee3"
