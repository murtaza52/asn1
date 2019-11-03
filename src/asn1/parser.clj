(ns asn1.parser
  (:require [clojure.pprint :refer [pprint]]
            [asn1.common :refer [bit-7?]]
            [asn1.tags :refer :all]
            [asn1.oid :refer :all]
            [asn1.buffer :refer :all])
  (:gen-class))

(defn parse-asn1
  [bb]
  bb)

;; (defn -main [& args]
;;   (if-let [key-path (first args)]
;;     (pprint (parse-asn1 (bytes->buffer key-path)))
;;     (binding [*out* *err*]
;;       (println "no path given")
;;       (System/exit 1))))

{:tag-type :sequence :length 1 :value 1}

(comment (def data ["30" "23" "31" "0f" "30" "0d" "06" "03" "55" "04" "03" "13" "06" "54" "65" "73" "74" "43" "4e" "31" "10" "30" "0e" "06" "03" "55" "04" "0a" "13" "07" "54" "65" "73" "74" "4f" "72" "67"]))

(defn parse-asn1
  [coll]
  (loop [[f & r] coll
         acc     {}]))

(comment
  (loop [[a b & c] [1 2 3 4 5]
         acc       {:t a :l b :v 1}]
    (recur {})))

(comment
  (def a (file->int-seq "resources/public.crt"))
  (= (byte 0x30) (.getInt a))

  (def data-3 (file->int-seq "resources/public.crt")))

(def data-2 [0x30 0x82 0x03 0x0D 1 2 3 4])

(defn constructed-type?
  "Returns true if the 6th bit is set to 1."
  [v]
  (= 2r100000 (bit-and v 2r00100000)))

(constructed-type? 0x31)
;; => true

(constructed-type? 0x02)
;; => false

(def constructed-type? #{:sequence :set})

(defn get-triplet
  [[f & r]]
  (let [{:keys [length-octets length]} (get-length r)
        triplet                        {:tag     (int->tag-type f)
                                        :length  length
                                        :value (take length (drop length-octets r))}
        remaining-octets               (drop (+ length-octets length) r)
        triplet-octets                 (take (+ 1 length-octets length) (cons f r))]
    [triplet remaining-octets triplet-octets]))

(get-triplet '(6 9 42 134 72 134 247 13 1 1 11 5 0))
;; => [{:tag :object-identifier, :length 9, :value (42 134 72 134 247 13 1 1 11)} (5 0) (6 9 42 134 72 134 247 13 1 1 11)]

(declare parse-tlv)

(defn get-triplet-seq
  [coll]
  (loop [coll coll
         acc  []]
    (let [[{:keys [tag length] :as m} remaining-octets triplet-octets] (get-triplet coll)
          triplet                                                      (if (constructed-type? tag)
                                                                         [(update m :value parse-tlv)]
                                                                         (parse-tlv triplet-octets))]
      (if (not (seq remaining-octets))
        (concat acc triplet)
        (recur remaining-octets (concat acc triplet))))))

(get-triplet-seq '(6 9 42 134 72 134 247 13 1 1 11 5 0))
;; => ({:tag :object-identifier, :length 9, :value {:oid "1.2.840.113549.1.1.11", :oid-text :sha256WithRSAEncryption}} {:tag :null, :length 0, :value nil})

(defn parse-value
  [tag value]
  (case tag
    :sequence (get-triplet-seq value)
    :integer (parse-integer value)
    :object-identifier (get-oid-mapping value)
    :printable-string (parse-string value)
    :utf8-string (parse-string value)
    :bit-string (parse-bit-string value)
    :utc-time (parse-utc-time value)
    :null nil
    value))

(defn parse-tlv
  [coll]
  (loop [coll coll
         acc  []]
    (let [[{:keys [tag length value]} remaining-octets] (get-triplet coll)
          triplet                                         {:tag tag :length length}]
      (let [parsed-triplet (assoc triplet :value (parse-value tag value))]
        (if (not (seq remaining-octets))
          (conj acc parsed-triplet)
          (recur remaining-octets (conj acc parsed-triplet)))))))

:t    :l     :v
             :t   :l   :v
                       :t :l :v
(def d [0x30 8     0x30  6   0x02 1 5  0x02 1 53])

(parse-tlv d)
;; => [{:tag :sequence, :length 8, :value {:tag :sequence, :length 8, :value [{:tag :sequence, :length 6, :value [{:tag :integer, :length 1, :value (5)} {:tag :integer, :length 1, :value (53)}]}]}}]


(defn to-hex
  [coll]
  (map #(Long/toHexString %) coll))

(to-hex '(42 134 72 134 247 13 1 1 11))
;; => ("2a" "86" "48" "86" "f7" "d" "1" "1" "b")

(comment (clojure.pprint/pprint (parse-tlv data-3)))


