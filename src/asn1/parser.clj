(ns asn1.parser
  (:require [clojure.pprint :refer [pprint]]
            [asn1.tags :refer [get-tag-type constructed-type?]]
            [asn1.length :refer [get-length]]
            [asn1.value :refer [parse-string parse-bit-string parse-integer parse-octet-string parse-utc-time]]
            [asn1.buffer :refer [file->seq]]
            [asn1.oid :refer [get-oid-mapping]])
  (:gen-class))

(defn get-triplet
  "Given a collection of bytes, returns a vector containing a map which represents the first triplet, the remaining octets and the triplet octets (all the tlv octets)."
  [[f & r]]
  (let [{:keys [length-octets length]} (get-length r)
        triplet                        {:tag     (get-tag-type f)
                                        :length  length
                                        :value (take length (drop length-octets r))}
        remaining-octets               (drop (+ length-octets length) r)
        triplet-octets                 (take (+ 1 length-octets length) (cons f r))]
    [triplet remaining-octets triplet-octets]))

(declare parse-tlv)

(defn get-triplet-seq
  "This is useful for parsing the sequence of triplets in a constructed type."
  [coll]
  (loop [coll coll
         acc  []]
    (let [[{:keys [tag length] :as m} remaining-octets triplet-octets] (get-triplet coll)
          triplets                                                     (if (constructed-type? tag)
                                                                         [(update m :value parse-tlv)]
                                                                         (parse-tlv triplet-octets))]
      (if (not (seq remaining-octets))
        (concat acc triplets)
        (recur remaining-octets (concat acc triplets))))))

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
    :octet-string (parse-octet-string value)
    :null nil
    value))

(defn parse-tlv
  [coll]
  (loop [coll coll
         acc  []]
    (let [[{:keys [tag length value]} remaining-octets] (get-triplet coll)]
      (let [triplet (assoc {:tag tag :length length} :value (parse-value tag value))]
        (if (not (seq remaining-octets))
          (conj acc triplet)
          (recur remaining-octets (conj acc triplet)))))))

(def parse-asn1 (comp parse-tlv file->seq))

(def parse-and-print (comp pprint parse-asn1))

(defn -main [& args]
  (if-let [file-path (first args)]
    (parse-and-print file-path)
    (binding [*out* *err*]
      (println "No path given.")
      (System/exit 1))))

(comment
  (get-triplet-seq '(6 9 42 134 72 134 247 13 1 1 11 5 0))
  (get-triplet '(6 9 42 134 72 134 247 13 1 1 11 5 0))
  (parse-tlv [0x30 8 0x30 6 0x02 1 5 0x02 1 53])
  (parse-and-print "resources/keys/rsa.key")
  (parse-and-print "resources/keys/ec.pem")
  (parse-and-print "resources/keys/rsa_public.crt"))
