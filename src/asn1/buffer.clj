(ns asn1.buffer
  (:require [clojure.string  :as str]
            [clojure.java.io :as io]
            [clojure.pprint  :refer [pprint]])
  (:import java.io.RandomAccessFile
           java.nio.ByteBuffer
           java.util.Base64))

(defn read-lines
  [path]
  (doall (line-seq (io/reader path))))

(read-lines "resources/public.crt")

(defn comment?
  [line]
  (str/starts-with? line "----"))

(defn remove-comments
  [coll]
  (reduce str "" (remove comment? coll)))

(remove-comments (read-lines "resources/public.crt"))

(defn base64-bytes
  [^String b64-str]
  (.decode (java.util.Base64/getDecoder) b64-str))

(base64-bytes (remove-comments (read-lines "resources/public.crt")))

(defn bytes->buffer
  [b64-bytes]
  (ByteBuffer/wrap b64-bytes))

(defn buffer->int-seq
  [bb]
  (for [i (range (.limit bb))]
    (.get bb)))

(defn file->int-seq
  [path]
  (->> path
       read-lines
       remove-comments
       base64-bytes
       bytes->buffer
       buffer->int-seq
       (map (fn [v] (bit-and v 0xff)))))


(comment
  (def data (file->int-seq "resources/public.crt"))
  (take 5 data)
  )

;; look into clojure io and understand buffers etc
