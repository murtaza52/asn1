(ns asn1.tags)

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

(def constructed-type? #{:sequence :set :context-specific-tag})

(defn get-tag-type
  [v]
  (if (context-specific-tag? v)
    :context-specific-tag
    (get
     (merge basic-types string-types constructed-types misc)
     v
     {:tag-not-found-for-hex (Long/toHexString v)})))

(comment
  (context-specific-tag? 0xA0)
  (context-specific-tag? 0x06)
  (get-tag-type 0x30)
  (get-tag-type 0xA0))

