(ns derivative.transformations.core)

(defmulti apply-transformation
  (fn [transformation _] (:type transformation)))

#_(defprotocol FileSystemTransformation
  (plan [this configuration options])                       ; => diffs
  (apply [this configuration options])                      ; => updates

  (inputs [this])
  (effects [this]))

; content scopes:
;   #{:ast, :file, :line, :none}
; attribute of definition? attribute of transformation?
; maybe default on definition, override on transformation?

; for now, load entire file?

; only want to load it once... therefore, should do so at start of pipeline and
; pass in to transformation

; maybe pass in reader and wrap reader with mapper for content transformations

; {:operation :create
;  :path <path>
;  :modifications [
;    {:coordinates [0,0]
;        <string,
;            vector of strings,
;            input stream,
;            reader,
;            byte array,
;            character array>
;  ]
;  :mode <mode>}
; {:operation :modify
;  :path <path>
;  :modifications
