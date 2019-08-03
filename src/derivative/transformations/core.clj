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
;  :original <nil|
;             string|
;             vector of strings|
;             input stream|
;             reader|
;             byte array|
;             character array>
;  :modifications [
;    {:coordinates [:remove [0,0] :add [1,4]]
;     :lines [
;       {:add ""}
;       {:add ""}
;       {:add ""}
;       {:add ""}
;     ]
;    }
;  ]
;  :mode <nil|mode>}
; {:operation :delete
;  :path <path>
;  :original <nil|
;             string|
;             vector of strings|
;             input stream|
;             reader|
;             byte array|
;             character array>
;  :modifications [
;    {:coordinates [:remove [1,4] :add [0,0]]
;     :lines [
;       {:remove ""}
;       {:remove ""}
;       {:remove ""}
;       {:remove ""}
;     ]
;    }
;  ]
;  :mode <nil|mode>}
; {:operation :modify-content
;  :path <path>
;  :original <nil|
;             string|
;             vector of strings|
;             input stream|
;             reader|
;             byte array|
;             character array>
;  :modifications [
;    {:coordinates [:remove [0,0] :add [1,4]]
;     :lines [
;       {:keep ""}
;       {:add ""}
;       {:remove ""}
;       {:keep ""}
;     ]
;    }
;  ]
;  :mode <nil|mode>}
; {:operation :modify-mode
;  :path <path>
;  :original <nil|
;             string|
;             vector of strings|
;             input stream|
;             reader|
;             byte array|
;             character array>
;  :modifications [
;    {:coordinates [:remove [0,0] :add [1,4]]
;     :lines [
;       {:keep ""}
;       {:add ""}
;       {:remove ""}
;       {:keep ""}
;     ]
;    }
;  ]
;  :mode <nil|mode>}
;
