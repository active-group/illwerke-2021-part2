;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname river) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Ein Fluss ist einer der folgenden:
; - kommt aus einer Quelle - ODER -
; - fließt aus zwei Flüssen zusammen: Hauptfluss und Nebenfluss
;                                          ^^^^^          ^^^^^
;                                          Selbstreferenz

; Ein Fluss ist einer der folgenden:
; - ein Bach aus einer Quelle - ODER -
; - ein Zusammentreffen von Hauptfluss und Nebenfluss
(define river
  (signature (mixed creek confluence)))

; Ein Bach hat folgende Eigenschaften:
; - Ursprungsort
(define-record creek
  make-creek
  creek?
  (creek-origin string))

(define eschach (make-creek "Heimliswald"))
(define prim (make-creek "Dreifaltigkeitsberg"))

(define-record confluence
  make-confluence
  confluence?
  (confluence-location string)
  (confluence-main-stem river) ; Selbstbezug
  (confluence-tributary river)) ; Selbstbezug

(define neckar1 (make-confluence "Rottweil" eschach prim))
(define schlichem (make-creek "Tieringen"))
(define neckar2 (make-confluence "Epfendorf" neckar1 schlichem))

; Fließt Wasser von einem bestimmten Ort in Fluss?
(: flows-from? (string river -> boolean))

(check-expect (flows-from? "Heimliswald" eschach) #t)
(check-expect (flows-from? "Heimliswald" neckar2) #t)
(check-expect (flows-from? "Heimliswald" prim) #f)
(check-expect (flows-from? "Berlin" neckar2) #f)

(define flows-from?
  (lambda (location river)
    (match river
      ((make-creek origin)
       (string=? origin location))
      ((make-confluence clocation main-stem tributary)
       (or (string=? clocation location)
           (flows-from? location main-stem) ; Selbstbezug
           (flows-from? location tributary))))))

