;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname animals) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Datenanalyse

; Datendefinition -> Code

; Datendefinition
; Ein Haustier ist eins der folgenden:  <-- Formulierung
; - Katze - ODER -
; - Hund - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
; "cat" "dog" "snake"
(define pet
  (signature (enum "cat" "dog" "snake")))

; Ist Haustier niedlich?
; C#: isCute
(: cute? (pet -> boolean))

(check-expect (cute? "cat") #t)
(check-expect (cute? "dog") #t)
(check-expect (cute? "snake") #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    ; pet ist eine Fallunterscheidung
    ; -> Verzweigung
    ; Zweig: (<Bedingung> <Antwort>)
    (cond
      ((string=? pet "cat") ...)
      ((string=? pet "dog") ...)
      ((string=? pet "snake") ...))))

#;(define cute?
  (lambda (pet)
    ; pet ist eine Fallunterscheidung
    ; -> Verzweigung
    ; Zweig: (<Bedingung> <Antwort>)
    (cond
      ((string=? pet "cat") #t)
      ((string=? pet "dog") #t)
      ((string=? pet "snake") #f))))

(define cute?
  (lambda (pet)
    ; pattern matching
    (match pet
      ("cat" #t)
      ("dog" #t)
      ("snake" #f))))

; Uhrzeit besteht aus: / hat folgende Eigenschaften:
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor / "Getter"
  (time-minute natural))

(define time1 (make-time 12 24))
(define time2 (make-time 16 05))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1) (+ (* 12 60) 24))
(check-expect (msm time2) (+ (* 16 60) 5))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Minuten seit Mitternacht zurück in Uhrzeit konvertieren
(: msm->time (natural -> time))

(check-expect (msm->time (+ (* 12 60) 24)) time1)

(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))

; Signaturverletzung:
;(msm->time 20.5)

;(msm->time -2)

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften: <-- zusammengesetzte Daten
; - lebendig oder tot - UND -
; - Gewicht
; Zustand des Gürteltiers zu einem bestimmten Zeitpunkt
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: dillo? (any -> boolean))

; Gürteltier, lebendig, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 9kg
(define dillo2 (make-dillo #f 9))

#|
// repräsentiert das Gürteltier
class Dillo {
   boolean isAlive;
   double weight;

   void runOver() {
     this.isAlive = false;
   } 
}
|#

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (if (dillo-alive? dillo)
        (make-dillo #f (dillo-weight dillo))
        (make-dillo #f (dillo-weight dillo)))
    #;(cond
      ((dillo-alive? dillo)
       (make-dillo #f (dillo-weight dillo)))
      (else ;(not (dillo-alive? dillo))
       (make-dillo #f (dillo-weight dillo))))))


#;(cond
    (<Bedingung1> <Antwort1>)
    (<Bedingung2> <Antwort2>)
    ...
    (<Bedingungn> <Antwortn>))

; =

#;(if <Bedingung1>
      <Antwort1>
      (if <Bedingung2>
          <Antwort2>
          ...
          (if <Bedingungn>
              <Antwortn>
              <Error>)))


#;(if <Bedingung>
      <Konsequente>
      <Alternative>)

; =
#;(cond
    (<Bedingung> <Konsequente>)
    (else <Alternative>))

#;(match <Expression>
    (<Pattern1> <Antwort1>)
    ...
    (<Patternn> <Antwortn>))


; Papagei hat folgende Eigenschaften:
; - Satz
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungs-Papagei, 1kg
(define parrot1 (make-parrot "Hello!" 1))
; ... andersrum, dick:
(define parrot2 (make-parrot "Goodbye!" 2))
  
; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))


(define run-over-parrot
  (lambda (parrot)
    (make-parrot ""
                 (parrot-weight parrot))))



; Ein Tier (auf dem texanischen Highway) ist eins der folgenden:
; - Gürteltier - ODER -
; - Papagei
; Fallunterscheidung
; hier: gemischte Daten, Fallunterscheidung aus jeweils zusammengesetzten Daten
(define animal
  (signature (mixed dillo parrot)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))

(define run-over-animal
  (lambda (animal)
    (match animal
      ((make-dillo alive? weight) (make-dillo #f weight))
       ;           ^^^^^^ ^^^^^^ frei wählbar
      ((make-parrot sentence weight) (make-parrot "" weight)))
        
    #;(cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; lexikalische Bindung
; innen -> außen: erstmal nach lambda, dann define, dann eingebaut


; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste aus erstem Element und Rest-Liste
;                                               ^^^^^ Selbstbezug
(define list-of-numbers
  (signature
   (mixed empty-list
          cons-list)))

; Die leere Liste besteht aus ... nix.
(define-record empty-list
  make-empty
  empty?)

(define empty (make-empty))

(define-record cons-list ; Signatur
  cons ; Konstruktor
  cons? ; Prädikat
  (first number)
  (rest list-of-numbers)) ; Selbstbezug

; (: cons (number list-of-numbers -> list-of-numbers))

; 1elementige Liste: 17
(define list1 (cons 17 empty))
; 2elementige Liste: 5 17
(define list2 (cons 5 (cons 17 empty)))
; 3elementige Liste 12 17 3:
(define list3 (cons 12 (cons 17 (cons 3 empty))))
; 4elementige Liste: 5 12 17 3:
(define list4 (cons 5 list3))

; Elemente einer Liste aufsummieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list3)
              32)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; "additive Einheit", "neutrales Element der Addition"
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Elemente einer Liste aufmultiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list3)
              612)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; "multiplikative Einheit", "neutrales Element der Multiplikation"
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; alle geraden Zahlen einer Liste extrahieren
(: extract-evens (list-of-numbers -> list-of-numbers))

(check-expect (extract-evens (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty)))))))
              (cons 2 (cons 4 (cons 6 empty))))


(define extract-evens
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (even? (first list))
           (cons (first list) (extract-evens (rest list)))
           (extract-evens (rest list)))))))

; alle positiven Zahlen einer Liste extrahieren
(: extract-positives (list-of-numbers -> list-of-numbers))

(check-expect (extract-positives (cons 1 (cons -1 (cons 4 (cons -12 (cons 0 empty))))))
              (cons 1 (cons 4 empty)))

(define extract-positives
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (positive? (first list))
           (cons (first list) (extract-positives (rest list)))
           (extract-positives (rest list)))))))

; alle Elemente, die ein Kriterium erfüllen, aus einer Liste extrahieren
(: extract ((number -> boolean) list-of-numbers -> list-of-numbers))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list) (extract p? (rest list)))
           (extract p? (rest list)))))))
