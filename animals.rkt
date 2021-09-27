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
