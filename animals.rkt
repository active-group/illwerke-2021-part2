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
(check-expect (cute? "snake") #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
(define cute?
  (lambda (pet)
    ; pet ist eine Fallunterscheidung
    ; -> Verzweigung
    ; Zweig: (<Bedingung> <Antwort>)
    (cond
      ((string=? pet "cat") ...)
      ((string=? pet "dog") ...)
      ((string=? pet "snake") ...))))
