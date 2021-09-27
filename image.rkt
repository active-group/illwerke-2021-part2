;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname image) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Language -> Add Teachpack -> image.rkt -> OK
; dann Run / Start oben rechts
(define x
  (+ 23
     (* 12
        14)))


(define square1 (square 100 "solid" "blue"))
(define circle1 (circle 50 "solid" "red"))
(define star1 (star 50 "solid" "green"))

(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion
; - kopieren
; - Variablen einführen
; - Variablen -> lambda
(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

(tile square1 circle1)

#|
class Image {

  static Image tile(Image image1, Image image2) {
    ...
  }

  static int m(int x) {
    ...
    x
    ...
    x = 23;
    x
    ...
  }

}

... Image.m(17) ...

hier / FP: Variable steht für einen Wert
C#: Variable steht für eine Speicherzelle, die für einen Wert steht


|#







