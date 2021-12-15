#lang racket
(provide all-faculty
         faculty-member?
         faculty->n
         faculty->area-tag
         area-tag->area)

(define faculty->area-tag-ht (make-hash))
(define tags '())
(define (tag tag faculty)
  (for ([p (in-list faculty)])
    (hash-set! faculty->area-tag-ht p (length tags)))
  (set! tags (append tags (list tag))))

(define-syntax-rule
  (define-faculty id more)
  (begin
    (define id more)
    (tag 'id id)))

(define-faculty ai
  '("Chris Riesbeck"
    "Kris Hammond"
    "Larry Birnbaum"
    "VS Subrahmanian"
    "Ken Forbus"
    "Bryan Pardo"
    "Han Liu"
    "Ian Horswill"
    "Brenna Argall"
    "Mike Rubenstein"))

(define-faculty interfaces
  '("Jessica Hullman"
    "Haoqi Zhang"
    "Matt Kay"
    "Nell O'Rourke"
    "Mike Horn"
    "Uri Wilensky"
    "Marcelo Worsley"
    "Maia Jacobs"
    "Jack Tumblin"
    "Ollie Cossairt"))

(define-faculty systems
  '("Yan Chen"
    "Jennie Rogers"
    "Nikos Hardavellas"
    "Fabian Bustamante"
    "Peter Dinda"
    "Seda Memik"
    "Gokhan Memik"
    "Russ Joseph"
    "Christos Dimoulas"
    "Robby Findler"
    "Aleksandar Kuzmanovic"
    "Simone Campanoni"
    "Josiah Hester"))
 
(define-faculty theory
  '("Jason Hartline"
    "Kostya Makarychev"
    "Samir Khuller"
    "Aravindan Vijayaraghavan"
    "Xiao Wang"
    "Annie Liang"
    "Ben Golub"))

(define-faculty teaching
  '("Huiling Hu"
    "Connor Bain"
    "Sara Sood"
    "Sarah Van Wart"
    "Mohammed Alam"
    "Kate Compton"
    "Branden Ghena"
    "Vincent St-Amour"))

(define all-faculty
  (append ai
          interfaces
          systems
          theory
          teaching))

(define faculty->n-hash (make-hash))
(for ([faculty (in-list all-faculty)]
      [n (in-naturals)])
  (hash-set! faculty->n-hash faculty n))

(define (faculty->n faculty)
  (hash-ref faculty->n-hash faculty))
(define (faculty->area-tag faculty)
  (hash-ref faculty->area-tag-ht faculty))
(define (faculty-member? n) (and (member n all-faculty) #t))
(define (area-tag->area area-tag)
  (and (< area-tag (length tags))
       (list-ref tags area-tag)))