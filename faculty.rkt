#lang racket
(provide all-faculty
         faculty-member?
         faculty->n)

(define ai
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

(define interfaces
  '("Jessica Hullman"
    "Haoqi Zhang"
    "Matt Kay"
    "Nell O'Rourke"
    "Mike Horn"
    "Uri Wilensky"
    "Marcelo Worsley"
    "Maia Jacobs"
    "Jack Tumblin"
    "Ollie Cossairt"
    "Brent Hecht"))

(define systems
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
 
(define theory
  '("Jason Hartline"
    "Kostya Makarychev"
    "Samir Khuller"
    "Aravindan Vijayaraghavan"
    "Xiao Wang"
    "Annie Liang"
    "Ben Golub"))

(define teaching
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

(define (faculty-member? n) (and (member n all-faculty) #t))