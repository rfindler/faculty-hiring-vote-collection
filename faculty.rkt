#lang racket
(provide teaching-track-faculty
         tenure-track-faculty
         all-faculty
         tenure-track?
         teaching-track?
         faculty->n)

(define (tenure-track? x)
  (and (member x tenure-track-faculty) #t))
(define (teaching-track? x)
  (and (member x teaching-track-faculty) #t))

(define tenure-track-faculty
  '("Brenna Argall"
    "Larry Birnbaum"
    "Fabian Bustamante"
    "Simone Campanoni"
    "Yan Chen"
    "Oliver Cossairt"
    "Christos Dimoulas"
    "Peter Dinda"
    "Robby Findler"
    "Ken Forbus"
    "Benjamin Golub"
    "Kristian Hammond"
    "Nikos Hardavellas"
    "Jason Hartline"
    "Brent Hecht"
    "Josiah Hester"
    "Michael Horn"
    "Ian Horswill"
    "Jessica Hullman"
    "Maia Jacobs"
    "Russ Joseph"
    "Matthew Kay"
    "Samir Khuller"
    "Aleksandar Kuzmanovic"
    "Annie Liang"
    "Han Liu"
    "Konstantin Makarychev"
    "Gokhan Memik"
    "Seda Memik"
    "Eleanor O'Rourke"
    "Bryan Pardo"
    "Chris Riesbeck"
    "Jennie Rogers"
    "Michael Rubenstein"
    "Jack Tumblin"
    "Aravindan Vijayaraghavan"
    "Xiao Wang"
    "Marcelo Worsley"
    "Haoqi Zhang"
    "Uri Wilensky"))

(define teaching-track-faculty
  '("Sarah Van Wart"
    "Sara Owsley Sood"
    "Vincent St-Amour"
    "Branden Ghena"
    "Kate Compton"
    "Mohammed Alam"
    "Huiling Hu"))

(define all-faculty
  (append tenure-track-faculty teaching-track-faculty))

(define faculty->n-hash (make-hash))
(for ([faculty (in-list all-faculty)]
      [n (in-naturals)])
  (hash-set! faculty->n-hash faculty n))

(define (faculty->n faculty)
  (hash-ref faculty->n-hash faculty))
