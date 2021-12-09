#lang racket
(require "threaded-store.rkt"
         "ids.rkt"
         net/url
         web-server/http/request-structs)
(module+ test (require rackunit))

(provide
 (contract-out
  [votes-page
   (-> string? any/c any/c)]))

(define areas
  (append (sort '("Systems" "Theory" "AI" "Interfaces" "Teaching-track") string<?)
          (list "Prefer not to say")))

(define nothanks "nothanks")
(define proposals+ids
  (list (list "Bio-inspired Robotics - Brenna presenting" "bio")
        (list "Collaborative AI - Ken presenting" "ai")
        (list "Critical Networked Systems - Fabian presenting" "net")
        (list "Parallel Systems - Peter presenting" "parallel")
        (list "Quantum - Nikos presenting" "quantum")
        (list "Vision - Mike presenting" "vision")
        (list "Proposals scored below this do not have my support" nothanks)))

(define (valid-entry? n)
  (cond
    [(string? n)
     (string->exact-number n)]
    [else #f]))

(define (build-url code)
  (url->string
   (make-url "http" ;; scheme
             #f ;; user
             #f ;; host
             #f ;; port
             #t ;; absolute
             (list (path/param "vote" '())
                   (path/param code '()))
             '()
             #f)))

(define (string->exact-number ent)
  (parameterize ([read-decimal-as-inexact #f])
    (define n (string->number ent))
    (and n (exact? n) (real? n) (not (negative? n)) n)))
(module+ test
  (check-equal? (string->exact-number "1") 1)
  (check-equal? (string->exact-number "1/2") 1/2)
  (check-equal? (string->exact-number "0.5") 1/2)
  (check-equal? (string->exact-number "0.1") 1/10)
  (check-true (exact? (string->exact-number "0.1"))))

(define (votes-page code req)
  (define current (update-vote code req))
  (define normalized (make-hash))
  (define sum (for/fold ([sum 0])
                        ([p+id (in-list proposals+ids)])
                (cond
                  [sum
                   (define ent (hash-ref current (list-ref p+id 1) #f))
                   (define n (and (string? ent) (string->exact-number ent)))
                   (and n (+ sum n))]
                  [else #f])))
  (define dont-vote? (hash-ref current no-opinion #f))
  (define nothanks-vote (hash-ref current nothanks #f))
  `(html
    (head (title "Vote for " ,code))
    (body
     (h1 "Vote for " (tt ,code))

     (br)
     
     (p "Indicate your preferences by putting a number (fraction and decimal"
        " notation are both okay) next to each proposal;"
        " the larger the number the more you prefer it. The actual vote you"
        " cast will be normalized to sum to 1.")

     (p "Give something a lower score than \""
        ,(list-ref (last proposals+ids) 0)
        "\" to indicate you do not support it.")

     (p "If you see any red, then the vote isn't valid.")

     (br) (br)
     
     (form ((action ,(build-url code)))
           (table
            ,@(for/list ([p+id (in-list proposals+ids)])
                (match-define (list p id) p+id)
                (define this-proposal-score (hash-ref current id #f))
                (define this-proposal-numeric-score
                  (and (string? this-proposal-score)
                       (string->exact-number this-proposal-score)))

                (define invalid-reason
                  (cond
                    [(hash-ref current no-opinion #f) #f]
                    [(not (valid-entry? this-proposal-score)) '("This doesn't look like a nonnegative number")]
                    [(and (not (equal? id nothanks))
                          (valid-entry? nothanks-vote)
                          this-proposal-numeric-score
                          (= (string->exact-number nothanks-vote)
                             this-proposal-numeric-score))
                     '("This has the same score as the cutoff")]
                    [else #f]))
                `(tr (td ,@(if invalid-reason
                               (list `((style "color:red")))
                               (list))
                         ,p)
                     (td (input ((size "40")
                                 (type "text")
                                 (value ,(~a (or this-proposal-score "")))
                                 (onchange "this.form.submit()")
                                 (id ,id)
                                 (name ,id))))
                     (td ,@(if invalid-reason
                               (list `((style "color:red")))
                               (list))
                         ,@(or invalid-reason
                               (if (and sum this-proposal-numeric-score (not dont-vote?))
                                   (list "Normalized score: "
                                         (~r (/ this-proposal-numeric-score sum)))
                                   '())))))

            (tr (td ((colspan "2"))) (br))

            (tr (td ((colspan "2"))
                    (p "Your Area: " ,(~a (id->area code)))))

            (tr (td ((colspan "2")) (br)))
            (tr (td ((colspan "2")) (br)))

            (tr (td ((colspan "2"))
                    (p "If you do not have strong "
                       "opinions or did not pay close enough attention to the process and proposals, please do not feel like you need to vote. "
                       "Instead, check this checkbox:")
                    (input ((type "checkbox") (onchange ,(format "document.getElementById(\"~a\").value=~a; this.form.submit()"
                                                                 no-opinion
                                                                 (if dont-vote? "false" "true")))
                                              ,@(if dont-vote?
                                                    (list `(checked "on"))
                                                    (list)))
                           (b "I won't vote; I am happy with what others decide."))
                    (input ((type "hidden") (id ,no-opinion) (name ,no-opinion) (value ,(if dont-vote? "true" "false"))))))
           )))))

(define (update-vote code req)
  (define original (or (get code) (hash)))
  (define new-vote
    (for/fold ([table original])
              ([proposal+id (in-list proposals+ids)])
      (match-define (list proposal id) proposal+id)
      (define incoming (extract-binding req (string->bytes/utf-8 id)))
      (cond
        [(or (not incoming) (equal? incoming ""))
         table]
        [else
         (hash-set table id incoming)])))
  (define no-vote (extract-binding req (string->bytes/utf-8 no-opinion)
                                   #:not-there 'not-there))
  (define with-area-and-no-vote
    (if (equal? no-vote 'not-there)
        new-vote
        (hash-set new-vote no-opinion (equal? no-vote "true"))))
  (set code with-area-and-no-vote)
  with-area-and-no-vote)

(define no-opinion "noopinion")

(define (hyphens->false-otherwise-bytes->string/utf-8 bytes)
  (cond
    [(regexp-match #rx"^[-]+$" bytes) #f]
    [else (bytes->string/utf-8 bytes)]))

(define (extract-binding req what
                         [convert hyphens->false-otherwise-bytes->string/utf-8]
                         #:not-there [not-there #f])
  (define b (bindings-assq what req))
  (cond
    [(binding:form? b)
     (convert (binding:form-value b))]
    [else not-there]))
