#lang racket
(require "threaded-store.rkt" net/url
          web-server/http/request-structs)
(provide
 (contract-out
  [votes-page
   (-> string? any/c any/c)]))

(define proposals+ids
  (list (list "Bio-inspired Robotics - Brenna presenting" "bio")
        (list "Conversational AI - Ken presenting" "ai")
        (list "Critical Networked Systems - Fabian presenting" "net")
        (list "Parallel Systems - Peter presenting" "parallel")
        (list "Quantum - Nikos presenting" "quantum")
        (list "Vision - Mike presenting" "vision")))

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

(define (votes-page code req)
  (define current (update-vote code req))
  (define total-vote
    (for/sum ([p+id (in-list proposals+ids)])
      (match-define (list p id) p+id)
      (hash-ref current id 0)))
  `(html
    (head (title "Vote for " ,code))
    (body
     (h1 "Vote for " (tt ,code))

     (br)
     
     "You have 9 votes to cast; you must cast at least 6 of them. You may not cast more than 3 on any one proposal. "
     "If you see any red, then your vote is not following those rules and will not count."

     (br) (br) (br)
     
     (form ((action ,(build-url code)))
           (table
            ,@(for/list ([p+id (in-list proposals+ids)])
                (match-define (list p id) p+id)
                (define this-proposal-vote (hash-ref current id 0))
                `(tr (td ,@(if (<= this-proposal-vote 3)
                               (list)
                               (list `((style "color:red"))))
                         ,p)
                     (td (input ((size "40")
                                 (type "text")
                                 (value ,(~a this-proposal-vote))
                                 (onchange "this.form.submit()")
                                 (id ,id)
                                 (name ,id))))))

            (tr (td (b "Total")) (td ,@(if (<= 6 total-vote 9)
                                           (list)
                                           (list `((style "color:red"))))
                                     ,(~a total-vote))))))))

(define (update-vote code req)
  (define new-vote
    (for/fold ([table (or (get code) (hash))])
              ([proposal+id (in-list proposals+ids)])
      (match-define (list proposal id) proposal+id)
      (define incoming (extract-binding req (string->bytes/utf-8 id)))
      (define num (and incoming (string->number incoming)))
      (cond
        [(and (exact-integer? num) (<= 0 num 10))
         (hash-set table id num)]
        [else table])))
  (set code new-vote)
  new-vote)


(define (hyphens->false-otherwise-bytes->string/utf-8 bytes)
  (cond
    [(regexp-match #rx"^[-]+$" bytes) #f]
    [else (bytes->string/utf-8 bytes)]))

(define (extract-binding req what [convert hyphens->false-otherwise-bytes->string/utf-8])
  (define b (bindings-assq what req))
  (cond
    [(binding:form? b)
     (convert (binding:form-value b))]
    [else #f]))
