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
     (form ((action ,(build-url code)))
           (table
            ,@(for/list ([p+id (in-list proposals+ids)])
                (match-define (list p id) p+id)
                `(tr (td ,p)
                     (td (input ((size "40")
                                 (type "text")
                                 (value ,(~a (hash-ref current id 0)))
                                 (onchange "this.form.submit()")
                                 (id ,id)
                                 (name ,id))))))

            (tr (td (b "Total")) (td ,@(if (= total-vote 10)
                                           (list)
                                           (list `((style "color:red"))))
                                     ,(~a total-vote)))))
     "The vote will count only if the sum is 10.")))

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

#|
     )))


      (form ((action ,(build-url #f #f #f)))
            (div
             "Instructor"
             (select ((name "instructor") (id "instructor") (onchange "this.form.submit()"))
                     (option ((value "--")
                              ,@(if instructor (list) (list `(selected "yes"))))
                             "--")
                     ,@(for/list ([name (in-list (all-instructions-that-teach-at-least-one-CS-class))])
                         `(option ((value ,(~a name))
                                   ,@(add-selected-if (equal? instructor name)))
                                  ,(~a name))))))

      (define (text-field description lab selector)
    `(div ,@(if (list? description) description (list description)) (br)
          (input ((size "100")
                  (type "text")
                  (value ,(or (selector class-status) ""))
                  (onchange "this.form.submit()")
                  (id ,lab) (name ,lab)))))

|#