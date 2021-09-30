#lang racket
(require "threaded-store.rkt" net/url
          web-server/http/request-structs)
(provide
 (contract-out
  [votes-page
   (-> string? any/c any/c)]))

(define areas
  (append (sort '("Systems" "Theory" "AI" "Interfaces" "Teaching-track") string<?)
          (list "Prefer not to say")))

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
  (define constituency-choice (hash-ref current constituency))
  (define dont-vote? (hash-ref current no-opinion))
  `(html
    (head (title "Vote for " ,code))
    (body
     (h1 "Vote for " (tt ,code))

     (br)
     
     (p "You have 9 votes to cast; you must cast at least 6 of them. You may not cast more than 3 on any one proposal. "
        "If you see any red, then your vote is not following those rules and will not count.")

     (br) (br)
     
     (form ((action ,(build-url code)))
           (table
            ,@(for/list ([p+id (in-list proposals+ids)])
                (match-define (list p id) p+id)
                (define this-proposal-vote (hash-ref current id 0))
                `(tr (td ,@(if (or (<= this-proposal-vote 3) dont-vote?)
                               (list)
                               (list `((style "color:red"))))
                         ,p)
                     (td (input ((size "40")
                                 (type "text")
                                 (value ,(~a this-proposal-vote))
                                 (onchange "this.form.submit()")
                                 (id ,id)
                                 (name ,id))))))

            (tr (td (b "Total")) (td ,@(if (or (<= 6 total-vote 9) dont-vote?)
                                           (list)
                                           (list `((style "color:red"))))
                                     ,(~a total-vote)))

            (tr (td ((colspan "2"))) (br))

            (tr (td ((colspan "2"))
                    (p "Area"
                       (select ((id ,constituency) (name ,constituency) (onchange "this.form.submit()"))
                               (option ((value ,no-area-selected)) (p ((style "color:red")) (b ,no-area-selected)))
                               ,@(for/list ([area (in-list areas)])
                                   `(option ((value ,area)
                                             ,@(if (equal? area constituency-choice)
                                                   (list `(selected "yes"))
                                                   (list)))
                                            ,area)))
                       ,@(if (equal? constituency-choice no-area-selected)
                             (list `(span ((style "color:red")) "Please chose an area or explicitly choose not to"))
                             (list)))))

            (tr (td ((colspan "2")) (br)))
            (tr (td ((colspan "2")) (br)))

            (tr (td ((colspan "2"))
                    (p "If you do not have strong "
                       "opinions or did not pay close enough attention to the process and proposals, please do not feel like you need to vote. "
                       "Instead, check this checkbox:")
                    (input ((type "checkbox") (id ,no-opinion) (name ,no-opinion) (onchange "this.form.submit()")
                                              ,@(if dont-vote?
                                                    (list `(checked "on"))
                                                    (list)))
                           (b "I won't vote; I am happy with what others decide."))))
           )))))

(define (update-vote code req)
  (define original (or (get code) (hash)))
  (define new-vote
    (for/fold ([table original])
              ([proposal+id (in-list proposals+ids)])
      (match-define (list proposal id) proposal+id)
      (define incoming (extract-binding req (string->bytes/utf-8 id)))
      (define num (cond
                    [(not incoming) #f]
                    [(equal? incoming "") 0]
                    [else (string->number incoming)]))
      (cond
        [(and (exact-integer? num) (<= 0 num 10))
         (hash-set table id num)]
        [else table])))
  (define constituency-choice (or (extract-binding req (string->bytes/utf-8 constituency))
                                  (hash-ref original constituency (first areas))))
  (unless (or (equal? constituency-choice no-area-selected)
              (member constituency-choice areas))
    (printf "unknown constituency-choice: ~s\n" constituency-choice)
    (set! constituency-choice (first areas)))
  (define no-vote (extract-binding req (string->bytes/utf-8 no-opinion)))
  (define with-area-and-no-vote (hash-set (hash-set new-vote constituency constituency-choice)
                              no-opinion no-vote))
  (set code with-area-and-no-vote)
  with-area-and-no-vote)

(define constituency "constituency")
(define no-area-selected "No choice yet made")
(define no-opinion "noopinion")

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
