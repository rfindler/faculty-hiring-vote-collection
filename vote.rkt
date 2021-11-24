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
        (list "Collaborative AI - Ken presenting" "ai")
        (list "Critical Networked Systems - Fabian presenting" "net")
        (list "Parallel Systems - Peter presenting" "parallel")
        (list "Quantum - Nikos presenting" "quantum")
        (list "Vision - Mike presenting" "vision")
        (list "Proposals ranked below this do not have my support" "nothanks")))

(define (valid-rank? n)
  (and (natural? n) (<= 1 n (length proposals+ids))))

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
  (define rank->proposals (make-hash))
  (for ([p+id (in-list proposals+ids)])
    (match-define (list p id) p+id)
    (define rank (hash-ref current id #f))
    (when rank
      (hash-set! rank->proposals rank (cons id (hash-ref rank->proposals rank '())))))
  (define (multiple-at-this-rank? rank)
    ((length (hash-ref rank->proposals rank '()))
     . >= .
     2))
  (define constituency-choice (hash-ref current constituency))
  (define dont-vote? (hash-ref current no-opinion))
  `(html
    (head (title "Vote for " ,code))
    (body
     (h1 "Vote for " (tt ,code))

     (br)
     
     (p "Rank order the choices below, where your top choice gets the number 0,"
        " your next choice gets the number 1, etc.")

     (p "Place the special item \""
        ,(list-ref (last proposals+ids) 0)
        "\" in the rank order to indicate which proposals you do not support at all."
        " That is, if you support all proposals, put it last; if you support no proposals, put it first.")

     (p "If you see any red, then the vote isn't valid.")

     (br) (br)
     
     (form ((action ,(build-url code)))
           (table
            ,@(for/list ([p+id (in-list proposals+ids)])
                (match-define (list p id) p+id)
                (define this-proposal-rank (hash-ref current id #f))
                (define invalid-reason
                  (cond
                    [(hash-ref current no-opinion #f) #f]
                    [(not this-proposal-rank) "This proposal is unranked"]
                    [(not (valid-rank? this-proposal-rank))
                     (format "Proposal ranks must be betweeen 1 and ~a" (length proposals+ids))]
                    [(multiple-at-this-rank? this-proposal-rank)
                     (format "Multiple proposals have rank ~a" this-proposal-rank)]
                    [else #f]))
                `(tr (td ,@(if invalid-reason
                               (list `((style "color:red")))
                               (list))
                         ,p)
                     (td (input ((size "40")
                                 (type "text")
                                 (value ,(~a (or this-proposal-rank "")))
                                 (onchange "this.form.submit()")
                                 (id ,id)
                                 (name ,id))))
                     (td ,(or invalid-reason ""))))

            (tr (td ((colspan "2"))) (br))

            (tr (td ((colspan "2"))
                    (p "Your Area:"
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
      (cond
        [(or (not incoming) (equal? incoming ""))
         table]
        [else
         (hash-set table id (string->number incoming))])))
  (define constituency-choice (or (extract-binding req (string->bytes/utf-8 constituency))
                                  (hash-ref original constituency no-area-selected)))
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
