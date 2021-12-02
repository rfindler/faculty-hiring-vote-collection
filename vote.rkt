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

(define (valid-entry? n) (and (string? n) (real? (string->number n))))

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
  (define normalized (make-hash))
  (define sum (for/fold ([sum 0])
                        ([p+id (in-list proposals+ids)])
                (cond
                  [sum
                   (define ent (hash-ref current (list-ref p+id 1) #f))
                   (define n (and (string? ent) (string->number ent)))
                   (and n (+ sum n))]
                  [else #f])))
  (define constituency-choice (hash-ref current constituency))
  (define dont-vote? (hash-ref current no-opinion))
  `(html
    (head (title "Vote for " ,code))
    (body
     (h1 "Vote for " (tt ,code))

     (br)
     
     (p "Indicate your preferences by putting a number (fraction and decimal notation are both okay) next to each proposal;"
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
                       (string->number this-proposal-score)))

                (define invalid-reason
                  (cond
                    [(hash-ref current no-opinion #f) #f]
                    [(not (valid-entry? this-proposal-score)) '("This doesn't look like a number")]
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
                     (td ,@(or invalid-reason
                               (if (and sum this-proposal-numeric-score)
                                   (list "Normalized score: "
                                         (~r (/ this-proposal-numeric-score sum)))
                                   '())))))

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
         (hash-set table id incoming)])))
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
