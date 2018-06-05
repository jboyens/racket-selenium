#lang racket

(require mock)
(require "json.rkt")

(provide (struct-out session)
         (struct-out element)
         current-session
         create-session
         delete-session!
         delete-all-sessions!
         with-session)

(define *desiredCapabilities* #hasheq((desiredCapabilities . #hasheq((browserName . "chrome")))))

(struct session (id) #:transparent)
(struct element (id))

(define current-session (make-parameter null))

(define/mock (create-session)
  #:mock json-post #:as json-post-mock

  (let ([response (json-post "/session"
                             #:json *desiredCapabilities*)])
    (session (hash-ref response 'sessionId))))


(define/mock (delete-session! session)
  #:mock json-delete

  (void (json-delete (format "/session/~a" (session-id session)))))


(define/mock (delete-all-sessions!)
  #:mock json-get
  #:mock json-delete

  (let ([ids (map (λ (item) (hash-ref item 'id))
                  (hash-ref (json-get "/sessions") 'value))])
    (for-each (λ (id) (json-delete (format "/session/~a" id))) ids)))


(define (with-session body)
  (parameterize ([current-session (create-session)])
    (body)
    (delete-session! (current-session))))

(module+ test
  (require rackunit mock/rackunit)
  (parameterize ([current-session (session "fake-id")])

    (test-case "Test create-session"
      (with-mocks create-session
        (define mock-return (const (hash 'sessionId "random-session-id")))

        (with-mock-behavior ([json-post-mock mock-return])
          (check-equal? (session-id (create-session)) "random-session-id")
          (check-mock-called-with? json-post-mock (arguments "/session" #:json *desiredCapabilities*)))))

    (test-case "Test delete-session!"
      (with-mocks delete-session!
        (with-mock-behavior ([json-delete void])
          (delete-session! (session "session-id"))
          (check-mock-called-with? json-delete (arguments "/session/session-id")))))

    (test-case "Test delete-all-sessions!"
      (with-mocks delete-all-sessions!
        (with-mock-behavior ([json-get (const (hash 'value
                                                    (list (hash 'id "session-1")
                                                          (hash 'id "session-2")
                                                          (hash 'id "session-3"))))]
                             [json-delete void])
        (delete-all-sessions!)
        (check-mock-called-with? json-get (arguments "/sessions"))
          (check-mock-calls json-delete (list
                                         (arguments "/session/session-1")
                                         (arguments "/session/session-2")
                                         (arguments "/session/session-3"))))))

  ))
