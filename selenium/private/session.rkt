#lang racket

(require mock
         "http.rkt"
         "utils.rkt"
         "struct.rkt")

(provide *desiredCapabilities*
         create-session
         delete-session!
         delete-all-sessions!
         update-configuration)

(define *desiredCapabilities* #hasheq((desiredCapabilities . #hasheq((browserName . "chrome")))))

(define (update-configuration #:browser [browser "chrome"])
  (set! *desiredCapabilities* (hash 'desiredCapabilities (hash 'browserName browser)))
  (unless (empty? (current-session))
    (delete-session! (current-session))))

(define/mock (create-session)
  #:mock json-post #:as json-post-mock

  (let ([response (json-post (string-append base-url "/session")
                             #:json *desiredCapabilities*)])
    (session (hash-ref response 'sessionId))))


(define/mock (delete-session! session)
  #:mock json-delete

  (void (json-delete (format-url "/session/{sessionid}" (hash "sessionid" (session-id session))))))


(define/mock (delete-all-sessions!)
  #:mock json-get
  #:mock json-delete

  (let ([ids (map (λ (item) (hash-ref item 'id))
                  (hash-ref (json-get (format "~a/sessions" base-url)) 'value))])
    (for-each (λ (id) (json-delete (format "~a/session/~a" base-url id))) ids)))


(define (with-session body)
  (parameterize ([current-session (create-session)])
    (body)
    (delete-session! (current-session))))


(module+ test
  (require rackunit mock/rackunit)

  (parameterize ([current-session (session "sessionid")])

    (test-case "Test create-session"
      (with-mocks create-session
        (define mock-return (const (hash 'sessionId "random-session-id")))

        (with-mock-behavior ([json-post-mock mock-return])
          (check-equal? (session-id (create-session)) "random-session-id")
          (check-mock-called-with? json-post-mock (arguments "http://localhost:4444/wd/hub/session" #:json *desiredCapabilities*)))))

    (test-case "Test delete-session!"
      (with-mocks delete-session!
        (with-mock-behavior ([json-delete void])
          (delete-session! (session "session-id"))
          (check-mock-called-with? json-delete (arguments "http://localhost:4444/wd/hub/session/session-id")))))

    (test-case "Test delete-all-sessions!"
      (with-mocks delete-all-sessions!
        (with-mock-behavior ([json-get (const (hash 'value
                                                    (list (hash 'id "session-1")
                                                          (hash 'id "session-2")
                                                          (hash 'id "session-3"))))]
                             [json-delete void])
          (delete-all-sessions!)
          (check-mock-called-with? json-get (arguments "http://localhost:4444/wd/hub/sessions"))
          (check-mock-calls json-delete (list
                                         (arguments "http://localhost:4444/wd/hub/session/session-1")
                                         (arguments "http://localhost:4444/wd/hub/session/session-2")
                                         (arguments "http://localhost:4444/wd/hub/session/session-3"))))))
  ))
