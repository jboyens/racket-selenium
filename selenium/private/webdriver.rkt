#lang racket

(require mock
         "utils.rkt"
         "http.rkt"
         "struct.rkt")

(provide download-link
         get-session-cookies
         http-request
         implicit-wait-time!
         open-url)

;; (provide (contract-out
;;           [struct session ((id string?))]
;;           [struct element ((id string?))]
;;           [create-session (-> session?)]))

(define/mock (open-url url)
  #:mock format-url
  (let ([session-url (format-url "/session/{sessionid}/url")])
    (json-post session-url #:json (hash 'url url))))


(define/mock (implicit-wait-time! ms)
  #:mock json-post
  #:mock format-url

  (json-post (format-url "/session/{sessionid}/timeouts/implicit_wait") #:json (hash 'ms ms)))

(define/mock (current-url)
  #:mock json-get #:as json-get-mock
  #:mock format-url

  (hash-ref (json-get (format-url "/session/{sessionid}/url")) 'value))


(define/mock (download-link href)
  #:mock http-request
  #:mock get-session-cookies
  #:mock current-url

  (http-request href #:heads (list (cons "Cookie" (cookies->header (get-session-cookies)))
                                   (cons "Referer" (current-url)))))



(define current-session (make-parameter null))

(module+ test
  (require rackunit mock/rackunit net/cookies)

  (test-case "Test current-url"
    (with-mocks current-url
      (define mock-return (const (hash 'value "https://localhost")))

      (with-mock-behavior ([json-get-mock mock-return]
                           [format-url void])
        (check-equal? (current-url) "https://localhost"))))


    (test-case "Test implicit-wait-time!"
      (with-mocks implicit-wait-time!
        (with-mock-behavior ([json-post void/kw]
                             [format-url (const "http://localhost:4444/wd/hub/session/fake-id/timeouts/implicit_wait")])
          (implicit-wait-time! 250)
          (check-mock-called-with? json-post (arguments "http://localhost:4444/wd/hub/session/fake-id/timeouts/implicit_wait" #:json (hash 'ms 250))))))

    (test-case "Download-link"
      (test-case "- without cookies"
        (with-mocks download-link
          (with-mock-behavior ([http-request (const (string->bytes/utf-8 "contents"))]
                               [get-session-cookies (const empty)]
                               [current-url (const "https://localhost")])
            (download-link "href")
            (check-mock-called-with? http-request (arguments "href" #:heads '(("Cookie" . #f) ("Referer" . "https://localhost")))))))

      (test-case "- with cookies"
        (with-mocks download-link
          (with-mock-behavior ([http-request (const (string->bytes/utf-8 "contents"))]
                               [get-session-cookies (const (list (make-cookie "a" "b")))]
                               [current-url (const "https://localhost")])
            (download-link "href")
            (check-mock-called-with? http-request (arguments "href" #:heads '(("Cookie" . #"a=b") ("Referer" . "https://localhost"))))))))
    )
