#lang racket

(require mock)
(require json)
(require "json.rkt"
         "session.rkt"
         "find.rkt"
         "cookies.rkt")

(provide with-session
         implicit-wait-time!
         open-url
         download-link)

(provide (contract-out
          [struct session ((id string?))]
          [struct element ((id string?))]
          [create-session (-> session?)]))

(define chromedriver "http://localhost:4444/wd/hub")

(define (open-url url)
  (let ([session-url (format "/session/~a/url" (session-id (current-session)))])
    (json-post session-url #:json (hash 'url url))))


(define/mock (implicit-wait-time! ms)
  #:mock json-post

  (json-post
   (format "/session/~a/timeouts/implicit_wait" (session-id (current-session)))
   #:json (hash 'ms ms)))

(define/mock (current-url)
  #:mock json-get #:as json-get-mock

  (define json (json-get (format "/session/~a/url" (session-id (current-session)))))
  (hash-ref json 'value))


(define/mock (download-link href)
  #:mock http-request
  #:mock get-session-cookies
  #:mock current-url

  (http-request href #:heads (list (cons "Cookie" (cookies->header (get-session-cookies)))
                                   (cons "Referer" (current-url)))))


(module+ test
  (require rackunit mock/rackunit net/cookies)
  (parameterize ([current-session (session "fake-id")])
    (test-case "Test current-url"
      (with-mocks current-url
        (define mock-return (const (hash 'value "https://localhost")))

        (with-mock-behavior ([json-get-mock mock-return])
          (check-equal? (current-url) "https://localhost"))))


    (test-case "Test implicit-wait-time!"
      (with-mocks implicit-wait-time!
        (with-mock-behavior ([json-post void/kw])
          (implicit-wait-time! 250)
          (check-mock-called-with? json-post (arguments "/session/fake-id/timeouts/implicit_wait" #:json (hash 'ms 250))))))

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
            (check-mock-called-with? http-request (arguments "href" #:heads '(("Cookie" . #"a=b") ("Referer" . "https://localhost")))))))
      )))
