#lang racket

(require "http.rkt"
         "utils.rkt"
         "struct.rkt")


(define (find-element selector #:by [by "css selector"])
  (define response (json-post (format-url "/session/{sessionid}/element")
                              #:json (hash 'using by 'value selector)))
  (element (hash-ref (hash-ref response 'value) 'ELEMENT)))


(define (find-element/by-id id)
  (find-element id #:by "id"))


(define (find-element/by-xpath xpath)
  (find-element xpath #:by "xpath"))


(define (find-element/by-class-name name)
  (find-element name #:by "class name"))


(define (find-element/by-name name)
  (find-element name #:by "name"))


(define (find-element/by-link-text text)
  (find-element text #:by "link text"))


(define (find-element/by-partial-link-text text)
  (find-element text #:by "partial link text"))


(define (find-element/by-tag-name name)
  (find-element name #:by "tag name"))


(define (get-element-attribute elem attrib)
  (define json
    (json-get
     (format-url "/session/{sessionid}/element/{elementid}/attribute/{attributeid}"
                 (hash "elementid" (element-id elem)
                       "attributeid" attrib))))
  (hash-ref json 'value))


(define (fill-in elem value)
  (define e (if (string? elem) (find-element elem) elem))
  (json-post
   (format-url "/session/{sessionid}/element/{elementid}/value" (hash "elementid" (element-id e)))
   #:json (hash 'value (map string (string->list value)))))


(define (click elem)
  (define e (if (string? elem) (find-element elem) elem))
  (json-post
   (format-url "/session/{sessionid}/element/{elementid}/click"
           (hash "elementid" (element-id e)))))


(module+ test)
