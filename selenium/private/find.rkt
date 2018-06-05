#lang racket

(require "json.rkt" "session.rkt" json)
(provide fill-in
         click
         find-element
         find-element/by-link-text
         find-element/by-xpath
         find-element/by-class-name
         find-element/by-id
         find-element/by-name
         find-element/by-partial-link-text
         find-element/by-tag-name)

(define (find-element selector #:by [by "css selector"])
  (define response (json-post (format "/session/~a/element" (session-id (current-session)))
                                 #:json (hash 'using by 'value selector)))
  (element (hash-ref (hash-ref (string->jsexpr (bytes->string/utf-8 response)) 'value) 'ELEMENT)))


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
     (format "/session/~a/element/~a/attribute/~a"
             (session-id (current-session))
             (element-id elem)
             attrib)))
  (hash-ref json 'value))


(define (fill-in elem value)
  (define e (if (string? elem) (find-element elem) elem))
  (json-post
   (format "/session/~a/element/~a/value" (session-id (current-session)) (element-id e))
   #:json (hash 'value (map string (string->list value)))))


(define (click elem)
  (define e (if (string? elem) (find-element elem) elem))
  (json-post
   (format "/session/~a/element/~a/click"
           (session-id (current-session))
           (element-id e))))
