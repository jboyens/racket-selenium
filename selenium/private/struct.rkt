#lang racket

(provide (struct-out session)
         (struct-out element))

(struct session (id) #:transparent)
(struct element (id))

(module+ test)
