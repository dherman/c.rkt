#lang setup/infotab
(define name "c")
(define blurb
  (list "Tools for manipulating C, including header extraction for the FFI."))
(define scribblings '(("scribblings/c.scrbl" (multi-page))))
(define categories '(misc metaprogramming system devtools))
(define version "0.5")
(define primary-file "main.rkt")
(define release-notes
  (list '(p "Updated for Racket.")))
(define required-core-version "5.0")
(define repositories '("4.x"))
