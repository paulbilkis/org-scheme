#lang racket

(require rackunit rackunit/text-ui "org-racket.rkt")
(require/expose "org-racket.rkt" (org-header? org-level org-extract-header))


(define org-sexp-tests
  (test-suite
   "org->sexp suite"
   (test-case "org->sexp"
     (check-equal? (org->sexp '("* header 1" "* header 1" "** header 2" "non-header"))
                   '(("header 1") ("header 1" ("header 2"))))
     (check-equal? (org->sexp '("* header 1" "*** header 3"))
                   '(("header 1" (("header 3")))))
     (check-equal? (org->sexp '())
                   '())
     (check-equal? (org->sexp (parse-org-file "test.org"))
                   '(("test header level 1")
                     ("another lvl 1" ("second level" ("third") ("third2")) ("second again") ("second again x2"))
                     ("stand alone lvl1" (("wow lvl 3") ("")))
                     (""))))

   (test-case "org-header"
     (check-true (org-header? "* a header") #t)
     (check-false (org-header? "not a header") #f)
     (check-true (org-header? "*header") #t)
     (check-true (org-header? "*") #t))

   (test-case "org-level"
     (check-equal? (org-level "") 0)
     (check-equal? (org-level "*") 1)
     (check-equal? (org-level "* ") 1)
     (check-equal? (org-level "* a header") 1)
     (check-equal? (org-level "* * a header") 1)
     (check-equal? (org-level "** a header") 2))
   ))

(define sexp-org-tests
  (test-suite
   "sexp->org suite"
   (test-case "sexp->org basic"
     (check-equal? (sexp->org '(("header 1") ("header 1.2" ("header 2"))))
                   '("* header 1" "* header 1.2" "** header 2")))
   (test-case "sexp-org back and forth"
     (let ((parsed-org (parse-org-file "test.org")))
       (check-equal? (sexp->org (org->sexp parsed-org))
                     parsed-org)))
   ))

(run-tests org-sexp-tests 'normal)
(run-tests sexp-org-tests 'normal)

(define org (parse-org-file "test.org"))
(define org-translated (org->sexp org))
org
org-translated
(sexp->org org-translated)

