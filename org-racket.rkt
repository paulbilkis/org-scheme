#lang racket

(provide parse-org-file org->sexp sexp->org) 

(require racket/base)
(require racket/file)
(require racket/string)

;; returns the depth of a header
(define (org-level org-line)
  (define (org-level-step l c)
    (if [equal? (car l) #\*]
        (org-level-step (cdr l) (add1 c))
        c))
  (org-level-step (string->list org-line) 0))
(org-level "** ers")
(org-level "ers")

;; retrieves continious org headers from l, that are lower than n
(define (retrieve-lower-levels l n)
  (define (is-lower e)
    [> (org-level e) n])
  (define (retrieve-lower-levels-rec l r n)    
    (if [empty? l]
        r
        [if (is-lower [car l])
            (retrieve-lower-levels-rec [cdr l] [cons (car l) r] n)
            r])
    )
  [reverse (retrieve-lower-levels-rec l '() n)])
(retrieve-lower-levels  '("** srsr" "** ersr" "ers" "* ers") 1)

;; org-header predicate
(define (org-header? org-str)
  (> (org-level org-str) 0))
(org-header? "** res")
(org-header? "*res")
(org-header? "res")

;; extract just the header, without the asterics
(define (org-extract-header org-str)
  (list->string (drop (string->list org-str) (org-level org-str))))

(org-extract-header "** res")
(org-extract-header "**res")
(org-extract-header "res")

;; reads an org file and convert it into an sexp
;; basically reads an org file and calls org->sexp
; in: fn - string, containing filename of an org file
; out: sexp
(define (parse-org-file fn)
  (define (read-org org-file)
    (regexp-split "\n+" (file->string org-file)))
  (if [non-empty-string? fn]
      (filter non-empty-string? (read-org fn))
      '[]
      ))
(parse-org-file "test.org")

;; format applied after removing asterics, before string is put into final sexp
;; this function may be used in the future for e.g removal of leading whitespaces
(define (org-header-format-str-sexp s)
  s)

;; format applied after the string is extracted from sexp and before adding leading asterics
(define (org-header-format-sexp-str s)
  s)

;; takes a list of consequitive org headers as strings, returns sexp representation
(define (org->sexp org-list)
  ;; creates N nested lists with e: e 2 -> ((e))
  ;; this solved the problem, when header of level N is followed by header of level N+M, where M > 1
  ;; this is better than just empty string (e.g. "* header 1\n**\n*** header 3"), because it allows
  ;; to handle two different situations differently (actual empty header in the original and missed/skipped header in the original file)
  (define (create-nested-lists e n)
    (if (zero? n)
        e
        (create-nested-lists (cons e '()) (sub1 n))))
  (define (org->sexp-rec l n)
    (if (empty? l)
        l
        (let* ((h (org-level (car l)))
               (h-str (org-header-format-str-sexp (org-extract-header (car l))))
               (t (cdr l))
               (lower (retrieve-lower-levels t h))
               (reduced (drop t (length lower))))
          (create-nested-lists (cons (cons h-str (org->sexp-rec lower h))
                                     (org->sexp-rec reduced (sub1 h)))
                               (- h n 1))
          )))
  (org->sexp-rec org-list 0))

(org->sexp (filter org-header? (parse-org-file "test.org")))
(org->sexp (filter org-header? (parse-org-file "todo.org")))

(define (test-sub a)
  (displayln a)
  (displayln (org->sexp a))
  (displayln #\*))

(test-sub (list "* rei rise" "* test1 ste" "** tesr"))

;; takes sexp representation and turns it into org structure
(define (sexp->org org-sexp) '())

'((" test header level 1")
  (" another lvl 1" (" second level"
                     (" third")
                     (" third2")))
  (" stand alone lvl1" (
                        (" wow lvl 3")
                        (" ")
                        ))
  )
