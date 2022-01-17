#lang racket

(provide parse-org-file org->sexp sexp->org) 

(require racket/base)
(require racket/file)
(require racket/string)

;; returns the depth of a header
(define (org-level org-line)
  (define (org-level-step l c)
    (if (empty? l)
        c
        (if [equal? (car l) #\*]
            (org-level-step (cdr l) (add1 c))
            c)))
  (org-level-step (string->list org-line) 0))



;; org-header predicate
(define (org-header? org-str)
  (> (org-level org-str) 0))

;; extract just the header, without the asterics
(define (org-extract-header org-str)
  (list->string (drop (string->list org-str) (org-level org-str))))

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

;; format applied after removing asterics, before string is put into final sexp
;; this function may be used in the future for e.g removal of leading whitespaces
(define (org-header-format-str-sexp s)
  (let ((l (string->list s)))
    (if (empty? l)
        s
        (if (equal? (car l) #\space)
            (org-header-format-str-sexp (list->string (cdr l)))
            (list->string l))
        )))

;; format applied after the string is extracted from sexp and before adding leading asterics
(define (org-header-format-sexp-str s)
  (string-append " " s))

;; takes a list of consequitive org headers as strings, returns sexp representation
(define (org->sexp org-list)
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
  (org->sexp-rec (filter org-header? org-list) 0))

;; takes sexp representation and turns it into org structure
(define (sexp->org org-sexp)
  (define (set-depth s d)
    (if [equal? d 0]
        s
        (set-depth [string-append "*" s] (sub1 d))))

  (define (org-sexp-loop sexp depth)
    (if (empty? sexp)
        sexp
        (if [string? sexp]
            (list [set-depth (org-header-format-sexp-str sexp) depth])
            [append (org-sexp-loop (car sexp) (add1 depth))
                    (org-sexp-loop (cdr sexp) depth)])
        ))
  
  (org-sexp-loop org-sexp -1))


