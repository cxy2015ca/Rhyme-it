;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname suggest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; OPTIONAL -- spellcheck.rkt provides the following function:
;;
;; (spellcheck? s) determines if s is spelled correctly
;;   (according to a medium-sized wordlist)
;; spellcheck: Str -> Bool
;;
;; You may use this function for your own experiments
;; (and to show off your program to your friends & family)

;; Do NOT leave this require in your file when you submit your code.
;; (require "spellcheck.rkt")
;; [this file will be available after the A07 deadline]
;; NOTE: You do not need to open spellcheck.rkt in DrRacket to use it
;;       (opening the file in DrRacket may slow down your computer).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cindy Chen
;; CS 135 Fall
;; Assignment 9
;; Q4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: you should complete the documentation & tests (design recipe)
;; for all functions (except remove-at and remove-letters)
;; But remember, because most of your functions will not have a cond
;; or much logic, exhaustive tests may not be required

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Word is a Str
;; requires: only lowercase letters appear in the word
;;           (no spaces, punctuation, etc.)

(define letters (string->list "abcdefghijklmnopqrstuvwxyz"))

;; 4a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-dups slst) consumes a list of words and produces the list with
;;   the duplicates removed
;; remove-dups: (listof Word) -> (listof Word)
;; requires: slst is sorted in non-decreasing order
;; example
(check-expect (remove-dups '("apples" "banana" "orange" "cherry" "cherry"))
              '("apples" "banana" "orange" "cherry"))

(define (remove-dups slst)
  (foldr (lambda (x y) (cond [(empty? y) (cons x y)]
                             [(string=? x (first y)) y]
                             [else (cons x y)])) empty slst))

;; tests
(check-expect (remove-dups '("apples" "banana" "orange"))
              '("apples" "banana" "orange"))
(check-expect (remove-dups empty) empty)

;; 4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ifoldr combine base lst) consumes a function and applies it to all elements
;;   in the list with a counter keeping track of how many times the function
;;   has been run
;; ifoldr: (Nat X Y -> Y) Y (listof X) -> Y
;; example
(check-expect (ifoldr (lambda (i x y) (cons (list i x) y)) empty '(a b c))
              '((0 a) (1 b) (2 c)))

(define (ifoldr combine base lst)
  (local
    [;; (icombine nat x y) consumes a nat and two elements(x, y) and
     ;;   applies the combine function from the main function to x, y
     ;;   with nat as an accumulator
     ;; icombine: Nat X Y -> (Nat X Y -> Y) 
     (define (icombine nat x y) 
       (combine (add1 nat) x y))]
    (cond [(empty? lst) base]
          [else (icombine -1 (first lst)
                          (ifoldr icombine base (rest lst)))])))

;; tests
(check-expect (ifoldr add1 empty empty) '())

;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-at i lst) removes element with index i from lst
;; remove-at: Nat (listof Any) -> (listof Any)
;; Examples:
;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
(check-expect (remove-at 0 '(a b c d)) '(b c d))
(check-expect (remove-at 3 '(a b c d)) '(a b c))
(check-expect (remove-at 0 '()) '())


(define (remove-at i lst)
  (ifoldr (lambda (k x y)
            (cond [(= i k) y]
                  [else (cons x y)]))
          empty lst))

;; single deletions
;; (remove-letters s) produces a list of Words,
;;    each with one letter removed from s
;; remove-letters: Word -> (listof Word)
;; Examples:
;;;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
(check-expect (remove-letters "abc") '("bc" "ac" "ab"))
(check-expect (remove-letters "") '())

(define (remove-letters s)
  (local [(define loc (string->list s))]
    (build-list (length loc) (lambda (i) (list->string (remove-at i loc))))))

;; 4c ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (nlst->lst nlst) consumes a nested-list of characters and produces
;;   a list of strings
;; nlst->lst: nested-list-of-Char -> (listof Str)
(define (nlst->lst nlst)
  (foldr (lambda (x y) (cond [(empty? y) (cons (list->string x) y)]
                             [else (cons (list->string x) y)]))
         empty nlst))

;;*******************************************************************

;; (insert-letters word) consume a word and produces a list of words
;;   with every possible letter inserted before each letter in the word
;; insert-letters: Word -> (listof Word)
;; example
(check-expect (insert-letters "hi")
              (list "ahi" "bhi" "chi" "dhi" "ehi" "fhi" "ghi" "hhi"
                    "ihi" "jhi" "khi" "lhi" "mhi" "nhi" "ohi" "phi"
                    "qhi" "rhi" "shi" "thi" "uhi" "vhi" "whi" "xhi"
                    "yhi" "zhi" "hai" "hbi" "hci" "hdi" "hei" "hfi"
                    "hgi" "hhi" "hii" "hji" "hki" "hli" "hmi" "hni"
                    "hoi" "hpi" "hqi" "hri" "hsi" "hti" "hui" "hvi"
                    "hwi" "hxi" "hyi" "hzi"))

(define (insert-letters s)
  (local
    [;; (flatten-nlst nlst) consumes a nested list of strings and
     ;;   produces a large list of all the strings
     ;; flatten-nlst: nested-list-of-Str -> (listof Str)
     (define (flatten-nlst nlst)
       (foldr (lambda (x y) (cond [(empty? y) (append x y)]
                                  [else (append x y)])) empty nlst))]
    (flatten-nlst (build-list (length (string->list s))
                              (lambda (ind)
                                (insert-alphabet ind (string->list s)))))))

;; (insert-char ind c loc) inserts a character(c) before an index(ind)
;;   in a list of characters(loc)
;; insert-char: Nat Char (listof Char) -> (listof Char)
;; example
(check-expect (insert-char 1 #\x (list #\a #\a #\a))
              (list #\a #\x #\a #\a))

(define (insert-char ind c loc)
  (ifoldr (lambda (n x y) (cond [(= ind n) (append (list c) (cons x y))]
                                [else (cons x y)])) empty loc))

;; (insert-alphabet ind loc) inserts all letters in the alphabet before
;;   the index(ind)
;; insert-alphabet: Nat (listof Char) -> (listof Str)
;; example:
(check-expect (insert-alphabet 1 '(#\k #\k))
              (list "kak" "kbk" "kck" "kdk" "kek" "kfk" "kgk" "khk" "kik"
                    "kjk" "kkk" "klk" "kmk" "knk" "kok" "kpk" "kqk" "krk"
                    "ksk" "ktk" "kuk" "kvk" "kwk" "kxk" "kyk" "kzk"))

(define (insert-alphabet ind loc)
  (nlst->lst (ifoldr (lambda (n x y) (cons (insert-char ind x loc) y))
                     empty letters)))

;;*******************************************************************

;; (trailing-letters s) consumes a word(s) and produces a list of string
;;   with every letter inserted at the end of the word
;; trailing-letters: Word -> (listof Word)
;; example 
(check-expect (trailing-letters "a")
              (list "aa" "ab" "ac" "ad" "ae" "af" "ag" "ah" "ai" "aj"
                    "ak" "al" "am" "an" "ao" "ap" "aq" "ar" "as" "at"
                    "au" "av" "aw" "ax" "ay" "az"))

(define (trailing-letters s)
  (nlst->lst
   (map (lambda (x) (append (string->list s) (cons x empty))) letters)))

;;*********************************************************************

;; (replace-letters s) consumes a string and replaces each letter in the
;;   string with every possible letter in the alphabet
;; replace-letters: Word -> (listof Word)
;; example
(check-expect (replace-letters "hi")
              (list "ai" "bi" "ci" "di" "ei" "fi" "gi" "hi" "ii"
                    "ji" "ki" "li" "mi" "ni" "oi" "pi" "qi" "ri"
                    "si" "ti" "ui" "vi" "wi" "xi" "yi" "zi" "ha"
                    "hb" "hc" "hd" "he" "hf" "hg" "hh" "hi" "hj"
                    "hk" "hl" "hm" "hn" "ho" "hp" "hq" "hr" "hs"
                    "ht" "hu" "hv" "hw" "hx" "hy" "hz"))

(define (replace-letters s)
  (local
    [;; (flatten-nlst nlst) consumes a nested list of strings and
     ;;   produces a large list of all the strings
     ;; flatten-nlst: nested-list-of-Str -> (listof Str)
     (define (flatten-nlst nlst)
       (foldr (lambda (x y) (cond [(empty? y) (append x y)]
                                  [else (append x y)])) empty nlst))]
    (flatten-nlst (build-list (length (string->list s))
                              (lambda (ind)
                                (sub-alphabet ind (string->list s)))))))

;; (sub-char ind c loc) replaces a character(c) at an index(ind)
;;   in a list of characters(loc)
;; sub-char: Nat Char (listof Char) -> (listof Char)
;; example
(check-expect (sub-char 1 #\x (list #\a #\b #\c))
              (list #\a #\x #\c))
(define (sub-char ind c loc)
  (ifoldr (lambda (n x y) (cond [(= ind n) (append (list c) y)]
                                [else (cons x y)])) empty loc))

;; (sub-alphabet ind loc) replaces all letters in the alphabet before
;;   the index(ind)
;; sub-alphabet: Nat (listof Char) -> (listof Str)
;; example
(check-expect (sub-alphabet 1 (list  #\a #\b #\c))
              (list "aac" "abc" "acc" "adc" "aec" "afc" "agc" "ahc"
                    "aic" "ajc" "akc" "alc" "amc" "anc" "aoc" "apc"
                    "aqc" "arc" "asc" "atc" "auc" "avc" "awc" "axc"
                    "ayc" "azc"))

(define (sub-alphabet ind loc)
  (nlst->lst (ifoldr (lambda (n x y) (cons (sub-char ind x loc) y))
                     empty letters)))

;;*********************************************************************

;; (swap-letters s) consumes a string and produces a list of strings
;;   with all adjacent letters in that string swapped
;; swap-letters: Word -> (listof Word)
;; examples
(check-expect (swap-letters "okay") (list "koay" "oaky" "okya" "okay"))

(define (swap-letters s)
  (nlst->lst (build-list (floor (length (string->list s)))
                         (lambda (ind) (swap-char ind (string->list s))))))

;; (swap-char ind loc) consumes an index and a list of char and swaps
;;   the letters on both sides of that index
;; swap-char: Nat (listof Char) -> (listof Char)
;; example
(check-expect (swap-char 1 (string->list "okay")) (list #\o #\a #\k #\y))

(define (swap-char ind loc)
  (ifoldr (lambda (n x y) (cond [(and (= ind n) (not (empty? y)))
                                 (cons (first y) (cons x (rest y)))]
                                [else (cons x y)])) empty loc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You are not required to modify the definition of suggest,
;; but you may if you wish

(define (valid? s)
  (member? s '("rights" "right" "fight" "aardvark" "fhqwhgads" "bright")))

;; (suggest s) consumes a word(s) and a predicate function for determining
;;    if a word is spelled correction and produces a list of correctly
;;    spelled words that are "close" to the consumed word
;; suggest: Word (Word -> Bool) -> (listof Word)
;; examples
(check-expect (suggest "right" valid?) '("bright" "fight" "rights"))
(check-expect (suggest " " valid?) empty)

(define (suggest s valid?)
  (local [(define words (append (remove-letters s)
                                (insert-letters s)
                                (trailing-letters s)
                                (replace-letters s)
                                (swap-letters s)))

          (define valid-words (filter valid? words))

          (define legal-words (filter (lambda (x) (and (not (string=? s x))
                                                       (not (string=? x ""))))
                                      valid-words))

          (define clean-words (remove-dups (sort legal-words string<=?)))]

    clean-words))

;; tests
(check-expect (suggest "ight" valid?) '("fight" "right"))
