;; test.scm

{define (factorial n)
  {if (= n 0)
      1
      (* n (factorial (- n 1)))}}

;; Solve a quadratic equation, returning the two roots.
{define (solve-quadratic a b c)
  ; compute the discrimnant
  {let ([d (sqrt (- (* b b) (* 4.0 a c)))])
    ; assemble the two results
    (list (/ (+ (- b) d) (* 2.0 a))
          (/ (- (- b) d) (* 2.0 a)))}}

(list #f (lambda (x) x) '(foo bar baz) 'foo 'bar 'baz)

(+ 2.0 -3.5 1.2e10 1.0E-10 3.14E+101)

(string-append "foobar" "this
is
a
very
long
string
divided
into
multiple
lines
with
some
symbols like ~!@#$%^&*()_+`-={}|[]\:;'<>?,./
added for good measure")

