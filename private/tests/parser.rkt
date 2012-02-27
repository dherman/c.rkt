#lang racket/base

;; TESTS:
;;   - typedef enum { T = 128 } E; typedef int T[T]; T x;
;;   - enum { A = 0, B = A + 1 } E;
;;   - typedef int foo; struct s { int foo; };
;;   - typedef int foo; struct foo { int x; };
;;   - int (*(apfi[3]))(int x, int y);             // array of pointers to functions
;;   - int *f(int x, int y);                       // function that returns int pointer
;;   - int *(f)(int x, int y);                     // function that returns int pointer
;;   - int (*f)(int x, int y);                     // pointer to function
;;   - typedef int t; sizeof(int(int t, t x));     // parse error
;;   - typedef int t; sizeof(int(int t)); { t x; } // formals scope restricted to formals
;;   - typedef int T; ... { enum E { T = 128, U = T + 1 } }
;;   - typedef int t; ... { enum E { t = t }; }    // parse error
