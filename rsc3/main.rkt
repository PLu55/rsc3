#lang racket

;;; History
;;;    2015.12.21 PLu
;;;       Conserving the position of the parameters to synthdef
;;;
;;;    2015.12.26 PLu
;;;       Synthdef version 2 is implemented.

(require
  sosc/bytevector
  sosc/transport
  sosc/sosc
  (prefix-in srfi: srfi/27)  ;; Random bits
  (prefix-in srfi: srfi/19)) ;; Time Data Types, now defined in racket/base 

;; TODO - export only useful funcs
(provide (all-defined-out)
         send)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Replacing rhs

;;; Replaced
;(define nil '())
;(define head car)
;(define tail cdr)
;(define tuple2 cons)
;(define fst car)
;(define snd cdr)
;(define replicate make-list)
;(define mlength length)
;(define (enum-from-to f t) (range f (add1 t)))

(define zip-with
  (lambda (f a b)
    (cond ((null? a) '())
          ((null? b) '())
          (else (cons (f (car a) (car b))
                      (zip-with f (cdr a) (cdr b)))))))

;; zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
(define zip-with3
  (lambda (f a b c)
    (cond ((null? a) '())
          ((null? b) '())
          ((null? c) '())
          (else (cons (f (car a) (car b) (car c))
                      (zip-with3 f (cdr a) (cdr b) (cdr c)))))))
(define map-accum-l
  (lambda (f s l)
    (if (null? l)
        (cons s '())
        (let* ((x (car l))
               (xs (cdr l))
               (s_y (f s x))
               (s_ (car s_y))
               (y (cdr s_y))
               (s__ys (map-accum-l f s_ xs))
               (s__ (car s__ys))
               (ys (cdr s__ys)))
          (cons s__ (cons y ys))))))

(define foldl1
  (lambda (f l)
    (foldl f (car l) (cdr l))))

(define-syntax replicate-m
  (syntax-rules ()
    ((_ i x)
     (replicate-m* i (lambda () x)))))

;; int -> (() -> a) -> [a]
(define replicate-m*
  (lambda (i x)
    (if (<= i 0)
        '()
        (cons (x) (replicate-m* (- i 1) x)))))

;; concat :: [[a]] -> [a]
(define concat
  (lambda (l)
    (foldr append '() l)))

;; concatMap :: (a -> [b]) -> [a] -> [b]
(define concat-map
  (lambda (f l)
    (concat (map f l))))

;; nub :: (Eq a) => [a] -> [a]
(define nub
  (lambda (l)
    (nub-by equal? l)))

;; nubBy :: (a -> a -> Bool) -> [a] -> [a]
(define nub-by
  (lambda (f l)
    (if (null? l)
        '()
        (let ((x (car l))
              (xs (cdr l)))
          (cons x (nub-by f (filter (lambda (y) (not (f x y))) xs)))))))

;; findIndex :: (a -> Bool) -> [a] -> Maybe Int
(define find-index
  (letrec ((g (lambda (f l n)
                (if (null? l)
                    #f
                    (if (f (car l))
                        n
                        (g f (cdr l) (+ n 1)))))))
    (lambda (f l)
      (g f l 0))))

;; transpose :: [[a]] -> [[a]]
(define transpose
  (lambda (l)
    (let ((protect
           (lambda (f)
             (lambda (x)
               (if (null? x)
                   '()
                   (f x))))))
      (cond ((null? l) '())
            ((null? (car l)) (transpose (cdr l)))
            (else (let* ((e (car l))
                         (x (car e))
                         (xs (cdr e))
                         (xss (cdr l)))
                    (cons (cons x
                                (filter (compose not null?)
                                        (map (protect car) xss)))
                          (transpose (cons xs
                                           (map (protect cdr) xss))))))))))

;; maximum :: (Ord a) => [a] -> a
(define maximum
  (lambda (l)
    (foldl1 max l)))

;; intersperse :: a -> [a] -> [a]
(define intersperse
  (lambda (x l)
    (cond ((null? l) '())
          ((null? (cdr l)) l)
          (else (cons (car l) (cons x (intersperse x (cdr l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [a] -> int -> [a]
(define extend
  (lambda (l n)
    (let ((z (length l)))
      (cond ((= z n) l)
            ((> z n) (take n l))
            (else (extend (append l l) n))))))

;; [a] -> int -> [a]
(define take-cycle
  (lambda (l n)
    (if (null? l)
        '()
        (cons (car l)
              (take-cycle (drop n l) n)))))

;; (a -> a -> a) -> ([a] -> [a])
(define differentiate-with
  (lambda (f)
    (lambda (l)
      (zip-with f l (cons 0 l)))))

;; num a => [a] -> [a]
;;
;; (equal? (differentiate '(1 2 4 7 11)) '(1 1 2 3 4))
(define differentiate
  (differentiate-with -))

;; (a -> a -> a) -> ([a] -> [a])
(define integrate-with
  (lambda (f)
    (lambda (l)
      (let ((x (car l))
            (xs (cdr l))
            (g (lambda (a x) (let ((y (f a x))) (cons y y)))))
        (cons x (cdr (map-accum-l g x xs)))))))

;; num a => [a] -> [a]
;;
;; (equal? (integrate (list 3 4 1 1)) (list 3 7 8 9))
;; (equal? (integrate '(1 1 2 3 4)) '(1 2 4 7 11))
(define integrate
  (integrate-with +))

;; ord a => a -> a -> a
(define s:lt
  (lambda (p q)
    (if (< p q) 1 0)))

;; ord a => a -> a -> a
(define s:le
  (lambda (p q)
    (if (<= p q) 1 0)))

;; ord a => a -> a -> a
(define s:ge
  (lambda (p q)
    (if (>= p q) 1 0)))

;; ord a => a -> a -> a
(define s:gt
  (lambda (p q)
    (if (> p q) 1 0)))

;; real -> real -> real
(define s:round
  (lambda (p q)
    (* (round (/ p q)) q)))

;; ord a => a -> a -> a -> a
(define s:clip
  (lambda (a b n)
    (cond ((< n a) a)
	  ((> n b) b)
	  (else n))))

;; number a => a -> a
(define s:squared
  (lambda (n)
    (* n n)))

;; number a => a -> a
(define s:cubed
  (lambda (n)
    (* n n n)))

;; number a => a -> a
(define s:recip
  (lambda (n)
    (/ 1 n)))

;; float
(define e
  (exp 1.0))

;; float
(define pi
  (* 4 (atan 1)))

;; float -> float
(define s:log2
  (lambda (x)
    (/ (log (abs x)) (log 2))))

;; float -> float
(define s:log10
  (lambda (x)
    (/ (log x) (log 10))))

;; float -> float
(define s:amp-db
  (lambda (x)
    (* (s:log10 x) 20)))

;; float -> float
(define s:db-amp
  (lambda (x)
    (expt 10 (* x 0.05))))

;; float -> float
(define pow-db
  (lambda (x)
    (* (s:log10 x) 10)))

;; float -> float
(define db-pow
  (lambda (x)
    (expt 10 (* x 0.1))))

;; float -> float
(define s:midi-cps
  (lambda (note)
    (* 440.0 (expt 2.0 (* (- note 69.0) 0.083333333333)))))

;; float -> float
(define s:cps-midi
  (lambda (freq)
    (+ (* (s:log2 (* freq 0.0022727272727)) 12.0) 69.0)))

;; float -> float
(define s:midi-ratio
  (lambda (midi)
    (expt 2.0 (* midi 0.083333333333))))

;; float -> float
(define s:ratio-midi
  (lambda (ratio)
    (* 12.0 (s:log2 ratio))))

;; float -> float
(define s:oct-cps
  (lambda (note)
    (* 440.0 (expt 2.0 (- note 4.75)))))

;; float -> float
(define s:cps-oct
  (lambda (freq)
    (+ (s:log2 (* freq 0.0022727272727)) 4.75)))

;; float -> [float] -> int -> float
(define s:degree-to-key
  (lambda (degree scale steps)
    (let ((scale-n (length scale)))
      (+ (* steps (quotient degree scale-n))
	 (list-ref scale (inexact->exact (remainder degree scale-n)))))))

;; int -> [any] -> [any]
(define without
  (lambda (n l)
    (append (take n l) (drop (+ n 1) l))))

;; [int] -> bool
(define consecutive?
  (lambda (l)
    (let ((x (car l))
	  (xs (cdr l)))
      (or (null? xs)
	  (and (= (+ x 1) (car xs))
	       (consecutive? xs))))))

;; int -> uid
(struct uid (n) #:transparent)

;; () -> uid
(define unique-uid
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (uid n))))

;; string -> int -> control
(struct control (name index) #:transparent)

;; string -> float -> rate -> float -> control*
(struct control* (name default rate index) #:transparent)

;; string -> [float] -> [float] -> [controls] -> [ugens] -> graphdef
(struct graphdef (name constants defaults controls ugens)
  #:transparent)

;; graphdef -> int -> ugen
(define graphdef-ugen
  (lambda (g n)
    (list-ref (graphdef-ugens g) n)))

;; graphdef -> int -> control
(define graphdef-control
  (lambda (g n)
    (list-ref (graphdef-controls g) n)))

;; graphdef -> int -> float
(define graphdef-constant
  (lambda (g n)
    (list-ref (graphdef-constants g) n)))

;; int -> int -> input
(struct input (ugen port) #:transparent)

;; [ugen] -> mce
(struct mce (proxies) #:transparent)

;; ugen -> ugen -> mce
(define mce2
  (lambda (a b)
    (mce (list a b))))

;; ugen -> ugen -> ugen -> mce
(define mce3
  (lambda (a b c)
    (mce (list a b c))))

;; ugen -> ugen -> ugen -> ugen -> mce
(define mce4
  (lambda (a b c d)
    (mce (list a b c d))))

;; ugen -> ugen -> ugen -> ugen -> ugen -> mce
(define mce5
  (lambda (a b c d e)
    (mce (list a b c d e))))

;; node -> [ugen]
(define mce-channels
  (lambda (u)
    (cond
     ((mce? u) (mce-proxies u))
     ((mrg? u) (let ((rs (mce-channels (mrg-left u))))
                 (cons (mrg (car rs) (mrg-right u)) rs)))
     (else (list u)))))

;; mce -> int -> ugen
(define mce-channel
  (lambda (u n)
    (list-ref (mce-proxies u) n)))

;; ugen -> ugen -> mrg
(struct mrg (left right) #:transparent)

;; [ugen] -> mrg
(define mrg-n
  (lambda (xs)
    (if (null? xs)
	(error "mrg-n" "nil input list" xs)
	(if (null? (cdr xs))
	    (car xs)
	    (mrg2 (car xs) (mrg-n (cdr xs)))))))

;; ugen -> ugen -> mrg
(define mrg2
  mrg)

;; ugen -> ugen -> ugen -> mrg
(define mrg3
  (lambda (a b c)
    (mrg a (mrg b c))))

;; ugen -> ugen -> ugen -> ugen -> mrg
(define mrg4
  (lambda (a b c d)
    (mrg a (mrg b (mrg c d)))))

;; rate -> output
(struct output (rate) #:transparent)

;; ugen -> int -> proxy
(struct proxy (ugen port) #:transparent)

;; int -> rate
(struct rate (value) #:transparent)

;; rate
(define ir
  (rate 0))

;; rate
(define kr
  (rate 1))

;; rate
(define ar
  (rate 2))

;; rate
(define dr
  (rate 3))

;; any -> rate
(define rate-of
  (lambda (o)
    (cond ((number? o) ir)
	  ((control*? o) (control*-rate o))
	  ((ugen? o) (ugen-rate o))
	  ((proxy? o) (rate-of (proxy-ugen o)))
	  ((mce? o) (rate-select (map rate-of (mce-proxies o))))
	  ((mrg? o) (rate-of (mrg-left o)))
	  (else (error "rate-of" "illegal value" o)))))

;; rate -> int
(define rate-to-ordinal
  (lambda (r)
    (cond ((eq? r ir) 0)
	  ((eq? r kr) 1)
	  ((eq? r ar) 2)
	  ((eq? r dr) 3)
	  (else (error "rate-to-ordinal" "illegal rate")))))

;; rate -> rate -> rate
(define rate-select*
  (lambda (a b)
    (let ((a* (rate-to-ordinal a))
	  (b* (rate-to-ordinal b)))
      (if (> a* b*) a b))))

;; [rate] -> rate
(define rate-select
  (lambda (l)
    (foldl1 rate-select* l)))

;; string -> rate -> [ugen] -> [output] -> int -> uid -> ugen
(struct ugen (name rate inputs outputs special id)
  #:transparent)

;; ugen -> int -> output
(define ugen-output
  (lambda (u n)
    (list-ref (ugen-outputs u) n)))

;; ugen -> (ugen -> any) -> any
(define ugen-transform
  (lambda (u f)
    (let ((n (ugen-name u))
	  (r (ugen-rate u))
	  (i (ugen-inputs u))
	  (o (ugen-outputs u))
	  (s (ugen-special u))
	  (d (ugen-id u)))
      (f n r i o s d))))

;; any -> bool
(define input*?
  (lambda (i)
    (or (number? i)
	(control*? i)
	(ugen? i)
	(proxy? i)
	(mce? i)
        (mrg? i))))

;; ugen -> bool
(define ugen-valid?
  (lambda (u)
    (ugen-transform
     u
     (lambda (n r i o s d)
       (and (string? n)
	    (rate? r)
	    (and (list? i) (and input*? i))
	    (and (list? o) (and output? o))
	    (integer? s)
	    (uid? d))))))

;; int -> (() -> ugen) -> mce
(define clone*
  (lambda (n f)
    (mce (replicate-m* n f))))

(define-syntax clone
  (syntax-rules ()
    ((_ n u) (mce (replicate-m n u)))))

;; control -> [bytevector]
(define encode-control-v1
  (lambda (c)
    (list (encode-pstr (control-name c))
           (encode-i16 (control-index c)))))

(define encode-control-v2
  (lambda (c)
    (list (encode-pstr (control-name c))
           (encode-i32 (control-index c)))))

;; input -> [bytevector]
(define encode-input-v1
  (lambda (i)
    (list (encode-i16 (input-ugen i))
          (encode-i16 (input-port i)))))
      
(define encode-input-v2
  (lambda (i)
    (list (encode-i32 (input-ugen i))
          (encode-i32 (input-port i)))))

;; output -> [bytevector]
(define encode-output
  (lambda (o)
    (encode-u8 (rate-value (output-rate o)))))

;; [bytevector]
(define scgf
  (map encode-u8 (map char->integer (string->list "SCgf"))))

;; ugen -> [bytevector]
(define encode-ugen-v1
  (lambda (u)
    (ugen-transform
     u
     (lambda (n r i o s d)
       (list
        (encode-pstr n)
        (encode-u8 (rate-value r))
        (encode-i16 (length i))
        (encode-i16 (length o))
        (encode-i16 s)
        (map encode-input-v1 i)
        (map encode-output o))))))

(define encode-ugen-v2
  (lambda (u)
    (ugen-transform
     u
     (lambda (n r i o s d)
       (list
        (encode-pstr n)
        (encode-u8 (rate-value r))
        (encode-i32 (length i))
        (encode-i32 (length o))
        (encode-i16 s)
        (map encode-input-v2 i)
        (map encode-output o))))))

;; graphdef -> bytevector
(define encode-graphdef
  (lambda (g (ver 1))
    (flatten-bytevectors
     (let ((n (graphdef-name g))
           (c (graphdef-constants g))
           (d (graphdef-defaults g))
           (k (graphdef-controls g))
           (u (graphdef-ugens g)))
       (case ver
         ((0 1)
          (list
           scgf                         ; magic number
           (encode-i32 0)               ; version
           (encode-i16 1)               ; number of synth defs
           (encode-pstr n)              ; name
           (encode-i16 (length c))      ; number of constants
           (map encode-f32 c)           ; constants
           (encode-i16 (length d))      ; number of initial parameter values 
           (map encode-f32 d)           ; initial parameter values
           (encode-i16 (length k))      ; number of parameter names
           (map encode-control-v1 k)    ; parameter names (pstring <name> int16 index)
           (encode-i16 (length u))      ; number of ugens
           (map encode-ugen-v1 u)))     ; ugens
                                        ; variants are not implemented
         ((2)
          (list
           scgf                         ; magic number
           (encode-i32 2)               ; version
           (encode-i16 1)               ; number of synth defs
           (encode-pstr n)              ; name
           (encode-i32 (length c))      ; number of constants
           (map encode-f32 c)           ; constants
           (encode-i32 (length d))      ; number of initial parameter values 
           (map encode-f32 d)           ; initial parameter values
           (encode-i32 (length k))      ; number of parameter names
           (map encode-control-v2 k)    ; parameter names (pstring <name> int16 index)
           (encode-i32 (length u))      ; number of ugens
           (map encode-ugen-v2 u)))     ; ugens
         (else
          (error "encode-graphdef" "unknown version" ver)))))))

;; PLu, this causes the loss of the order of the arguments of the synthdef
;;
;; syntax for binding control values
(define-syntax letc
  (lambda (stx)
    (syntax-case stx ()
      ((_ () expr)
       #'expr)
      ((_ ((name default) ...) expr)
       (with-syntax (((idx ...) #`(#,@(build-list (length (syntax->datum #'(name ...))) abs))))
         #'(let ((name (control* (symbol->string (quote name)) default kr idx))
                 ...)
             expr))))))

;; node = ugen | proxy | control* | float

;; string -> maybe rate -> [node] -> maybe node -> int -> int -> uid -> ugen
(define construct-ugen
  (lambda (name rate? inputs mce? outputs special id)
    (let* ((inputs* (if mce?
			(append inputs (mce-channels mce?))
			inputs))
	   (rate (if rate?
		     rate?
		     (rate-select (map rate-of inputs*))))
	   (u (ugen
	       name
	       rate
	       inputs*
	       (make-list outputs (output rate))
	       special
	       id)))
      (proxify (mce-expand u)))))

;; ugen -> [node]
(define graph-nodes
  (lambda (u)
    (cond
     ((ugen? u) (cons u (concat-map graph-nodes (ugen-inputs u))))
     ((proxy? u) (cons u (graph-nodes (proxy-ugen u))))
     ((control*? u) (list u))
     ((number? u) (list u))
     ((mce? u) (concat (map graph-nodes (mce-proxies u))))
     ((mrg? u) (append (graph-nodes (mrg-left u)) (graph-nodes (mrg-right u))))
     (else (error "graph-nodes" "illegal value" u)))))

;; ugen -> [float]
(define graph-constants
  (lambda (u)
    (nub (filter number? (graph-nodes u)))))

;; ugen -> [control*]
(define graph-controls*
  (lambda (u)
    (nub (filter control*? (graph-nodes u)))))

;; ugen -> [ugen]
(define graph-ugens
  (lambda (u)
    (nub (reverse (filter ugen? (graph-nodes u))))))

;; ugen -> [node] -> [control] -> [ugen] -> ugen
(define ugen-close
  (lambda (u nn cc uu)
    (if (not (ugen-valid? u))
	(error "ugen-close" "invalid ugen" u)
	(ugen (ugen-name u)
		   (ugen-rate u)
		   (map (lambda (i)
			   (input*-to-input i nn cc uu))
			 (ugen-inputs u))
		   (ugen-outputs u)
		   (ugen-special u)
		   (ugen-id u)))))

;; ugen -> ugen
(define prepare-root
  (lambda (u)
    (cond
     ((mce? u) (mrg-n (mce-proxies u)))
     ((mrg? u) (mrg (prepare-root (mrg-left u))
                         (prepare-root (mrg-right u))))
     (else u))))

;; string -> ugen -> graphdef
(define synthdef
  (lambda (name pre-u)
    (let* ((u (prepare-root pre-u))
           (nn (graph-constants u))
	   (cc (graph-controls* u))
	   (uu (graph-ugens u))
	   (uu* (if (null? cc) uu (cons (implicit-ugen cc) uu))))
      (graphdef
       name
       nn
       (map control*-default cc)
       (map (lambda (c) (control*-to-control c cc)) cc)
       (map (lambda (u) (ugen-close u nn cc uu*)) uu*)))))

;; [control] -> ugen
(define implicit-ugen
  (lambda (cc)
    (ugen "Control"
	       kr
	       '()
	       (map output (make-list (length cc) kr))
	       0
	       (uid 0))))

;; node -> [node] -> int
(define calculate-index
  (lambda (n nn)
    (let ((i (find-index (lambda (e) (equal? e n)) nn)))
      (if (not i)
	  (error "calculate-index" "not located" n nn)
	  i))))

;; float -> [node] -> input
(define number-to-input
  (lambda (n nn)
    (input -1 (calculate-index n nn))))

;; control* -> [control*] -> control
(define control*-to-control
  (lambda (c cc)
    (control (control*-name c) (control*-index c))))

;; control* -> [control*] -> input
(define control*-to-input
  (lambda (c cc)
    (input 0 (control*-index c))))

;; ugen -> [ugen] -> input
(define ugen-to-input
  (lambda (u uu)
    (input (calculate-index u uu) 0)))

;; proxy -> [ugen] -> input
(define proxy-to-input
  (lambda (p uu)
    (input (calculate-index (proxy-ugen p) uu)
		(proxy-port p))))

;; node -> [node] -> [control] -> [ugen] -> input
(define input*-to-input
  (lambda (i nn cc uu)
    (cond
     ((number? i) (number-to-input i nn))
     ((control*? i) (control*-to-input i cc))
     ((ugen? i) (ugen-to-input i uu))
     ((proxy? i) (proxy-to-input i uu))
     ((mrg? i) (input*-to-input (mrg-left i) nn cc uu))
     ((mce? i) (error "input*-to-input" "mce?" i))
     (else (error "input*-to-input" "illegal input" i)))))

;; mce|mrg -> int
(define mce-degree
  (lambda (m)
    (cond
     ((mce? m) (length (mce-proxies m)))
     ((mrg? m) (mce-degree (mrg-left m)))
     (else (error "mce-degree" "illegal input" m)))))

;; ([ugen] -> [ugen]) -> (mce -> mce)
(define mce-edit
  (lambda (f)
    (lambda (u)
      (mce (f (mce-proxies u))))))

;; mce -> mce
(define mce-reverse
  (mce-edit reverse))

;; mce -> mce
(define mce-transpose
  (lambda (u)
    (mce
     (map mce (transpose (map mce-channels (mce-channels u)))))))

;; ugen -> bool
(define mce-required?
  (lambda (u)
    (not (null? (filter mce? (ugen-inputs u))))))

;; int -> node -> [node]
(define mce-extend
  (lambda (n i)
    (cond ((mce? i) (extend (mce-proxies i) n))
          ((mrg? i) (let ((rs (mce-extend n (mrg-left i))))
                      (cons (mrg (car rs) (mrg-right i)) (cdr rs))))
          (else (make-list n i)))))

;; ugen -> mce
(define mce-transform
  (lambda (u)
    (ugen-transform
     u
     (lambda (n r i o s d)
       (let* ((f (lambda (i*) (ugen n r i* o s d)))
	      (m (maximum (map mce-degree (filter mce? i))))
	      (e (lambda (i) (mce-extend m i)))
	      (i* (transpose (map e i))))
	 (mce (map f i*)))))))

;; node -> node|mce
(define mce-expand
  (lambda (u)
    (cond ((mce? u) (mce (map mce-expand (mce-proxies u))))
          ((mrg? u) (mrg (mce-expand (mrg-left u)) (mrg-right u)))
          (else (if (mce-required? u)
                    (mce-transform u)
                    u)))))

;; node -> mce
(define proxify
  (lambda (u)
    (cond
     ((mce? u) (mce (map proxify (mce-proxies u))))
     ((mrg? u) (mrg (proxify (mrg-left u)) (mrg-right u)))
     ((ugen? u) (let* ((o (ugen-outputs u))
		       (n (length o)))
		  (if (< n 2)
		      u
		      (mce (map (lambda (i) (proxy u i))
				      (range 0 n))))))
     (else (error "proxify" "illegal ugen" u)))))

;; int -> maybe (float -> float) -> (node -> node)
(define mk-unary-operator
  (lambda (s f)
    (lambda (a)
      (if (and (number? a)
	       f)
	  (f a)
	  (construct-ugen "UnaryOpUGen" #f (list a) #f 1 s (uid 0))))))

;; int -> maybe (float -> float -> float) -> (node -> node -> node)
(define mk-binary-operator
  (lambda (s f)
    (lambda (a b)
      (if (and (number? a)
	       (number? b)
	       f)
	  (f a b)
	  (construct-ugen "BinaryOpUGen" #f (list a b) #f 1 s (uid 0))))))

;; string -> [symbol] -> int ~> (ugen ... -> ugen)
(define-syntax mk-filter
  (syntax-rules ()
    ((_ m (i ...) o)
     (lambda (i ...)
       (construct-ugen m #f (list i ...) #f o 0 (uid 0))))))

;; string -> [symbol] ~> (int -> ugen ... -> ugen)
(define-syntax mk-filter-n
  (syntax-rules ()
    ((_ m (i ...))
     (lambda (nc i ...)
       (if (not (integer? nc))
	   (error "mk-filter-n" "illegal channel count" 'n nc)
	   #f)
       (let ((l (list i ...)))
	 (construct-ugen m #f l #f nc 0 (uid 0)))))))

;; string -> [symbol] -> int ~> (ugen ... -> ugen)
(define-syntax mk-filter-mce
  (syntax-rules ()
    ((_ m (i ... v) o)
     (lambda (i ... v)
       (construct-ugen m #f (list i ...) v o 0 (uid 0))))))

;; string -> [symbol] -> int ~> (ugen ... -> ugen)
(define-syntax mk-filter-id
  (syntax-rules ()
    ((_ m (i ...) o)
     (lambda (i ...)
       (construct-ugen m #f (list i ...) #f o 0 (unique-uid))))))

;; string -> [symbol] -> int -> int ~> (ugen ... -> ugen)
(define-syntax mk-filter-k
  (syntax-rules ()
    ;; k = keyed input
    ((_ m (i ...) o k)
     (lambda (i ...)
       (let ((l (list i ...)))
         (construct-ugen m (rate-of (list-ref l k)) l #f o 0 (uid 0)))))))

;; string -> [symbol] -> int ~> (rate -> ugen ... -> ugen)
(define-syntax mk-oscillator
  (syntax-rules ()
    ((_ m (i ...) o)
     (lambda (r i ...)
       (construct-ugen m r (list i ...) #f o 0 (uid 0))))))

;; string -> [symbol] ~> (int -> rate -> ugen ... -> ugen)
(define-syntax mk-oscillator-n
  (syntax-rules ()
    ((_ m (i ...))
     (lambda (nc r i ...)
       (if (not (integer? nc))
	   (error "mk-oscillator-n" "illegal channel count:" 'n nc)
	   #f)
       (let ((l (list i ...)))
	 (construct-ugen m r l #f nc 0 (uid 0)))))))

;; string -> [symbol] -> int ~> (rate -> ugen ... -> ugen)
(define-syntax mk-oscillator-mce
  (syntax-rules ()
    ((_ m (i ... v) o)
     (lambda (r i ... v)
       (construct-ugen m r (list i ...) v o 0 (uid 0))))))

;; string -> [symbol] -> int ~> (rate -> ugen ... -> ugen)
(define-syntax mk-oscillator-id
  (syntax-rules ()
    ((_ m (i ...) o)
     (lambda (r i ...)
       (construct-ugen m r (list i ...) #f o 0 (unique-uid))))))

;; string -> [symbol] -> int -> rate ~> (ugen ... -> ugen)
(define-syntax mk-specialized
  (syntax-rules ()
    ((_ m (i ...) o r)
     (lambda (i ...)
       (construct-ugen m r (list i ...) #f o 0 (uid 0))))))

;; string -> int -> rate ~> ugen
(define-syntax mk-specialized-c
  (syntax-rules ()
    ((_ m o r)
     (construct-ugen m r '() #f o 0 (uid 0)))))

;; string -> [symbol] -> int -> rate ~> (ugen ... -> ugen)
(define-syntax mk-specialized-mce
  (syntax-rules ()
    ((_ m (i ... v) o r)
     (lambda (i ... v)
       (construct-ugen m r (list i ...) v o 0 (uid 0))))))

;; string -> [symbol] -> rate ~> (int -> ugen ... -> ugen)
(define-syntax mk-specialized-n
  (syntax-rules ()
    ((_ m (i ...) r)
     (lambda (nc i ...)
       (if (not (integer? nc))
	   (error "mk-specialized-n" "illegal channel count:" 'n nc)
	   #f)
       (let ((l (list i ...)))
	 (construct-ugen m r l #f nc 0 (uid 0)))))))

;; string -> [symbol] -> int -> rate ~> (ugen ... -> ugen)
(define-syntax mk-specialized-id
  (syntax-rules ()
    ((_ m (i ...) o r)
     (lambda (i ...)
       (construct-ugen m r (list i ...) #f o 0 (unique-uid))))))

;; string -> [symbol] -> int -> rate ~> (ugen ... -> ugen)
(define-syntax mk-specialized-mce-id
  (syntax-rules ()
    ((_ m (i ... v) o r)
     (lambda (i ... v)
       (construct-ugen m r (list i ...) v o 0 (unique-uid))))))

(define *unary-op-code-hash* (make-hash))

(define (unary-op-add-code code name)
  (hash-set! *unary-op-code-hash* code name))

(define (unary-op-code->name code)
  (hash-ref *unary-op-code-hash* code))

(define-syntax define-unary-op
  (syntax-rules ()
    ((_ name code v)
     (begin
       (define name (mk-unary-operator code v))
       (unary-op-add-code code 'name)))))

(define *binary-op-code-hash* (make-hash))

(define (binary-op-add-code code name)
  (hash-set! *binary-op-code-hash* code name))

(define (binary-op-code->name code)
  (hash-ref *binary-op-code-hash* code))

(define-syntax define-binary-op
  (syntax-rules ()
    ((_ name code v)
     (begin
       (define name (mk-binary-operator code v))
       (binary-op-add-code code 'name)))))

(define *ugen-hash* (make-hash))

(define (ugen-add-hash name sym)
  (hash-set! *ugen-hash* name sym))

(define (ugen-name->symbol code)
  (hash-ref *ugen-hash* code))

;;; TODO: remove second macro level (mk-???) 
(define-syntax define-filter
  (syntax-rules ()
    ((_ sym name args v)
     (begin
       (define sym (mk-filter name args v))
       (ugen-add-hash name 'sym)))))

(define-syntax define-filter-mce
  (syntax-rules ()
    ((_ sym name args v)
     (begin
       (define sym (mk-filter-mce name args v))
       (ugen-add-hash name 'sym)))))

(define-syntax define-filter-n
  (syntax-rules ()
    ((_ sym name args)
     (begin
       (define sym (mk-filter-n name args))
       (ugen-add-hash name 'sym)))))

(define-syntax define-filter-id
  (syntax-rules ()
    ((_ sym name args v)
     (begin
       (define sym (mk-filter-id name args v))
       (ugen-add-hash name 'sym)))))

(define-syntax define-filter-k
  (syntax-rules ()
    ((_ sym name args v k)
     (begin
       (define sym (mk-filter-k name args v k))
       (ugen-add-hash name 'sym)))))

(define-syntax define-oscillator
  (syntax-rules ()
    ((_ sym name args v)
     (begin
       (define sym (mk-oscillator name args v))
       (ugen-add-hash name 'sym)))))

(define-syntax define-oscillator-mce
  (syntax-rules ()
    ((_ sym name args v)
     (begin
       (define sym (mk-oscillator-mce name args v))
       (ugen-add-hash name 'sym)))))

(define-syntax define-oscillator-n
  (syntax-rules ()
    ((_ sym name args)
     (begin
       (define sym (mk-oscillator-n name args))
       (ugen-add-hash name 'sym)))))

(define-syntax define-oscillator-k
  (syntax-rules ()
    ((_ sym name args v)
     (begin
       (define sym (mk-oscillator-k name args v))
       (ugen-add-hash name 'sym)))))

(define-syntax define-oscillator-id
  (syntax-rules ()
    ((_ sym name args v)
     (begin
       (define sym (mk-oscillator-id name args v))
       (ugen-add-hash name 'sym)))))

(define-syntax define-specialized
  (syntax-rules ()
    ((_ sym name args o r)
     (begin
       (define sym (mk-specialized name args o r))
       (ugen-add-hash name 'sym)))))

(define-syntax define-specialized-c
  (syntax-rules ()
    ((_ sym name o r)
     (begin
       (define sym (mk-specialized-c name o r))
       (ugen-add-hash name 'sym)))))

(define-syntax define-specialized-mce
  (syntax-rules ()
    ((_ sym name args o r)
     (begin
       (define sym (mk-specialized-mce name args o r))
       (ugen-add-hash name 'sym)))))

(define-syntax define-specialized-n
  (syntax-rules ()
    ((_ sym name args r)
     (begin
       (define sym (mk-specialized-n name args r))
       (ugen-add-hash name 'sym)))))

(define-syntax define-specialized-id
  (syntax-rules ()
    ((_ sym name args o r)
     (begin
       (define sym (mk-specialized-id name args o r))
       (ugen-add-hash name 'sym)))))

(define-syntax define-specialized-mce-id
  (syntax-rules ()
    ((_ sym name args o r)
     (begin
       (define sym (mk-specialized-id name args o r))
       (ugen-add-hash name 'sym)))))

;; ugen -> ugen
(define-unary-op u:abs 5 abs)
(define-unary-op amp-db 22 s:amp-db)
(define-unary-op arc-cos 32 acos)
(define-unary-op arc-sin 31 asin)
(define-unary-op arc-tan 33 atan)
(define-unary-op as-float 6 #f)
(define-unary-op as-int 7 #f)
(define-unary-op bi-lin-rand 40 #f)
(define-unary-op bit-not 4 #f)
(define-unary-op cps-midi 18 s:cps-midi)
(define-unary-op cps-oct 24 s:cps-oct)
(define-unary-op ceil 8 ceiling)
(define-unary-op coin 44 #f)
(define-unary-op u:cos 29 cos)
(define-unary-op cos-h 35 #f)
(define-unary-op cubed 13 s:cubed)
(define-unary-op db-amp 21 s:db-amp)
(define-unary-op digit-value 45 #f)
(define-unary-op distort 42 #f)
(define-unary-op u:exp 15 exp)
(define-unary-op u:floor 9 floor)
(define-unary-op frac 10 #f)
(define-unary-op han-window 49 #f)
(define-unary-op is-nil 2 #f)
(define-unary-op u:log 25 log)
(define-unary-op u:log10 27 s:log10)
(define-unary-op u:log2 26 s:log2)
(define-unary-op midi-cps 17 s:midi-cps)
(define-unary-op midi-ratio 19 s:midi-ratio)
(define-unary-op neg 0 -)
(define-unary-op u:not 1 #f)
(define-unary-op not-nil 3 #f)
(define-unary-op oct-cps 23 s:oct-cps)
(define-unary-op rand2 38 #f)
(define-unary-op ratio-midi 20 s:ratio-midi)
(define-unary-op recip 16 s:recip)
(define-unary-op rect-window 48 #f)
(define-unary-op scurve 53 #f)
(define-unary-op sign 11 #f)
(define-unary-op silence 46 #f)
(define-unary-op u:sin 28 sin)
(define-unary-op sin-h 34 #f)
(define-unary-op soft-clip 43 #f)
(define-unary-op u:sqrt 14 sqrt)
(define-unary-op squared 12 s:squared)
(define-unary-op sum3rand 41 #f)
(define-unary-op u:tan 30 tan)
(define-unary-op tan-h 36 #f)
(define-unary-op thru 47 #f)
(define-unary-op tri-window 51 #f)
(define-unary-op welch-window 50 #f)
(define-unary-op lin-rand* 39 #f)
(define-unary-op ramp* 52 #f)
(define-unary-op rand* 37 #f)

;; ugen -> ugen -> ugen
(define-binary-op am-clip 40 #f)
(define-binary-op abs-dif 38 #f)
(define-binary-op add 0 +)
(define-binary-op atan2 22 #f)
(define-binary-op bit-and 14 #f)
(define-binary-op bit-or 15 #f)
(define-binary-op bit-xor 16 #f)
(define-binary-op clip2 42 #f)
(define-binary-op dif-sqr 34 #f)
(define-binary-op eq 6 #f)
(define-binary-op excess 43 #f)
(define-binary-op exp-rand-range 48 #f)
(define-binary-op fdiv 4 /)
(define-binary-op fill 29 #f)
(define-binary-op first-arg 46 #f)
(define-binary-op fold2 44 #f)
(define-binary-op u:gcd 18 #f)
(define-binary-op ge 11 s:ge)
(define-binary-op gt 9 s:gt)
(define-binary-op hypot 23 #f)
(define-binary-op hypotx 24 #f)
(define-binary-op idiv 3 #f)
(define-binary-op u:lcm 17 #f)
(define-binary-op le 10 s:le)
(define-binary-op lt 8 s:lt)
(define-binary-op u:max 13 max)
(define-binary-op u:min 12 min)
(define-binary-op u:mod 5 #f)
(define-binary-op mul 2 *)
(define-binary-op ne 7 #f)
(define-binary-op pow 25 #f)
(define-binary-op rand-range 47 #f)
(define-binary-op ring1 30 #f)
(define-binary-op ring2 31 #f)
(define-binary-op ring3 32 #f)
(define-binary-op ring4 33 #f)
(define-binary-op u:round 19 #f)
(define-binary-op round-up 20 #f)
(define-binary-op scale-neg 41 #f)
(define-binary-op shift-left 26 #f)
(define-binary-op shift-right 27 #f)
(define-binary-op sqr-dif 37 #f)
(define-binary-op sqr-sum 36 #f)
(define-binary-op sub 1 -)
(define-binary-op sum-sqr 35 #f)
(define-binary-op thresh 39 #f)
(define-binary-op trunc 21 #f)
(define-binary-op unsigned-shift 28 #f)
(define-binary-op wrap2 45 #f)

(define-filter allpass-c "AllpassC" (in max-dt dt decay) 1)
(define-filter allpass-l "AllpassL" (in max-dt dt decay) 1)
(define-filter allpass-n "AllpassN" (in max-dt dt decay) 1)
(define-filter amp-comp "AmpComp" (freq root exp) 1)
(define-filter amp-comp-a "AmpCompA" (freq root min-amp root-amp) 1)
(define-filter apf "APF" (in freq radius) 1)
(define-filter balance2 "Balance2" (left right pos level) 1)
(define-filter ball "Ball" (in g damp friction) 1)
(define-filter bi-pan-b2 "BiPanB2" (in-a in-b azimuth gain) 3)
(define-filter bpf "BPF" (in freq rq) 1)
(define-filter bpz2 "BPZ2" (in) 1)
(define-filter brf "BRF" (in freq rq) 1)
(define-filter brz2 "BRZ2" (in) 1)
(define-filter buf-allpass-c "BufAllpassC" (buf in dt decay) 1)
(define-filter buf-allpass-l "BufAllpassL" (buf in dt decay) 1)
(define-filter buf-allpass-n "BufAllpassN" (buf in dt decay) 1)
(define-filter buf-comb-c "BufCombC" (buf in dt decay) 1)
(define-filter buf-comb-l "BufCombL" (buf in dt decay) 1)
(define-filter buf-comb-n "BufCombN" (buf in dt decay) 1)
(define-filter buf-delay-c "BufDelayC" (buf in dt) 1)
(define-filter buf-delay-l "BufDelayL" (buf in dt) 1)
(define-filter buf-delay-n "BufDelayN" (buf in dt) 1)
(define-filter-mce buf-wr "BufWr" (bufnum phase loop input-array) 1)
(define-filter clip "Clip" (in lo hi) 1)
(define-filter-id coin-gate "CoinGate" (prob in) 1)
(define-filter comb-c "CombC" (in max-dt dt decay) 1)
(define-filter comb-l "CombL" (in max-dt dt decay) 1)
(define-filter comb-n "CombN" (in max-dt dt decay) 1)
(define-filter compander "Compander" (in ctl thr sl-b sl-a cl-tm rx-tm) 1)
(define-filter compander-d "CompanderD" (in thr sl-b sl-a cl-tm rx-tm) 1)
(define-filter decay "Decay" (in decay-time) 1)
(define-filter decay2 "Decay2" (in attack-time decay-time) 1)
(define-filter-n decode-b2 "DecodeB2" (w x y orientation))
(define-filter degree-to-key "DegreeToKey" (bufnum in octave) 1)
(define-filter delay-c "DelayC" (in max-dt dt) 1)
(define-filter delay-l "DelayL" (in max-dt dt) 1)
(define-filter delay-n "DelayN" (in max-dt dt) 1)
(define-filter delay1 "Delay1" (in) 1)
(define-filter delay2 "Delay2" (in) 1)
(define-filter-k demand "Demand" (trig reset demand-ugens) 1 0)
(define-filter detect-silence "DetectSilence" (in amp time done-action) 1)
(define-filter-mce disk-out "DiskOut" (bufnum array) 0)
(define-filter done "Done" (src) 1)
(define-filter fold "Fold" (in lo hi) 1)
(define-filter formlet "Formlet" (in freq attacktime decay) 1)
(define-filter fos "FOS" (in a0 a1 b1) 1)
(define-filter free "Free" (in node-id) 1)
(define-filter free-self "FreeSelf" (in) 1)
(define-filter free-self-when-done "FreeSelfWhenDone" (in) 1)
(define-filter free-verb "FreeVerb" (in mix room damp) 1)
(define-filter free-verb2 "FreeVerb2" (in1 in2 mix room damp) 2)
(define-filter gate "Gate" (in trig) 1)
(define-filter hasher "Hasher" (in) 1)
(define-filter hilbert "Hilbert" (in) 2)
(define-filter hpf "HPF" (in freq) 1)
(define-filter hpz1 "HPZ1" (in) 1)
(define-filter hpz2 "HPZ2" (in) 1)
(define-filter image-warp "ImageWarp" (pic x y interpolation-type) 1)
(define-filter in-range "InRange" (in lo hi) 1)
(define-filter in-rect "InRect" (x y rect) 1)
(define-filter index "Index" (bufnum in) 1)
(define-filter integrator "Integrator" (in coef) 1)
(define-filter-mce klank "Klank" (i f-scale f-offset dscale spec) 1)
(define-filter lag "Lag" (in lag-time) 1)
(define-filter lag-control "LagControl" () 1)
(define-filter lag2 "Lag2" (in lag-time) 1)
(define-filter lag3 "Lag3" (in lag-time) 1)
(define-filter last-value "LastValue" (in diff) 1)
(define-filter latch "Latch" (in trig) 1)
(define-filter leak-dc "LeakDC" (in coef) 1)
(define-filter least-change "LeastChange" (a b) 1)
(define-filter limiter "Limiter" (in level dur) 1)
(define-filter lin-exp "LinExp" (in srclo srchi dstlo dsthi) 1)
(define-filter lin-pan2 "LinPan2" (in pos level) 2)
(define-filter lin-x-fade2 "LinXFade2" (in-a in-b pan level) 1)
(define-filter linen "Linen" (gate atk-tm sus-lvl rel-tm done-action) 1)
(define-filter-mce local-out "LocalOut" (array) 0)
(define-filter lpf "LPF" (in freq) 1)
(define-filter lpz1 "LPZ1" (in) 1)
(define-filter lpz2 "LPZ2" (in) 1)
(define-filter mantissa-mask "MantissaMask" (in bits) 1)
(define-filter median "Median" (length in) 1)
(define-filter mid-eq "MidEq" (in freq rq db) 1)
(define-filter moog-ff "MoogFF" (in freq gain reset) 1)
(define-filter most-change "MostChange" (a b) 1)
(define-filter mul-add "MulAdd" (a b c) 1)
(define-filter normalizer "Normalizer" (in level dur) 1)
(define-filter-mce offset-out "OffsetOut" (bus inputs) 0)
(define-filter one-pole "OnePole" (in coef) 1)
(define-filter one-zero "OneZero" (in coef) 1)
(define-filter-mce out "Out" (bus inputs) 0)
(define-filter pan-az "PanAz" (nc in pos lvl wdth orientation) 1)
(define-filter pan-b "PanB" (in azimuth elevation gain) 3)
(define-filter pan-b2 "PanB2" (in azimuth gain) 3)
(define-filter pan2 "Pan2" (in pos level) 2)
(define-filter pan4 "Pan4" (in xpos ypos level) 4)
(define-filter pause "Pause" (in node-id) 1)
(define-filter pause-self "PauseSelf" (in) 1)
(define-filter pause-self-when-done "PauseSelfWhenDone" (in) 1)
(define-filter peak "Peak" (trig reset) 1)
(define-filter peak-follower "PeakFollower" (in decay) 1)
(define-filter pitch-shift "PitchShift" (in win-sz p-rt p-dp t-dp) 1)
(define-filter pluck "Pluck" (in trig max-dt dt decay coef) 1)
(define-filter-mce poll "Poll" (trig in trig-id label) 0)
(define-filter pulse-count "PulseCount" (trig reset) 1)
(define-filter pulse-divider "PulseDivider" (trig div start) 1)
(define-filter ramp "Ramp" (in lag-time) 1)
(define-filter-mce record-buf "RecordBuf" (b off rl pl r lp tr i) 1)
(define-filter-mce replace-out "ReplaceOut" (bus inputs) 0)
(define-filter resonz "Resonz" (in freq bwr) 1)
(define-filter rhpf "RHPF" (in freq rq) 1)
(define-filter ringz "Ringz" (in freq decay) 1)
(define-filter rlpf "RLPF" (in freq rq) 1)
(define-filter rotate2 "Rotate2" (x y pos) 2)
(define-filter running-max "RunningMax" (in trig) 1)
(define-filter running-min "RunningMin" (in trig) 1)
(define-filter running-sum "RunningSum" (in numsamp) 1)
(define-filter schmidt "Schmidt" (in lo hi) 1)
(define-filter-mce scope-out "ScopeOut" (input-array bufnum) 0)
(define-filter-mce select "Select" (which array) 1)
(define-filter send-trig "SendTrig" (in id value) 0)
(define-filter set-reset-ff "SetResetFF" (trig reset) 1)
(define-filter shaper "Shaper" (bufnum in) 1)
(define-filter-n silent "Silent" ())
(define-filter slew "Slew" (in up dn) 1)
(define-filter slope "Slope" (in) 1)
(define-filter sos "SOS" (in a0 a1 a2 b1 b2) 1)
(define-filter spring "Spring" (in spring damp) 1)
(define-filter stepper "Stepper" (trig reset min max step resetval) 1)
(define-filter sweep "Sweep" (trig rate) 1)
(define-filter t-ball "TBall" (in g damp friction) 1)
(define-filter t-delay "TDelay" (in dur) 1)
(define-filter-id t-exp-rand "TExpRand" (lo hi trig) 1)
(define-filter-n t-grains "TGrains" (tr b rt c-pos dur pan amp interp))
(define-filter t-pulse "TPulse" (trig freq width) 1)
(define-filter-id t-rand "TRand" (lo hi trig) 1)
(define-filter-id ti-rand "TIRand" (lo hi trig) 1)
(define-filter timer "Timer" (trig) 1)
(define-filter toggle-ff "ToggleFF" (trig) 1)
(define-filter trapezoid "Trapezoid" (in a b c d) 1)
(define-filter trig "Trig" (in dur) 1)
(define-filter trig1 "Trig1" (in dur) 1)
(define-filter-mce tw-index "TWindex" (in normalize array) 1)
(define-filter two-pole "TwoPole" (in freq radius) 1)
(define-filter two-zero "TwoZero" (in freq radius) 1)
(define-filter vibrato "Vibrato" (f rt dpth dly onset rvar dvar iphase) 1)
(define-filter wrap "Wrap" (in lo hi) 1)
(define-filter wrap-index "WrapIndex" (bufnum in) 1)
(define-filter x-fade2 "XFade2" (in-a in-b pan level) 1)
(define-filter-mce x-out "XOut" (bus xfade inputs) 0)
(define-filter xy "XY" (xscale yscale xoff yoff rot rate) 1)
(define-filter zero-crossing  "ZeroCrossing" (in) 1)

(define-oscillator amplitude "Amplitude" (in atk-tm rel-tm) 1)
(define-oscillator blip "Blip" (freq numharm) 1)
(define-oscillator-id brown-noise "BrownNoise" () 1)
(define-oscillator buf-channels "BufChannels" (buf) 1)
(define-oscillator buf-dur "BufDur" (buf) 1)
(define-oscillator buf-frames "BufFrames" (buf) 1)
(define-oscillator buf-rate-scale "BufRateScale" (buf) 1)
(define-oscillator-n buf-rd "BufRd" (bufnum phase loop interp))
(define-oscillator buf-sample-rate "BufSampleRate" (buf) 1)
(define-oscillator buf-samples "BufSamples" (buf) 1)
(define-oscillator c-osc "COsc" (bufnum freq beats) 1)
(define-oscillator-id clip-noise "ClipNoise" () 1)
(define-oscillator crackle "Crackle" (chaos-param) 1)
(define-oscillator cusp-l "CuspL" (freq a b xi) 1)
(define-oscillator cusp-n "CuspN" (freq a b xi) 1)
(define-oscillator demand-env-gen "DemandEnvGen" (l d s c g r ls lb ts da) 1)
(define-oscillator-n disk-in "DiskIn" (bufnum))
(define-oscillator-id dust "Dust" (density) 1)
(define-oscillator-id dust2 "Dust2" (density) 1)
(define-oscillator-mce duty "Duty" (dur reset da lvl) 1)
(define-oscillator-mce env-gen "EnvGen" (g ls lb ts da spec) 1)
(define-oscillator f-sin-osc "FSinOsc" (freq iphase) 1)
(define-oscillator fb-sine-c "FBSineC" (freq im fb a c xi yi) 1)
(define-oscillator fb-sine-l "FBSineL" (freq im fb a c xi yi) 1)
(define-oscillator fb-sine-n "FBSineN" (freq im fb a c xi yi) 1)
(define-oscillator formant "Formant" (fundfreq formfreq bwfreq) 1)
(define-oscillator gbman-c "GbmanC" (freq xi yi) 1)
(define-oscillator gbman-l "GbmanL" (freq xi yi) 1)
(define-oscillator gbman-n "GbmanN" (freq xi yi) 1)
(define-oscillator gendy1 "Gendy1" (ad dd adp ddp mnf mxf as ds ic kn) 1)
(define-oscillator gendy2 "Gendy2" (ad dd adp ddp mnf mxf as ds ic kn a c) 1)
(define-oscillator gendy3 "Gendy3" (ad dd adp ddp f as ds ic kn) 1)
(define-oscillator-id gray-noise "GrayNoise" () 1)
(define-oscillator henon-c "HenonC" (freq a b x0 x1) 1)
(define-oscillator henon-l "HenonL" (freq a b x0 x1) 1)
(define-oscillator henon-n "HenonN" (freq a b x0 x1) 1)
(define-oscillator impulse "Impulse" (freq phase) 1)
(define-oscillator-n in "In" (bus))
(define-oscillator key-state "KeyState" (key min max lag) 1)
(define-oscillator-mce klang "Klang" (freqscale freqoffset spec-array) 1)
(define-oscillator latoocarfian-c "LatoocarfianC" (freq a b c d xi yi) 1)
(define-oscillator latoocarfian-l "LatoocarfianL" (freq a b c d xi yi) 1)
(define-oscillator latoocarfian-n "LatoocarfianN" (freq a b c d xi yi) 1)
(define-oscillator-id lf-clip-noise "LFClipNoise" (freq) 1)
(define-oscillator lf-cub "LFCub" (freq iphase) 1)
(define-oscillator-id lf-noise0 "LFNoise0" (freq) 1)
(define-oscillator-id lf-noise1 "LFNoise1" (freq) 1)
(define-oscillator-id lf-noise2 "LFNoise2" (freq) 1)
(define-oscillator lf-par "LFPar" (freq iphase) 1)
(define-oscillator lf-pulse "LFPulse" (freq iphase width) 1)
(define-oscillator lf-saw "LFSaw" (freq iphase) 1)
(define-oscillator lf-tri "LFTri" (freq iphase) 1)
(define-oscillator-id lfd-clip-noise "LFDClipNoise" (freq) 1)
(define-oscillator-id lfd-noise0 "LFDNoise0" (freq) 1)
(define-oscillator-id lfd-noise1 "LFDNoise1" (freq) 1)
(define-oscillator-id lfd-noise3 "LFDNoise3" (freq) 1)
(define-oscillator lin-cong-c "LinCongC" (freq a c m xi) 1)
(define-oscillator lin-cong-l "LinCongL" (freq a c m xi) 1)
(define-oscillator lin-cong-n "LinCongN" (freq a c m xi) 1)
(define-oscillator line "Line" (start end dur done-action) 1)
(define-oscillator-n local-in "LocalIn" ())
(define-oscillator logistic "Logistic" (chaos-param freq) 1)
(define-oscillator lorenz-l "LorenzL" (freq s r b h xi yi zi) 1)
(define-oscillator mouse-button "MouseButton" (minval maxval lag) 1)
(define-oscillator mouse-x "MouseX" (min max warp lag) 1)
(define-oscillator mouse-y "MouseY" (min max warp lag) 1)
(define-oscillator-id noah-noise "NoahNoise" () 1)
(define-oscillator osc "Osc" (bufnum freq phase) 1)
(define-oscillator osc-n "OscN" (bufnum freq phase) 1)
(define-oscillator p-sin-grain "PSinGrain" (freq dur amp) 1)
(define-oscillator phasor "Phasor" (trig rate start end reset-pos) 1)
(define-oscillator-id pink-noise "PinkNoise" () 1)
(define-oscillator pulse "Pulse" (freq width) 1)
(define-oscillator quad-c "QuadC" (freq a b c xi) 1)
(define-oscillator quad-l "QuadL" (freq a b c xi) 1)
(define-oscillator quad-n "QuadN" (freq a b c xi) 1)
(define-oscillator-id rand-id "RandID" (id) 1)
(define-oscillator-id rand-seed "RandSeed" (trig seed) 1)
(define-oscillator saw "Saw" (freq) 1)
(define-oscillator shared-in "SharedIn" () 1)
(define-oscillator sin-osc "SinOsc" (freq phase) 1)
(define-oscillator sin-osc-fb "SinOscFB" (freq feedback) 1)
(define-oscillator standard-l "StandardL" (freq k xi yi) 1)
(define-oscillator standard-n "StandardN" (freq k xi yi) 1)
(define-oscillator sync-saw "SyncSaw" (sync-freq saw-freq) 1)
(define-oscillator-mce t-duty "TDuty" (dur reset done-action level gap) 1)
(define-oscillator-n trig-control "TrigControl" ())
(define-oscillator v-osc "VOsc" (bufpos freq phase) 1)
(define-oscillator v-osc3 "VOsc3" (bufpos freq1 freq2 freq3) 1)
(define-oscillator var-saw "VarSaw" (freq iphase width) 1)
(define-oscillator-id white-noise "WhiteNoise" () 1)
(define-oscillator x-line "XLine" (start end dur done-action) 1)

(define-specialized-c control-rate "ControlRate" 1 ir)
(define-specialized convolution "Convolution" (in kernel frame-size) 1 ar)
(define-specialized convolution2 "Convolution2" (in b tr frame-size) 1 ar)
(define-specialized-id dbrown "Dbrown" (length lo hi step) 1 dr)
(define-specialized-id dbufrd "Dbufrd" (bufnum phase loop) 1 dr)
(define-specialized-id dgeom "Dgeom" (length start grow) 1 dr)
(define-specialized-id dibrown "Dibrown" (length lo hi step) 1 dr)
(define-specialized-id diwhite "Diwhite" (length lo hi) 1 dr)
(define-specialized-mce-id drand "Drand" (length array) 1 dr)
(define-specialized-mce-id dseq "Dseq" (length array) 1 dr)
(define-specialized-mce-id dser "Dser" (length array) 1 dr)
(define-specialized-id dseries "Dseries" (length start step) 1 dr)
(define-specialized-mce-id dswitch "Dswitch" (length array) 1 dr)
(define-specialized-mce-id dswitch1 "Dswitch1" (length array) 1 dr)
(define-specialized-id dwhite "Dwhite" (length lo hi) 1 dr)
(define-specialized-mce-id dxrand "Dxrand" (length array) 1 dr)
(define-specialized-id exp-rand "ExpRand" (lo hi) 1 ir)
(define-specialized fft "FFT" (buf in hop wintype active winsize) 1 kr)
(define-specialized-n grain-buf "GrainBuf" (tr dur sndb rt ps i pan envb) ar)
(define-specialized-n grain-fm "GrainFM" (tr dur cf mf indx pan envb) ar)
(define-specialized-n grain-in "GrainIn" (tr dur in pan envbuf) ar)
(define-specialized-n grain-sin "GrainSin" (tr dur freq pan envbuf) ar)
(define-specialized-id i-rand "IRand" (lo hi) 1 ir)
(define-specialized ifft "IFFT" (buf wintype winsize) 1 ar)
(define-specialized-n in-feedback "InFeedback" (bus) ar)
(define-specialized-n in-trig "InTrig" (bus) kr)
(define-specialized k2a "K2A" (in) 1 ar)
(define-specialized-n lag-in "LagIn" (bus lag) kr)
(define-specialized-id lin-rand "LinRand" (lo hi minmax) 1 ir)
(define-specialized-id n-rand "NRand" (lo hi n) 1 ir)
(define-specialized-c num-audio-buses "NumAudioBuses" 1 ir)
(define-specialized-c num-buffers "NumBuffers" 1 ir)
(define-specialized-c num-control-buses "NumControlBuses" 1 ir)
(define-specialized-c num-input-buses "NumInputBuses" 1 ir)
(define-specialized-c num-output-buses "NumOutputBuses" 1 ir)
(define-specialized-c num-running-synths "NumRunningSynths" 1 ir)
(define-specialized-mce pack-fft "PackFFT" (b sz fr to z mp) 1 kr)
(define-specialized pitch "Pitch" (in if mnf mxf ef mxb m at pt ds) 2 kr)
(define-specialized-n play-buf "PlayBuf" (b rt tr start loop) ar)
(define-specialized pv-add "PV_Add" (buf-a buf-b) 1 kr)
(define-specialized pv-bin-scramble "PV_BinScramble" (b wipe width trig) 1 kr)
(define-specialized pv-bin-shift "PV_BinShift" (b stretch shift) 1 kr)
(define-specialized pv-bin-wipe "PV_BinWipe" (b-a b-b wipe) 1 kr)
(define-specialized pv-brick-wall "PV_BrickWall" (b wipe) 1 kr)
(define-specialized pv-conformal-map "PV_ConformalMap" (b real imag) 1 kr)
(define-specialized pv-copy "PV_Copy" (b-a b-b) 1 kr)
(define-specialized pv-copy-phase "PV_CopyPhase" (b-a b-b) 1 kr)
(define-specialized pv-diffuser "PV_Diffuser" (b trig) 1 kr)
(define-specialized pv-hainsworth-foote "PV_HainsworthFoote" (b h f t w) 1 ar)
(define-specialized pv-jensen-andersen "PV_JensenAndersen" (b c e f s t w) 1 ar)
(define-specialized pv-local-max "PV_LocalMax" (b threshold) 1 kr)
(define-specialized pv-mag-above "PV_MagAbove" (b threshold) 1 kr)
(define-specialized pv-mag-below "PV_MagBelow" (b threshold) 1 kr)
(define-specialized pv-mag-clip "PV_MagClip" (b threshold) 1 kr)
(define-specialized pv-mag-freeze "PV_MagFreeze" (b freeze) 1 kr)
(define-specialized pv-mag-mul "PV_MagMul" () 1 kr)
(define-specialized pv-mag-noise "PV_MagNoise" (b) 1 kr)
(define-specialized pv-mag-shift "PV_MagShift" () 1 kr)
(define-specialized pv-mag-smear "PV_MagSmear" (b bins) 1 kr)
(define-specialized pv-mag-squared "PV_MagSquared" () 1 kr)
(define-specialized pv-max "PV_Max" () 1 kr)
(define-specialized pv-min "PV_Min" () 1 kr)
(define-specialized pv-mul "PV_Mul" () 1 kr)
(define-specialized pv-phase-shift "PV_PhaseShift" (b shift) 1 kr)
(define-specialized pv-phase-shift270 "PV_PhaseShift270" (b) 1 kr)
(define-specialized pv-phase-shift90 "PV_PhaseShift90" (b) 1 kr)
(define-specialized pv-rand-comb "PV_RandComb" (b wipe trig) 1 kr)
(define-specialized pv-rand-wipe "PV_RandWipe" (b-a b-b wipe trig) 1 kr)
(define-specialized pv-rect-comb "PV_RectComb" (b nt phase width) 1 kr)
(define-specialized pv-rect-comb2 "PV_RectComb2" () 1 kr)
(define-specialized-c radians-per-sample "RadiansPerSample" 1 ir)
(define-specialized-id rand "Rand" (lo hi) 1 ir)
(define-specialized-c sample-dur "SampleDur" 1 ir)
(define-specialized-c sample-rate "SampleRate" 1 ir)
(define-specialized shared-out "SharedOut" (bus inputs) 0 kr)
(define-specialized-c subsample-offset "SubsampleOffset" 1 ir)
(define-specialized unpack1-fft "Unpack1FFT" (c b bi wm) 1 dr)
(define-specialized-n warp1 "Warp1" (b ptr fs ws envb ov wrr i) ar)

;; ugen -> ugen -> ugen -> ugen
(define add3
  (lambda (a b c)
    (add (add a b) c)))

(define add4
  (lambda (a b c d)
    (add (add a b) (add c d))))

(define buf-rd-c
  (lambda (nc r b p l)
    (buf-rd nc r b p l 4)))

(define buf-rd-l
  (lambda (nc r b p l)
    (buf-rd nc r b p l 2)))

(define buf-rd-n
  (lambda (nc r b p l)
    (buf-rd nc r b p l 1)))

(define fft*
  (lambda (buf in)
    (fft buf in 0.5 0 1 0)))

(define ifft*
  (lambda (buf)
    (ifft buf 0 0)))

(define lin-lin
  (lambda (in srclo srchi dstlo dsthi)
    (let* ((scale (fdiv (sub dsthi dstlo) (sub srchi srclo)))
           (offset (sub dstlo (mul scale srclo))))
      (mul-add in scale offset))))

(define mul3
  (lambda (a b c)
    (mul (mul a b) c)))

(define mul4
  (lambda (a b c d)
    (mul (mul (mul a b) c) d)))

(define t-choose
  (lambda (trig array)
    (select (ti-rand 0 (length (mce-channels array)) trig) array)))

(define tw-choose
  (lambda (trig array weights normalize)
    (select (tw-index trig normalize weights) array)))

;; int
(define add-to-head 0)
(define add-to-tail 1)
(define add-before 2)
(define add-after 3)
(define add-replace 4)

;; int
(define gen-normalize 1)
(define gen-wavetable 2)
(define gen-clear 4)

;; int
(define do-nothing 0)
(define pause-synth 1)
(define remove-synth 2)

;; int
(define no-loop 0)
(define loop 1)

;; int
(define linear 0)
(define exponential 1)

(define quit
  (message "/quit" '()))

(define notify
  (lambda (i)
    (message "/notify" (list i))))

(define status
  (message "/status" '()))

(define dump-osc
  (lambda (i)
    (message "/dumpOSC" (list i))))

(define sync
  (lambda (i)
    (message "/sync" (list i))))

(define clear-sched
  (message "/clearSched" '()))

(define d-recv
  (lambda (b)
    (message "/d_recv" (list b))))

(define d-load
  (lambda (s)
    (message "/d_load" (list s))))

(define d-load-dir
  (lambda (s)
    (message "/d_loadDir" (list s))))

(define d-free1
  (lambda (s)
    (message "/d_free" (list s))))

(define n-free1
  (lambda (i)
    (message "/n_free" (list i))))

(define n-run1
  (lambda (i j)
    (message "/n_run" (list i j))))

(define n-set
  (lambda (i xys)
    (let ((z (concat-map (lambda (xy) (list (car xy) (cdr xy))) xys)))
      (message "/n_set" (cons i z)))))

(define n-set1
  (lambda (i s f)
    (message "/n_set" (list i s f))))

(define n-setn1
  (lambda (i s fs)
    (message "/n_setn" (cons i (cons s (cons (length fs) fs))))))

(define n-fill1
  (lambda (i s j f)
    (message "/n_fill" (list i s j f))))

(define n-map
  (lambda (i s j)
    (message "/n_map" (list i s j))))

(define n-mapn1
  (lambda (i s j k)
    (message "/n_mapn" (list i s j k))))

(define n-before
  (lambda (i j)
    (message "/n_before" (list i j))))

(define n-query
  (lambda (i)
    (message "/n_query" (list i))))

(define n-trace
  (lambda (i)
    (message "/n_trace" (list i))))

(define s-new0
  (lambda (s i j k)
    (message "/s_new" (list s i j k))))

(define s-new1
  (lambda (s i j k t f)
    (message "/s_new" (list s i j k t f))))

(define s-new2
  (lambda (s i j k t1 f1 t2 f2)
    (message "/s_new" (list s i j k t1 f1 t2 f2))))

(define s-new
  (lambda (s i j k cs)
    (message "/s_new" (append (list s i j k) cs))))

(define s-get1
  (lambda (i j)
    (message "/s_get" (list i j))))

(define s-getn1
  (lambda (i s j)
    (message "/s_getn" (list i s j))))

(define s-noid
  (lambda (i)
    (message "/s_noid" (list i))))

(define g-new1
  (lambda (i j k)
    (message "/g_new" (list i j k))))

(define g-head1
  (lambda (i j)
    (message "/g_head" (list i j))))

(define g-tail1
  (lambda (i j)
    (message "/g_tail" (list i j))))

(define g-free-all1
  (lambda (i)
    (message "/g_freeAll" (list i))))

(define g-deep-free1
  (lambda (i)
    (message "/g_deepFree" (list i))))

(define b-alloc
  (lambda (id frames channels)
    (message "/b_alloc" (list id frames channels))))

(define b-alloc-read
  (lambda (id path frame n)
    (message "/b_allocRead" (list id path frame n))))

(define b-read
  (lambda (id path frame n bframe flag)
    (message "/b_read" (list id path frame n bframe flag))))

(define b-write
  (lambda (id path header type frames start flag)
    (message "/b_write" (list id path header type frames start flag))))

(define b-free
  (lambda (i)
    (message "/b_free" (list i))))

(define b-zero
  (lambda (i)
    (message "/b_zero" (list i))))

(define b-set1
  (lambda (i j f)
    (message "/b_set" (list i j f))))

(define b-setn1
  (lambda (i j fs)
    (message "/b_setn" (cons i (cons j (cons (length fs) fs))))))

(define b-fill1
  (lambda (i j k f)
    (message "/b_fill" (list i j k f))))

(define b-close
  (lambda (i)
    (message "/b_close" (list i))))

(define b-query1
  (lambda (i)
    (message "/b_query" (list i))))

(define b-get1
  (lambda (i j)
    (message "/b_get" (list i j))))

(define b-getn1
  (lambda (i j k)
    (message "/b_getn" (list i j k))))

(define b-gen1
  (lambda (i s fs)
    (message "/b_gen" (cons i (cons s fs)))))

(define c-set1
  (lambda (i f)
    (message "/c_set" (list i f))))

(define c-setn1
  (lambda (i fs)
    (message "/c_setn" (cons i (cons (length fs) fs)))))

(define c-fill1
  (lambda (i j f)
    (message "/c_fill" (list i j f))))

(define c-get1
  (lambda (i)
    (message "/c_get" (list i))))

(define c-getn1
  (lambda (i j)
    (message "/c_getn" (list i j))))

;; port -> osc -> ()
(define async
  (lambda (fd m)
    (send fd m)
    (wait fd "/done")))

;; port -> graphdef -> ()
(define send-synthdef
  (lambda (fd u)
    (async fd (d-recv (encode-graphdef u)))))

;; port -> string -> ugen -> ()
(define send-synth
  (lambda (fd n u)
    (async fd (d-recv (encode-graphdef (synthdef n u))))))

;; osc message -> ()
(define (send-msg msg)
  (with-sc3 (lambda (fd)
              (send fd msg)))
              (void))

;; osc message -> ()
;; waits for /done
(define (send-async-msg msg)
  (with-sc3 (lambda (fd)
              (async fd msg))))



;; port -> ugen -> ()
(define play
  (lambda (fd u)
    (send-synth fd "anonymous" u)
    (send fd (s-new0 "anonymous" -1 1 1))))

;; (socket -> a) -> a
(define with-udp-sc3
  (lambda (f)
    (let* ((fd (udp:open "127.0.0.1" 57110))
	   (r (f fd)))
      (udp:close fd)
      r)))

;; (socket -> a) -> a
(define with-tcp-sc3
  (lambda (f)
    (let* ((fd (tcp:open "127.0.0.1" 57110))
	   (r (f fd)))
      (tcp:close fd)
      r)))

;; (socket -> a) -> a
(define with-sc3 with-udp-sc3)


;; port -> ()
(define (reset)
  (with-sc3 (lambda (fd)
              (send fd (bundle -1 (list (g-free-all1 0)
                                        clear-sched
                                        (g-new1 1 0 0))))))
  (void))



;; ((socket -> a) -> a) -> (ugen -> ())
(define audition-using
  (lambda (f)
    (lambda (u)
      (f
       (lambda (fd)
	 (play fd u))))))

;; ugen -> ()
(define audition (audition-using with-udp-sc3))


;; [string]
(define status-fields
  (list "# UGens                     "
	"# Synths                    "
	"# Groups                    "
	"# Instruments               "
	"% CPU (Average)             "
	"% CPU (Peak)                "
	"Sample Rate (Nominal)       "
	"Sample Rate (Actual)        "))

;; osc -> [string]
(define status-format
  (lambda (r)
    (cons "***** SuperCollider Server Status *****"
	  (zip-with string-append
		    status-fields
		    (map number->string (cdr (cdr r)))))))

;; port -> [string]
(define server-status
  (lambda (fd)
    (send fd status)
    (let ((r (wait fd "/status.reply")))
      (status-format r))))

;; port -> ()
(define display-server-status
  (lambda (fd)
    (newline)
    (for-each display (intersperse "\n" (server-status fd)))
    (newline)))

;; port -> int -> number
(define server-status-field
  (lambda (fd n)
    (send fd status)
    (let ((r (wait fd "/status.reply")))
      (list-ref r n))))

;; port -> float
(define server-sample-rate-nominal
  (lambda (s)
    (server-status-field s 8)))

;; port -> float
(define server-sample-rate-actual
  (lambda (s)
    (server-status-field s 9)))

;; string|number -> number
(define curve-to-shape
  (lambda (c)
    (cond
     ((string? c)
      (cond ((string=? c "step") 0.0)
	    ((string=? c "linear") 1.0)
	    ((string=? c "exponential") 2.0)
	    ((string=? c "sine") 3.0)
	    ((string=? c "welch") 4.0)
	    ((string=? c "squared") 6.0)
	    ((string=? c "cubed") 7.0)
	    (else (error "curve-to-shape" "string" c))))
     ((number? c)
      5.0)
     (else
      (error "curve-to-shape" "illegal curve" c)))))

;; any -> number
(define curve-to-value
  (lambda (c)
    (if (number? c) c 0.0)))

;; Make a <list> for use with the EnvGen UGen. `levels' is a <list>
;; containing the left to right gain values for the envelope, it has
;; one more element than the <list> `times', having the delta times
;; for each envelope segment. `curve' is either a string or a number
;; or a <list> of such, in either case it is expanded to a list of the
;; same length as `times'. `release-node' is the index of the
;; 'release' stage of the envelope, `loop-node' is the index of the
;; 'loop' stage of the envelope. These indices are set as invalid, by
;; convention -1, to indicate there is no such node.
(define env
  (lambda (levels times curves release-node loop-node)
    (mce
     (append
      (list (car levels) (length times) release-node loop-node)
      (concat
       (zip-with3
	(lambda (l t c)
	  (list l
		t
		(curve-to-shape c)
		(curve-to-value c)))
	(cdr levels)
	times
	curves))))))

(define d->dx
  (lambda (l)
    (zip-with sub (drop 1 l) l)))

;; Co-ordinate based static envelope generator.
;; [(ugen . ugen)] -> ugen -> ugen -> [ugen] -> ugen
(define env-coord
  (lambda (d dur amp curves)
    (env (map (lambda (e) (mul (cdr e) amp)) d)
         (map (lambda (e) (mul e dur)) (d->dx (map car d)))
         curves
         -1
         -1)))

(define env-coord-linear
  (lambda (d dur amp)
    (env-coord d dur amp (make-list (- (length d) 1) 1))))

;; (equal? (mk-coord (list 1 2 3 4)) (list (cons 1 2) (cons 3 4)))
(define mk-coord
  (lambda (l)
    (if (null? l)
        (list)
        (let ((x (car l))
              (y (cadr l))
              (r (cddr l)))
          (cons (cons x y) (mk-coord r))))))

(define env-bp
  (lambda (bp d a c) (env-coord (mk-coord bp) d a c)))

(define env-bp-linear
  (lambda (bp d a)
    (env-coord-linear (mk-coord bp) d a)))

;; Design a standard trapezoidal envelope. `shape' determines the
;; sustain time as a proportion of `dur', zero is a triangular
;; envelope, one a rectangular envelope. `skew' determines the
;; attack/decay ratio, zero is an immediate attack and a slow decay,
;; one a slow attack and an immediate decay. This implementation
;; builds a zero one breakpoint data set and calls env-coord.
(define env-trapezoid
  (lambda (shape skew dur amp)
    (let* ((x1 (mul skew (sub 1.0 shape)))
	   (bp (list (cons 0 (le skew 0.0))
		     (cons x1 1.0)
		     (cons (add shape x1) 1.0)
		     (cons 1.0 (ge skew 1.0)))))
      (env-coord bp dur amp (make-list 3 "linear")))))

(define env-triangle
  (lambda (dur level)
    (let ((half-dur (mul dur 0.5)))
      (env (list 0.0 level 0.0)
	   (list half-dur half-dur)
	   (list "linear" "linear")
	   -1
	   -1))))

(define env-sine
  (lambda (dur level)
    (let ((half-dur (mul dur 0.5)))
      (env (list 0.0 level 0.0)
	   (list half-dur half-dur)
	   (list "sine" "sine")
	   -1
	   -1))))

(define env-perc
  (lambda (attackTime releaseTime level curves)
    (env (list 0.0 level 0.0)
	 (list attackTime releaseTime)
	 curves
	 -1
	 -1)))

(define env-adsr
  (lambda (attackTime
	   decayTime
	   sustainLevel
	   releaseTime
	   peakLevel
	   curves
	   bias)
    (env (map (lambda (e) (mul e bias))
               (list 0.0 peakLevel (mul peakLevel sustainLevel) 0.0))
	 (list attackTime decayTime releaseTime)
	 curves
	 2
	 -1)))

(define env-asr
  (lambda (attackTime sustainLevel releaseTime curves)
    (env (list 0.0 sustainLevel 0.0)
	 (list attackTime releaseTime)
	 curves
	 1
	 -1)))

(define env-linen
  (lambda (attackTime sustainTime releaseTime level curves)
    (env (list 0.0 level level 0.0)
	 (list attackTime sustainTime releaseTime)
	 curves
	 -1
	 -1)))

;; [m] -> [p] -> [#, m, p...]
(define packfft-data
  (lambda (m p)
    (mce
     (cons (* 2 (length m))
	   (concat (zip-with list m p))))))

;; [[m, p]] -> [#, m, p...]
(define packfft-data*
  (lambda (mp)
    (mce
     (cons (* 2 (length mp))
	   (concat mp)))))

(define unpack-fft
  (lambda (c nf from to mp?)
    (map (lambda (i)
            (unpack1-fft c nf i mp?))
	 (range from (add1 to)))))

(define pvcollect
  (lambda (c nf f from to z?)
    (let* ((m (unpack-fft c nf from to 0))
	   (p (unpack-fft c nf from to 1))
	   (i (range from (add1 to)))
	   (e (zip-with3 f m p i)))
      (pack-fft c nf from to z? (packfft-data* e)))))

;; ugen -> ugen
(define sound-in
  (lambda (n)
    (if (mce? n)
	(let ((l (mce-proxies n)))
	  (if (consecutive? l)
	      (in (length l) ar (add num-output-buses (car l)))
	      (in 1 ar (add num-output-buses n))))
	(in 1 ar (add num-output-buses n)))))

;; [ugen] -> [ugen] -> [ugen] -> ugen
(define klang-data
  (lambda (freqs amps phases)
    (mce
     (concat
      (zip-with3
       list
       freqs amps phases)))))

;; [ugen] -> [ugen] -> [ugen] -> ugen
(define klank-data klang-data)

;; ugen -> ugen -> ugen -> ugen -> ugen -> ugen
(define dyn-klank
  (lambda (i fs fo ds s)
    (letrec ((gen (lambda (l)
		    (if (null? l)
			0
			(let ((f (list-ref l 0))
			      (a (list-ref l 1))
			      (d (list-ref l 2)))
			  (add (mul (ringz i (mul-add f fs fo) (mul d ds)) a)
			       (gen (drop 3 l))))))))
      (gen (mce-channels s)))))

;; ugen -> ugen -> ugen -> ugen
(define freq-shift
  (lambda (i f p)
    (let ((o (sin-osc ar f (mce2 (add p (* 0.5 pi)) p)))
	  (h (hilbert i)))
      (mix (mul h o)))))

;; rate -> ugen -> ugen -> ugen -> ugen -> ugen
(define pm-osc
  (lambda (r cf mf pm mp)
    (sin-osc r cf (mul (sin-osc r mf mp) pm))))

;; ugen -> ugen -> ugen
(define dcons
  (lambda (x xs)
    (let ((i (dseq 1 (mce2 0 1)))
	  (a (dseq 1 (mce2 x xs))))
      (dswitch i a))))

;; ugen|mce -> ugen
(define mix
  (lambda (u)
    (cond
     ((mce? u) (foldl1 add (mce-proxies u)))
     (else u))))

;; int -> (int -> ugen) -> mce
(define mce-fill
  (lambda (n f)
    (mce (map f (range 0 n)))))

;; int -> (int -> ugen) -> ugen
(define mix-fill
  (lambda (n f)
    (mix (mce-fill n f))))

;; float
(define dinf
  9.0e8)

;; float -> float -> float
(define random
  (lambda (a b)
    (+ (* (srfi:random-real) (- b a)) a)))

;; int -> int -> int
(define i-random
  (lambda (l r)
    (+ l (srfi:random-integer (- r l)))))

;; float -> float -> float
(define exp-random
  (lambda (a b)
    (let ((r (/ b a)))
      (* (expt r (random 0 1)) a))))

;; [a] -> a
(define choose
  (lambda (xs)
    (list-ref xs (srfi:random-integer (length xs)))))

;; () -> float
(define utc
  (lambda ()
    (let ((t (srfi:current-time srfi:time-utc)))
      (+ (srfi:time-second t)
         (/ (srfi:time-nanosecond t) 1e9)))))

;; Rate -> UGen -> UGen -> Warp -> UGen -> UGen
(define mouse-r
  (lambda (rt l r ty tm)
    (let ((f (if (= ty 0) lin-lin lin-exp)))
      (lag (f (lf-noise1 rt 1) -1 1 l r) tm))))

(define mouse-x* mouse-r)
(define mouse-y* mouse-r)

(define mouse-button*
  (lambda (rt l r tm)
    (let ((o (lf-clip-noise rt 1)))
      (lag (lin-lin o -1 1 l r) tm))))

;; * Timing

;; time.plt.scm ;;;;;;;;;


;; number -> void
(define thread-sleep sleep)

;; () -> float
(define utcr utc)

;; double -> void
(define pause-thread
  (lambda (n)
    (when (> n 1e-4)
        (thread-sleep n))))

;; double -> void
(define pause-thread-until
  (lambda (t)
    (let ((c (utcr)))
      (pause-thread (- t c)))))

;; Local Variables:
;; truncate-lines:t
;; End:





(module+ test
  (require rackunit)
  
  
  ;; name, ugen expr -> bytes (synthdef)
  ;; similar to send-synth in rsc3
  (define (ugens->synthdef name ugens)
    (encode-graphdef (synthdef name ugens)))
  
  
  ;; these should not break
  
  (check-equal? (ugens->synthdef "sine" (mul (sin-osc ar 440 0) 0.1))
                (bytes-append
                 #"SCgf\0\0\0\0\0\1\4sine\0\3C\334\0\0\0\0\0\0=\314\314\315"
                 #"\0\0\0\0\0\2\6SinOsc\2\0\2\0\1\0\0\377\377\0\0\377\377\0\1\2"
                 #"\fBinaryOpUGen\2\0\2\0\1\0\2\0\0\0\0\377\377\0\2\2"))
  
  (check-equal? (ugens->synthdef "sine0" (out 0 (mul (sin-osc ar 440 0) 0.1)))
                (bytes-append
                 #"SCgf\0\0\0\0\0\1\5sine0\0\3\0\0\0\0C\334\0\0=\314\314\315"
                 #"\0\0\0\0\0\3\6SinOsc\2\0\2\0\1\0\0\377\377\0\1\377\377\0\0\2"
                 #"\fBinaryOpUGen\2\0\2\0\1\0\2\0\0\0\0\377\377\0\2\2\3Out\2\0\2"
                 #"\0\0\0\0\377\377\0\0\0\1\0\0"))
  
  (check-equal? (ugens->synthdef "ring" (out 0 (mul (ring4 (f-sin-osc ar 800 0)
                                                           (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0))
                                                    0.125)))
                (bytes-append
                 #"SCgf\0\0\0\0\0\1\4ring\0\6\0\0\0\0DH\0\0CH\0\0C\372\0\0@\240\0\0>"
                 #"\0\0\0\0\0\0\0\0\6\5XLine\1\0\4\0\1\0\0\377\377\0\2\377\377\0\3"
                 #"\377\377\0\4\377\377\0\0\1\aFSinOsc\2\0\2\0\1\0\0\0\0\0\0\377\377"
                 #"\0\0\2\aFSinOsc\2\0\2\0\1\0\0\377\377\0\1\377\377\0\0\2\fBinaryOpUGen"
                 #"\2\0\2\0\1\0!\0\2\0\0\0\1\0\0\2\fBinaryOpUGen\2\0\2\0\1\0\2\0\3\0\0\377\377"
                 #"\0\5\2\3Out\2\0\2\0\0\0\0\377\377\0\0\0\4\0\0"))

  
  (check-equal? (ugens->synthdef  "twoSines" (out 0 (mce2 (mul (sin-osc ar 440. 0.0) 0.2)
                                                          (mul (sin-osc ar 554.365 0.0) 0.2))))
                (bytes-append
                 #"SCgf\0\0\0\0\0\1\btwoSines\0\5\0\0\0\0C\334\0\0\0\0\0\0>L\314\315"
                 #"D\n\227\\\0\0\0\0\0\5\6SinOsc\2\0\2\0\1\0\0\377\377\0\4\377\377\0"
                 #"\2\2\fBinaryOpUGen\2\0\2\0\1\0\2\0\0\0\0\377\377\0\3\2\6SinOsc\2"
                 #"\0\2\0\1\0\0\377\377\0\1\377\377\0\2\2\fBinaryOpUGen\2\0\2\0\1\0\2"
                 #"\0\2\0\0\377\377\0\3\2\3Out\2\0\3\0\0\0\0\377\377\0\0\0\3\0\0\0\1\0\0"))
  
  )
