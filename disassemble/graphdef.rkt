#lang racket

(require rnrs
         rsc3
         sosc/sosc
         rhs/rhs
         (prefix-in srfi: srfi/1))

;; Decode a binary 'Graph Definition'.  File containing more than one
;; graph definition are not supported.

(define read-control
  (lambda (p)
    (let* ((nm (read-pstr p))
           (ix (read-i16 p)))
      (make-control nm ix))))

(define read-input
  (lambda (p)
    (let* ((u (read-i16 p))
           (p (read-i16 p)))
      (make-input u p))))

(define read-output
  (lambda (p)
    (make-output
     (read-i8 p))))

(define read-ugen
  (lambda (k p)
    (let* ((name (read-pstr p))
           (rate (read-i8 p))
           (number-of-inputs (read-i16 p))
           (number-of-outputs (read-i16 p))
           (special (read-i16 p))
           (inputs (map (lambda (i) (read-input p)) (srfi:iota number-of-inputs)))
           (outputs (map (lambda (i) (read-output p)) (srfi:iota number-of-outputs))))
      (make-ugen name
                 rate
                 inputs
                 outputs
                 special
                 k))))
(define read-list
  (lambda (n f p)
    (map (lambda (_) (f p)) (srfi:iota n))))

(define read-list-k
  (lambda (n f p)
    (map (lambda (k) (f k p)) (srfi:iota n))))

(define read-graphdef
  (lambda (p)
    (let* ((magic (read-bstr p 4))
           (version (read-i32 p))
           (number-of-definitions (read-i16 p)))
      (if (not (bytevector=? magic (string->utf8 "SCgf")))
          (error "read-graphdef: illegal magic string" magic))
      (if (not (= version 0))
          (error "read-graphdef: version not at zero" version))
      (if (not (= number-of-definitions 1))
          (error "read-graphdef: non unary graphdef file" number-of-definitions))
      (let* ((name (read-pstr p))
             (number-of-constants (read-i16 p))
             (constants (read-list number-of-constants
                                   read-f32
                                   p))
             (number-of-control-defaults (read-i16 p))
             (control-defaults (read-list
                                number-of-control-defaults
                                read-f32
                                p))
             (number-of-controls (read-i16 p))
             (controls (read-list number-of-controls
                                  read-control
                                  p))
             (number-of-ugens (read-i16 p))
             (ugens (read-list-k number-of-ugens
                                 read-ugen
                                 p)))
        (make-graphdef name
                       constants
                       control-defaults
                       controls
                       ugens)))))

(define read-graphdef-file
  (lambda (nm)
    (let* ((p (open-file-input-port nm))
           (g (read-graphdef p)))
      (close-port p)
      g)))

(define trace
  (lambda (msg x)
    (display msg)
    (display x)
    (newline)
    x))
