#lang racket

(require rnrs rhs/rhs sosc/bytevector)

(provide (all-defined-out))
         
;; port -> int -> bytevector
(define read-bstr
  (lambda (p n)
    (get-bytevector-n p n)))

;; port -> string
(define read-pstr
  (lambda (p)
    (let* ((n (lookahead-u8 p))
	   (v (read-bstr p (+ n 1))))
      (decode-pstr v))))

;; port -> string
(define read-cstr
  (lambda (p)
    (let loop ((l nil)
	       (b (get-u8 p)))
      (if (= b 0)
	  (list->string (map1 integer->char (reverse l)))
	  (loop (cons b l) (get-u8 p))))))

;; bytevector -> int
(define decode-u8
  (lambda (v)
    (bytevector-u8-ref v 0)))

;; bytevector -> int
(define decode-u16
  (lambda (v)
    (bytevector-u16-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-u32
  (lambda (v)
    (bytevector-u32-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-u64
  (lambda (v)
    (bytevector-u64-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-i8
  (lambda (v)
    (bytevector-s8-ref v 0)))

;; bytevector -> int
(define decode-i16
  (lambda (v)
    (bytevector-s16-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-i32
  (lambda (v)
    (bytevector-s32-ref v 0 (endianness big))))

;; bytevector -> int
(define decode-i64
  (lambda (v)
    (bytevector-s64-ref v 0 (endianness big))))

;; bytevector -> double
(define decode-f32
  (lambda (v)
    (bytevector-ieee-single-ref v 0 (endianness big))))

;; bytevector -> double
(define decode-f64
  (lambda (v)
    (bytevector-ieee-double-ref v 0 (endianness big))))

;; bytevector -> string
(define decode-str
  (lambda (b)
    (utf8->string b)))

;; bytevector -> string
(define decode-pstr
  (lambda (v)
    (let* ((n (decode-u8 v))
	   (w (bytevector-section v 1 (+ n 1))))
      (decode-str w))))

;; bytevector -> string
(define decode-cstr
  (lambda (v)
    (let* ((n (bytevector-find-index v 0))
	   (w (bytevector-section v 0 n)))
      (decode-str w))))

