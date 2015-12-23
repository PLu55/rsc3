#lang racket

(require rnrs
         ;rhs/rhs
         rsc3
         sosc/sosc
         (prefix-in srfi: srfi/1)
         "graphdef.rkt")

(provide  (all-defined-out))

(define quantize
  (lambda (q n)
    (* (round (/ n q)) q)))

(define disassemble-controls
  (lambda (g)
    (map (lambda (control default)
           (list (string->symbol (control-name control))
                 (quantize 0.001 default)))
         (graphdef-controls g)
         (graphdef-defaults g))))

(define control-ugen?
  (lambda (u)
    (elem (ugen-name u)
          (list "Control" "LagControl" "TrigControl"))))

(define root-ugens
  (lambda (g)
    (let* ((ugens (graphdef-ugens g))
           (number-of-ugens (length ugens))
           (ugens-used-as-inputs
            (nub
             (flatten (map (lambda (u i)
                             (if (control-ugen? u)
                                 (list i)
                                 (map (lambda (input)
                                        (if (eq? (input-port input) -1)
                                            '()
                                            (input-ugen input)))
                                      (ugen-inputs u))))
                           ugens (srfi:iota number-of-ugens))))))
      (filter (lambda (e) (not (member e ugens-used-as-inputs)))
              (srfi:iota number-of-ugens)))))

(define root-ugen
  (lambda (g)
    (let ((r (root-ugens g)))
      (if (not (= (length r) 1))
          (error "root-ugen" "multiple root ugens not supported")
          (car r)))))

(define determine-control-index
  (lambda (g u o)
    (if (= u 0) o (error "multiple rate controls not supported" u o))))

(define pass-one
  (lambda (g n controls)
    (let* ((ugen (graphdef-ugen g n))
           (rate (rate->symbol (make-rate (ugen-rate ugen))))
           (inputs (ugen-inputs ugen))
           (outputs (ugen-outputs ugen))
           (special (ugen-special ugen))
           (name* (ugen-name ugen))
           ;(name (string->symbol (or2 (operator-name ugen) (scheme-name name*))))
           (name (string->symbol (or (operator-name ugen) (scheme-name name*))))
           (user-inputs
            (map
             (lambda (input)
               (let ((ugen-index (input-ugen input)))
                 (if (= ugen-index -1)
                     (quantize 0.001 (graphdef-constant g (input-port input)))
                     (if (control-ugen? (graphdef-ugen g ugen-index))
                         (car (list-ref
                               controls
                               (determine-control-index
                                g
                                ugen-index
                                (input-port input))))
                         (pass-one g ugen-index controls)))))
             inputs)))
      (cons name (cons rate user-inputs)))))

(define graphdef-disassemble
  (lambda (g)
    (let ((controls (disassemble-controls g)))
      (list 'synthdef
            (graphdef-name g)
            (list 'letc
                  controls
                  (pass-one g (root-ugen g) controls))))))

(define (rate->symbol rate)
  (case rate
    ((0) 'ir)
    ((1) 'kr)
    ((2) 'ar)
    ((3) 'dr)
    (else 'unknown)))

(define (operator-name ugen)
  (printf "operator-name ~a~n" ugen)
  'a)

(define (scheme-name ugen)
  (printf "scheme-name ~a~n" ugen)
  'b)
