#lang racket

(require racket/fixnum
         rsc3
         sosc/sosc
         sosc/bytevector
         (prefix-in srfi: srfi/1)
         "graphdef.rkt")

(provide  (all-defined-out))


(define quantize
  (lambda (q n)
    (* (round (/ n q)) q)))

(define disassemble-controls
  (lambda (g)
    (map (lambda (l) (take l 2))
         (sort    
          (map (lambda (control default)
                 (list (string->symbol (control-name control))
                       (quantize 0.00001 default)
                       (control-index control)))
               (graphdef-controls g)
               (graphdef-defaults g))
          (lambda (a b) (< (caddr a)(caddr b)))))))
         
(define control-ugen?
  (lambda (u)
    (member (ugen-name u)
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

;(define (control-find controls idx)
  
(define pass-one
  (lambda (g n)
    (let* ((ugen (graphdef-ugen g n))
           (rate (rate->symbol (rate (ugen-rate ugen))))
           (controls (sort (graphdef-controls g)
                           (lambda (a b)
                             (< (control-index a)(control-index b)))))
           (inputs (ugen-inputs ugen))
           (outputs (ugen-outputs ugen))
           (special (ugen-special ugen))
           (symbol (ugen-symbol ugen))
           (parameters
            (map
             (lambda (input)
               (let ((ugen-index (input-ugen input)))
                 (if (= ugen-index -1)
                     (quantize 0.001 (graphdef-constant g (input-port input)))
                     (if (control-ugen? (graphdef-ugen g ugen-index))
                         (string->symbol
                          (control-name (list-ref controls  (input-port input))))
                         (pass-one g ugen-index)))))
             inputs)))
      (cons symbol (cons rate parameters)))))

;;; disassemble-controls discard index information
(define graphdef-disassemble
  (lambda (g)
    (list 'synthdef
          (graphdef-name g)
          (list 'letc
                (disassemble-controls g)
                (pass-one g (root-ugen g))))))

(define (rate->symbol rate)
  (case (rate-value rate)
    ((0) 'ir)
    ((1) 'kr)
    ((2) 'ar)
    ((3) 'dr)
    (else 'unknown-rate)))

(define (ugen-symbol ugen)
  (cond [(equal? (ugen-name ugen) "UnaryOpUGen")
         (unary-op-code->name (ugen-special ugen))]
        [(equal? (ugen-name ugen) "BinaryOpUGen")
         (binary-op-code->name (ugen-special ugen))]
        [else
         (ugen-name->symbol (ugen-name ugen))]))

#|
(graphdef-disassemble (decode-graphdef (encode-graphdef (synthdef "sine" (mul (sin-osc ar 440 0) 0.1)))))

(define g (decode-graphdef (encode-graphdef (synthdef "sine" (mul (sin-osc ar 440 0) 0.1)))))
(disassemble-controls g)
(graphdef-name g)
(root-ugen g)
(pass-one g (root-ugen g) (disassemble-controls g))

(define ugen (graphdef-ugen g (root-ugen g)))
ugen
(ugen-inputs ugen)
(ugen-name ugen)
(string->symbol (or (operator-name ugen) (scheme-name name*)))

(define filt-noise
  (letc ((ampl 0.1)
         (freq 1500.0)
         (bw 0.1)
         (lag-time 0.02)
         (outbus 0))
        (let* ([n (mul (white-noise ar) (lag ampl lag-time))]
               [res (resonz n (lag freq lag-time) (lag bw lag-time))])
          (out outbus res))))

(decode-graphdef (encode-graphdef (synthdef "filt-noise" filt-noise)))
(graphdef-disassemble (decode-graphdef (encode-graphdef (synthdef "filt-noise" filt-noise))))

See:
http://doc.sccode.org/Reference/Synth-Definition-File-Format.html
file://usr/share/SuperCollider/SCClassLibrary/Common/Audio/SynthDef.sc


|#

