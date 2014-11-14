#lang racket

(provide 
 make-transition
 make-state-machine state-machine-get-state state-machine-get-next)

; Transitions

(define (make-transition start event end)
  (list start event end))

(define (transition-get-start trans)
  (first trans))

(define (transition-get-end trans)
  (third trans))

(define (transition-get-event trans)
  (second trans))

; State machines

(define (make-state-machine transitions)
  (let ([start-states (map transition-get-start transitions)])
    (make-state-machine-state (car start-states) transitions)))

(define (make-state-machine-state state transitions)
  (list state transitions))

(define (state-machine-get-state machine)
  (first machine))

(define (state-machine-get-transitions machine)
  (second machine))

(define (state-machine-get-next machine event)
  (let ([trans-list (filter (λ (x) (and (eq? (state-machine-get-state machine) (transition-get-start x))
                                             (eq? event (transition-get-event x))))
                            (state-machine-get-transitions machine))])
    (make-state-machine-state (transition-get-end (first trans-list)) (state-machine-get-transitions machine))))
