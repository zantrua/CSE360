#lang racket

(require racket/gui)

(provide make-save-panel)

(define (make-save-panel save-file-path master-panel handle-event)
  (let* ([panel (new vertical-panel% [parent master-panel])]
         [name (new text-field% [parent panel] [label "Save name"])]
         [btn (new button% [parent panel][label "Save"][callback (λ (event) (void))])])
    panel))