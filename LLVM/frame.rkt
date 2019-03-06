#lang racket

(require "types.rkt"
         "names.rkt")

(provide (all-defined-out))

; we're assuming a 64-bit machine, this would change depending on the
; target architecture and you'd probably do some fancy module thing
(define WORD_SIZE 8)

; the min frame size is how much we need to push the Rarp each time we
; have a function call, we need room to store:
; * the static link (if needed)
; * the the return address
; * the return value

(define MIN_FRAME_SIZE
  (+ WORD_SIZE ; for the return address
     WORD_SIZE ; for the static link
     WORD_SIZE)) ; for the return value
     

; a frame consists of its name, which is a label (labels are addresses)
; a static-link, which is also a label, an offset, which just counts how
; many locals and escaping variables have been seen, and finally, a
; reference to the funval associated with this Frame (because we'll need
; parameter info). local-space is the number of bytes allocated for the locals
(struct Frame (name
               [static-link #:mutable]
               [size #:mutable]           ; total size of the frame in bytes
               [locals #:mutable]    ; total size of the locals allocated in bytes
               [funval #:mutable])
  #:transparent
  #:guard (λ (name static-link size locals funval tyname)
            (cond
              [(not (Label? name)) (raise-argument-error tyname "Expectd a Label? for its name field" name)]
              [(not (or (eq? static-link #f) (Label? static-link))) (raise-argument-error tyname "Expected a Label? for its static-link field" static-link)]
              [(not (exact-nonnegative-integer? size))
               (raise-argument-error tyname "Expected an exact-positive-integer? for its size field" size)]
              [(not (exact-nonnegative-integer? locals))
               (raise-argument-error tyname "Expected an exact-positive-integer? for its locals field" locals)]
              [(not (or (FunValue? funval) (eq? funval #f)))
               (raise-argument-error tyname "Expected a FunValue? or #f for its funval field" funval)]
              [else (values name static-link size locals funval)])))

            

; build a frame, now with less typing:
; this allocates enough space for:
;    - the static link, which is a pointer
;    - the return address, if we were called as a function
;    - a return value, if needed (in theory this could be reduced by 8 bytes)
(define (make-frame name)
  (Frame name #f MIN_FRAME_SIZE 0 #f))

; create a frame based off info from the funvalue
(define (make-frame-from-funvalue funval)
  (let ([frame (make-frame (make-label))])
    ; set up references to and from the frame
    (set-FunValue-frame! funval frame)
    (set-Frame-funval! frame funval)
    frame))
     
; well, by default, all types are 1 word size, since we have ints and pointers,
; but in theory, you'd modify this by the size of the actual type (like a float
; would be 4 bytes, a character fewer possibly
(define (get-type-size ty)
  WORD_SIZE)

; a function to call that allocates parameter space for the frame
(define (alloc-parameter! frame ty)
  (let ([bytes (get-type-size ty)]
        [current-offset (Frame-size frame)])
    (set-Frame-size! frame (+ current-offset bytes))
    ; return our current offset (since we typically use this for binding)
    current-offset))

; a function to call that allocates a local variable on the frame
(define (alloc-local! frame ty)
  ; get the number of bytes it takes up
  (let ([bytes (get-type-size ty)]
        [current-offset (Frame-size frame)]
        [current-locals (Frame-locals frame)])
    (set-Frame-size! frame (+ current-offset bytes))
    (set-Frame-locals! frame (+ current-locals bytes)) ;; Should this be set-Frame-locals! ???

    ; and then return the current-offset (not the new one)
    ; so it can be passed back and stored in the VarValue
    ; itself as needed
    current-offset))

; returns the precall frame size, which is the parameters, plus static link, plus stuff like that
(define (precall-frame-size frame)
  (- (Frame-size frame) (Frame-locals frame)))

; prints out a frame
(define (output-frame out frame)
  (fprintf out "Frame: ~a, static-link: ~a, allocated-bytes: ~a, space for locals (bytes): ~afunction: ~a~n"
           (Label-name (Frame-name frame)) (Label-name (Frame-static-link frame))
           (Frame-size frame) (Frame-locals frame) (FunValue-name (Frame-funval frame))))