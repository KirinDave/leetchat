(module leetserver mzscheme
		(require (lib "list.ss") (lib "string.ss"))

(define (close-connection in out)
  (tcp-close in)
  (tcp-close out)
  'closed)

(define (echo line out)
  (printf "(~s) Got line: ~s~n" (current-thread) line)
  (fprintf out line)
  (flush-output out))

(define (thread-handler inport outport)
  (let ((inline (read-line inport)))
	(cond ((eof-object? inline) (display "Port closed!") 'done)
		  (else (echo inline outport) (thread-handler inport outport)))))

(define (server-for-port port)
  (let ([listener (tcp-listen port)])
	(display "Going to accept loop...\n")
	(let accept-loop ()
	  (let-values ([(in out) (tcp-accept listener)])
				  (display "tcp-accept\n")
				  (thread (lambda () 
							(display "Opening new connection!\n")
							(if (authenticated? in out)
								(thread-handler in out)
								(close-connection in out))))
				  (accept-loop)))))
				  
(define (start port)
  (thread (lambda () (server-for-port port))))

; Lousy password system
(define users (list))
(define (add-user n p) (set! users (cons (cons n p) users)))
(define (check-user n p)
  (equal? (findf (lambda (x) (equal? n (car x))) users)
		  (cons n p)))

; Lousy authentication
(define (authenticated? in out) 
  (display "Trying to authenticate...\n")
  (fprintf out "AUTHENTICATE:")
  (flush-output out)
  (let ([aline (regexp-split " " (read-line in))])
	(if (= (length aline) 2)
		(check-user (first aline) (second aline))
		#f)))


(add-user "dave" "f")
(provide start add-user)


; Line handlers
(define (match-line-spec pattern pliterals split-regex data)
  (descending-match pattern 
                    pliterals
                    (regexp-split split-regex data) 
                    (lambda (x) (string->bytes/utf-8 (symbol->string x)))
                    (list)))

(define (descending-match pattern pliterals items symbol->val accum)
  (let ((fail #f))
    (cond 
     ((and (null? pattern) (null? items)) accum) ; Finished pattern match, succed
     ((symbol? pattern) (cons (list pattern items) accum)) ; wildcard, succed. 
     ((or (null? pattern) (null? items)) fail) ; Incomplete match, fail
     (else ; Descend into an actual match
      (let ([p (car pattern)]
            [pv (symbol->val (car pattern))]
            [d (car items)])
        (if (member p pliterals) (if (equal? pv d)
                                     (descending-match (cdr pattern) pliterals (cdr items) symbol->val accum)
                                     fail)
            (descending-match (cdr pattern) pliterals (cdr items) symbol->val (cons (list p d) accum))))))))

  
) ; End module