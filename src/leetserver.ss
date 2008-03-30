(module leetserver mzscheme
		(require (lib "list.ss"))

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
	(let accept-loop ()
	  (let-values ([(in out) (tcp-accept listener)])
				  (thread (lambda () (thread-handler in out)))
				  (accept-loop)))))
				  
(define (start port)
  (thread (lambda () (server-for-port port))))

(provide start)
















)