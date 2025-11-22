; call to start the interpreter
(define (plan program)
	(evalExpr (cadr program))
)

(define (evalExpr expr)
	(cond
		((integer? expr) expr)
		((equal? (car expr) 'planAdd) (evalAdd expr))
	)
)

(define (evalAdd expr)
	(+
		(evalExpr (cadr expr))
		(evalExpr (caddr expr))
	)
)