; call to start the interpreter
(define (plan program)
	(evalExpr (cadr program) '())
)


; env helpers
(define (extend-env id val env)
	(cons (cons id val) env)
)

(define (lookup id env)
	(cond
		((null? env) 0)
		((equal? id (car (car env))) (cdr (car env)))
		(else (lookup id (cdr env)))
	)
)


; function representation
(define (make-fun param body)
	(list 'function param body)
)

(define (fun-param fn)
	(cadr fn)
)

(define (fun-body fn)
	(caddr fn)
)


; main evaluator
(define (evalExpr expr env)
	(cond
		((integer? expr) expr)
		((symbol? expr) (lookup expr env))

		((and (list? expr) (equal? (car expr) 'planIf))
			(evalIf expr env))
		((and (list? expr) (equal? (car expr) 'planAdd))
			(evalAdd expr env))
		((and (list? expr) (equal? (car expr) 'planMul))
			(evalMul expr env))
		((and (list? expr) (equal? (car expr) 'planSub))
			(evalSub expr env))
		((and (list? expr) (equal? (car expr) 'planLet))
			(evalLet expr env))
		((and (list? expr) (equal? (car expr) 'planFunction))
			(make-fun (cadr expr) (caddr expr)))
		((and (list? expr) (symbol? (car expr)))
			(evalApp expr env))
	)
)



(define (evalIf expr env)
	(let ((condVal (evalExpr (cadr expr) env)))
		(if (> condVal 0)
			(evalExpr (caddr expr) env)
			(evalExpr (cadddr expr) env)
		)
	)
)

(define (evalAdd expr env)
	(+ (evalExpr (cadr expr) env)
		(evalExpr (caddr expr) env)
	)
)

(define (evalMul expr env)
	(* (evalExpr (cadr expr) env)
		(evalExpr (caddr expr) env)
	)
)

(define (evalSub expr env)
	(- (evalExpr (cadr expr) env)
		(evalExpr (caddr expr) env)
	)
)

(define (evalLet expr env)
	(let ((id   (cadr expr))
			(rhs  (caddr expr))
			(body (cadddr expr)))
		(cond
			((and (list? rhs) (equal? (car rhs) 'planFunction))
				(let ((fun-val (make-fun (cadr rhs) (caddr rhs))))
					(evalExpr body (extend-env id fun-val env))
				))
			(else
				(let ((val (evalExpr rhs env)))
					(evalExpr body (extend-env id val env))
				)
			)
		)
	)
)

(define (evalApp expr env)
	(let ((fname    (car expr))
			(arg-expr (cadr expr)))
	(let ((fn (lookup fname env)))
		(let ((param   (fun-param fn))
				(body    (fun-body fn))
				(arg-val (evalExpr arg-expr env)))
		(evalExpr body (extend-env param arg-val env)))))
)



