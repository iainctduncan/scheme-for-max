;;; **********************************************************************
;;; Copyright (C) 2008, 2009 Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;
;;; Tokens
;

(define (first-token toks) (car toks))
(define (rest-tokens toks) (cdr toks))
(define (null-tokens? toks) (null? toks))

;
;;; Rules
;

(define *rules* (make-eq-hash-table))

(define-record rule name type pattern parser emiter)

;;(define-record-printer (rule r p)
;;  (fprintf p "#<r ~A>" (rule-name r)))

(define-macro (defrule typ pat . args)
  (letrec ((ppat (lambda (p)
		   (cond ((null? p) '(list))
			 ((pair? p)
			  `(cons ,(ppat (car p))
				 ,(ppat (cdr p))))
			 ((member p '(+ * @ and or))
			  `(quote ,p))
			 (else p)))))
    `(let ((ruleid ,typ))
       (hash-set! *rules* ruleid
		  (make-rule ,(symbol->string typ)
			     ruleid 
			     ,(ppat pat)
			     ,(if (null? args)
				  #f 
				  (car args))
			     ,(if (or (null? args)
				      (null? (cdr args)))
				  #f
				  (cadr args)))))))

(define (get-rule typ . def) 
  (or (hash-ref *rules* typ)
      (error "~S is not a rule" typ)))

;
;;; patterns
;

(define (pattern-value? x) (number? x))
(define (pattern-value x) x)
(define (pattern-clause? x) (pair? x))
(define (pattern-clause x) (car x))
(define (pattern-subs x) (cdr x))
(define (pattern-nsubs x) (length (pattern-subs x)))
(define (pattern-sub x i) (list-ref (pattern-subs x) i))
(define (pattern-and? x) (and (pattern-clause? x) (eq? (car x) 'and)))
(define (pattern-or? x) (and (pattern-clause? x) (eq? (car x) 'or)))
(define (pattern-one-or-more? x) (and (pattern-clause? x) (eq? (car x) '+)))
(define (pattern-zero-or-more? x) (and (pattern-clause? x) (eq? (car x) '*)))
(define (pattern-optional? x) (and (pattern-clause? x) (eq? (car x) '@)))

;;
;;; parse errors
;;

(define-record parse-error string position)
;;(define-record-printer (parse-error e p)
;;  (fprintf p "#<parse-error ~S (@ ~S)>"
;;	   (parse-error-string e)
;;	   (parse-error-position e)))

;
;; parse units
;

(define-record parse-unit type parsed position)

;;(define-record-printer (parse-unit u p)
;;  (let ((typ (parse-unit-type u)))
;;    (if (SalRuleType? typ)
;;	(fprintf p "#<u ~A>" (rule-name (get-rule typ)))
;;	(fprintf p "#<u x~X>" typ))))

(define (token-unit? x)
  (and (parse-unit? x)
       (SalTokenType? (parse-unit-type x))))

(define (token-unit-type=? x typ) 
  (and (token-unit? x)
       (SalType=? (parse-unit-type x) typ)))

(define (rule-unit? x)
  (and (parse-unit? x)
       (SalRuleType? (parse-unit-type x))))

;; emit methods

;; emit methods can access/pass information up/down the emit subcall
;; chain by adding things to the info parameter.

(define (get-emit-info name info)
  (let ((x (assoc name info)))
    (and x (cdr x))))

(define (add-emit-info name data info)
  (cons (cons name data) info))

(define (rem-emit-info name info)
  (let* ((head (list #f))
	 (tail head))
    (do ((data info (cdr data)))
	((null? data) (cdr head))
      (if (not (eq? (caar data) name))
	  (begin 
	    (set-cdr! tail (list (car data)))
	    (set! tail (cdr tail)))))))

(define (set-emit-info! name data info)
  ;; nconc data to end of info. this allows subforms to pass info back
  ;; up to calling forms.
  (let ((i (assoc name info)))
    (if (not i) (error "no info for ~S" name))
    (set-cdr! i data)))

(define (emit x info errf)
  (cond ((token-unit? x)
	 (emit-token-unit x info errf))
	((rule-unit? x)
	 (emit-rule-unit x info errf))
	;; check list? last since tokens and units are lists...
	((list? x)
	 (emit-list x info errf))
	(else
	 (error "don't know how to emit ~S" x))))

(define (emit-list lis info errf)
  (if (null? lis)
      (list)
      (cons (emit (car lis) info errf)
	    (emit-list (cdr lis) info errf))))

; FIX!

(define (!= . nums) (not (apply = nums)))

(define (emit-token-unit unit info errf)
  ;; token units have the string in their parsed slot
  (let ((typ (parse-unit-type unit))
	(str (parse-unit-parsed unit))
	)
    (cond ((SalNumberType? typ)
	   (string->number str))
	  ((SalLiteralType? typ)
	   (string->symbol str))
	  ((SalOpType? typ)
	   (cond ((SalType=? typ SalMod) 'modulo)
		 ((SalType=? typ SalExpt) 'expt)
		 ((SalType=? typ SalGeneralEqual) 'equal?)
		 ((SalType=? typ SalAnd) 'and)
		 ((SalType=? typ SalOr) 'or)
		 ((SalType=? typ SalNot) 'not)
		 (else
		  (string->symbol str))))
	  ((SalBoolType? typ)
	   (if (SalType=? typ SalTrue) #t #f))
	  ((SalType=? typ SalString )
	   str)
	  ((SalType=? typ SalKeyparam )
	   (string->keyword str))
	  ((SalType=? typ SalKeyword )
	   (string->keyword str))
	  ((SalType=? typ SalClass)
	   (string->symbol str))
	  ((SalType=? typ SalSlotRef )
	   (error "emit-token-unit: dont know how to emit slotref token ~S"
		  unit))
	  ((SalType=? typ SalIdentifier )
	   (string->symbol str) )
	  (else
	   (error "emit-token-unit: dont know how to emit ~S" 
		  unit)
	   ))))	

(define (emit-rule-unit unit info errf)
  ;; look up the rule and apply its emit method
  ;; if there is no emit method call emit on the parsed units
  (let ((rule (get-rule (parse-unit-type unit))))
    (let ((meth (rule-emiter rule)))
      (if (not meth)
	  (emit (parse-unit-parsed unit) info errf)
	  ( meth unit info errf)))))

(define (simple-unit-parser type)
  ;; return a parse unit that simply holds its subforms
  (lambda (args errf)
    (make-parse-unit type args #f)))

(define (simple-unit-emitter func)
  ;; return an emitter that emits a lisp form for each subunit with
  ;; the name of the statement optionally replaced by func name
  (lambda (unit info errf)
    ;; create an identical list and then modify
    (let* ((data (append (parse-unit-parsed unit) (list)))
	   (head data))	
      ;; replace sal statement name with function name
      (if func 
	  (begin (set-car! data func)
		 (set! data (cdr data))))
      ;; replace each unit by its emitted lisp expression
      (do ((tail data (cdr tail)))
	  ((null? tail) head)
	(set-car! tail (emit (car tail) info errf))))))

;
;;; the parser
;

(define *maxtokpos* #f)

(define (parse pattern tokens . args)
  ;; returns a lisp expression or a parse-error
  (let ((junk-allowed #f)
	(trace #f)
	(info (list)))
    (call/cc
     (lambda (errf)
       (call-with-values 
	   (lambda ()
	     (set! *maxtokpos* 0)
	     (parser pattern tokens #f 0 trace errf))
	 (lambda (success results remains) 
	   (if (and success (or (null? remains) junk-allowed)) 
	       (emit results
		     ;; top-level info is syntax to emit
		     (add-emit-info #:syntax #:scheme info)
		     errf)
	       (make-parse-error "illegal statement: "
				 *maxtokpos*))))))))

(define (parser pat tokens force-and level trace errf)
  (if trace (format #t "~%parse[~A]: pattern=~S tokens=~S" level pat tokens))
  (or (null-tokens? tokens)
      (set! *maxtokpos* (max *maxtokpos* (ffi_sal_token_position (first-token tokens)))))
  (cond	((pattern-value? pat)
	 ;; pattern value: is either a TokenType or RuleType
	 (let ((typ (pattern-value pat)))
	   (cond ((SalTokenType? typ)
		  (if (null-tokens? tokens) 
		      (values #f #f (list))
		      (let ((tok (first-token tokens)))
			(if (SalType=? (ffi_sal_token_type tok) typ)
			    ;; token = patten, return token unit
			    (values #t
				    (make-parse-unit (ffi_sal_token_type tok)
						     (ffi_sal_token_string tok)
						     (ffi_sal_token_position tok))
				    (rest-tokens tokens))
			    (values #f #f tokens)))))
		 ((SalRuleType? typ)
		  (let* ((rule (get-rule typ))
			 (test (rule-pattern rule))
			 (func (rule-parser rule)
			       ))
		    (call-with-values
			(lambda () (parser test tokens #f 
					   (+ level 1) trace errf) )
		      (lambda (a b c) 
			(if a
			    (values a 
				    (if func ( func b errf) b)
				    c)
			    (values #f #f c))))))
		 (else
		  (error "~S is not a token type or rule type" typ))
		 )))
	((pattern-clause? pat)
	 (cond ((or (pattern-and? pat) force-and)
		(let ((p #f)
		      (l tokens)
		      (r (list))
		      (s #f)
		      (n (pattern-nsubs pat))
		      (f #f)) ; fail flag
		  (do ((i 0 (+ i 1)))
		      ((or (= i n) f) #f)
		    (call-with-values 
			(lambda () 
			  (parser (pattern-sub pat i) l #f 
				  (+ level 1) trace errf))
		      (lambda (a b c) (set! p a) (set! s b) (set! l c)))
		    (if (not p)
			(set! f #t) ; subpattern failed
			(set! r (cons s r)))) ; else collect result
		  ;; if failed return tokens as they were else return
		  ;; only the tokens that remain after the parse
		  (if f
		      (begin (set! p #f)
			     (set! r (list))
			     (set! l tokens))
		      (begin (set! p #t)
			     (set! r (reverse r)) ))
		  (values p r l)
		  ))
	       ((pattern-or? pat)
		;; 'or' finds first true match. terms are independant
		;; and are given all tokens passed in
		(let ((p #f)
		      (s #f)
		      (l (list))
		      (n (pattern-nsubs pat)))
		  (do ((i 0 (+ i 1)) )
		      ((or p (= i n)) #f)
		    (call-with-values
			(lambda () 
			  (parser (pattern-sub pat i) tokens #f
				  (+ level 1) trace errf))
		      (lambda (a b c) (set! p a) (set! s b) (set! l c))))
		  (if p (values p s l )
		      (values #f (list) tokens))
		  ))
	       ((or (pattern-zero-or-more? pat)
		    (pattern-one-or-more? pat))
		;; '*' and '+' consume tokens while true, where entire
		;; clause must match to be true. * is zero or more, +
		;; is 1 or more
		(let ((p #t)
		      (s #f)
		      (l tokens)
		      (r (list))
		      (n 0))
		  ;; iterativly match pattern against tokens, stop on
		  ;; first fail or end of tokens
		  (do ()
		      ((or (not p) (null-tokens? l)) #f)
		    ;; call parser again with whole pattern forcing
		    ;; 'and' check
		    (call-with-values
			(lambda ()
			  (parser pat l #t level trace errf))
		      (lambda (a b c) (set! p a) (set! s b) (set! l c)))
		    ;; if we had a match update tokens and save
		    ;; results
		    (when p
	              (set! r (append r s))
		      (set! tokens l)
		      (set! n (+ n 1)))
		    )
		  (if (or (pattern-zero-or-more? pat)
			  (> n 0))
		      (set! p #t)
		      (begin (set! p #f) (set! r (list))))
		  (values p r tokens)))
	       ((pattern-optional? pat)
		(let ((p #f)
		      (s (list))
		      (l (list)))
		  ;; recall parser forcing 'and' test of whole pattern
		(call-with-values
		    (lambda ()
		      ;; dont bump level
		      (parser pat tokens #t level trace
			      errf))
		  (lambda (a b c) (set! p a) (set! s b) (set! l c)))
		;; ... if successful then return true with remaining
		;; tokens otherwise return true with tokens passed in
		(if p (values p s l)
		    (values #t #f tokens))))
	       (else
		(error "pattern operand ~S not one of and, or, *, +, @"
		       (pattern-clause pat))))
	 )
	(else
	 (error "~S is not a pattern value or pattern operator" pat))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAL language definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin
  ;; generated by (sal-enums)
  (define SalUntyped #x0)
  (define SAL_TOKEN_BEG #x0)
  (define SAL_DELIM_BEG #x0)
  (define SalComma #x100)
  (define SalLParen #x200)
  (define SalRParen #x300)
  (define SalLCurly #x400)
  (define SalRCurly #x500)
  (define SalLBrace #x600)
  (define SalRBrace #x700)
  (define SAL_DELIM_END #x800)
  (define SAL_DATA_BEG #x800)
  (define SalString #x900)
  (define SAL_NUMBER_BEG #xa00)
  (define SalInteger #xb00)
  (define SalRatio #xc00)
  (define SalFloat #xd00)
  (define SAL_NUMBER_END #xe00)
  (define SalKeyword #xf00)
  (define SAL_BOOL_BEG #x1000)
  (define SAL_HASH_BEG #x1000)
  (define SalTrue #x1100)
  (define SalFalse #x1200)
  (define SAL_BOOL_END #x1300)
  (define SalQMark #x1400)
  (define SalUnquote #x1500)
  (define SalSplice #x1600)
  (define SAL_HASH_END #x1700)
  (define SAL_DATA_END #x1700)
  (define SalKeyparam #x1800)
  (define SalClass #x1900)
  (define SAL_OP_BEG #x1a00)
  (define SalPlus #x1b05)
  (define SalMinus #x1c05)
  (define SalTimes #x1d06)
  (define SalDivide #x1e06)
  (define SalMod #x1f05)
  (define SalExpt #x2007)
  (define SalAnd #x2102)
  (define SalOr #x2201)
  (define SalNot #x2303)
  (define SAL_RELATION_BEG #x2400)
  (define SalLess #x2504)
  (define SalGreater #x2604)
  (define SalNotEqual #x2704)
  (define SalGeneralEqual #x2804)
  (define SAL_ASSIGNMENT_BEG #x2900)
  (define SalEqual #x2a04)
  (define SalLessEqual #x2b04)
  (define SalGreaterEqual #x2c04)
  (define SAL_RELATION_END #x2d00)
  (define SAL_OP_END #x2d00)
  (define SalInc #x2e00)
  (define SalMul #x2f00)
  (define SalCol #x3000)
  (define SalPre #x3100)
  (define SalApp #x3200)
  (define SAL_ASSIGNMENT_END #x3300)
  (define SAL_LITERAL_BEG #x3300)
  (define SAL_STATEMENT_BEG #x3300)
  (define SAL_COMMAND_BEG #x3300)
  (define SalBegin #x3410)
  (define SalChdir #x3500)
  (define SalDefine #x3600)
  (define SalExec #x3700)
  (define SalIf #x3800)
  (define SalLoad #x3900)
  (define SalLoop #x3a10)
  (define SalPlot #x3b00)
  (define SalPrint #x3c00)
  (define SalRun #x3d10)
  (define SalSend #x3e00)
  (define SalSet #x3f00)
  (define SalSoundFile #x4010)
  (define SalFomusFile #x4110)
  (define SalSprout #x4200)
  (define Sal2Variable #x4300)
  (define Sal2Function #x4410)
  (define Sal2Process #x4510)
  (define Sal2If #x4610)
  (define Sal2File #x4710)
  (define Sal2Wait #x4800)
  (define SAL_COMMAND_END #x4900)
  (define SAL_CONSTITUENT_BEG #x4900)
  (define SalElse #x4a00)
  (define SalEnd #x4b11)
  (define SalReturn #x4c00)
  (define SalThen #x4d00)
  (define SalUnless #x4e00)
  (define SalUntil #x4f00)
  (define SalWait #x5000)
  (define SalWhen #x5100)
  (define SalWhile #x5200)
  (define SalWith #x5300)
  (define SalOptKey #x5400)
  (define SAL_CONSTITUENT_END #x5500)
  (define SAL_STATEMENT_END #x5500)
  (define SAL_CLAUSAL_BEG #x5500)
  (define SalAbove #x5600)
  (define SalBelow #x5700)
  (define SalBy #x5800)
  (define SalDownto #x5900)
  (define SalFinally #x5a00)
  (define SalFor #x5b00)
  (define SalFrom #x5c00)
  (define SalIn #x5d00)
  (define SalOver #x5e00)
  (define SalRepeat #x5f00)
  (define SalTo #x6000)
  (define SAL_CLAUSAL_END #x6100)
  (define SAL_LITERAL_END #x6100)
  (define SAL_DEFINE_BEG #x6100)
  (define SalFunction #x6200)
  (define SalProcess #x6300)
  (define SalVariable #x6400)
  (define SAL_DEFINE_END #x6500)
  (define SAL_IDENTIFIER_BEG #x6500)
  (define SalIdentifier #x6600)
  (define SalSlotRef #x6700)
  (define SAL_IDENTIFIER_END #x6800)
  (define SAL_TOKEN_END #x6800)
  (define SAL_RULE_BEG #x6800)
  (define SalNumberRule #x6900)
  (define SalBoolRule #x6a00)
  (define SalAtomRule #x6b00)
  (define SalListRule #x6c00)
  (define SalEltRule #x6d00)
  (define SalArefRule #x6e00)
  (define SalIfExprRule #x6f00)
  (define SalUnquoteRule #x7000)
  (define SalFuncallRule #x7100)
  (define SalFunargsRule #x7200)
  (define SalPargsRule #x7300)
  (define SalKargsRule #x7400)
  (define SalOpRule #x7500)
  (define SalMexprRule #x7600)
  (define SalTermRule #x7700)
  (define SalSexprRule #x7800)
  (define SalBindingsRule #x7900)
  (define SalBindRule #x7a00)
  (define SalAssignmentRule #x7b00)
  (define SalAssignRule #x7c00)
  (define SalAssignerRule #x7d00)
  (define SalSetRule #x7e00)
  (define SalFunctionReturnRule #x7f00)
  (define SalProcessWaitRule #x8000)
  (define SalBlockRule #x8100)
  (define SalConditionalRule #x8200)
  (define SalLoopStatementRule #x8300)
  (define SalRunStatementRule #x8400)
  (define SalSteppingRule #x8500)
  (define SalTerminationRule #x8600)
  (define SalPrintStatementRule #x8700)
  (define SalExecStatementRule #x8800)
  (define SalSendStatementRule #x8900)
  (define SalSoundFileStatementRule #x8a00)
  (define SalFomusFileStatementRule #x8b00)
  (define SalSproutStatementRule #x8c00)
  (define SalLoadStatementRule #x8d00)
  (define SalChdirStatementRule #x8e00)
  (define SalPlotStatementRule #x8f00)
  (define SalDefineStatementRule #x9000)
  (define SalStatementRule #x9100)
  (define SalStatementSequenceRule #x9200)
  (define SalDeclarationRule #x9300)
  (define SalVarDeclRule #x9400)
  (define SalFunDeclRule #x9500)
  (define SalProcDeclRule #x9600)
  (define SalProcessBodyRule #x9700)
  (define Sal2FormalsRule #x9800)
  (define Sal2BeginStatementRule #x9900)
  (define Sal2IfStatementRule #x9a00)
  (define Sal2FunctionStatementRule #x9b00)
  (define Sal2VariableStatementRule #x9c00)
  (define Sal2WithStatementRule #x9d00)
  (define Sal2LoopStatementRule #x9e00)
  (define Sal2ProcessStatementRule #x9f00)
  (define Sal2ProcessWaitRule #xa000)
  (define Sal2FileStatementRule #xa100)
  (define Sal2StatementRule #xa200)
  (define Sal2StatementSequenceRule #xa300)
  (define SAL_RULE_END #xa400)
  (define SAL_TYPE_END #xa400)
  )

;;; The parser attempts to match rule patterns against (typed) tokens
;;; in the stream its parsing. the lower eight bits are ignored by the
;;; comparison function and may store info about the typed entity.

(define (SalType? x) (and (<= #x100 x) (< x SAL_TYPE_END)))

(define (SalTokenType? i) 
  (and (< SAL_TOKEN_BEG i) (< i SAL_TOKEN_END)))

(define (SalDelimType? i) 
  (and (< SAL_DELIM_BEG i) (< i SAL_DELIM_END)))

(define (SalNumberType? i) 
  (and (< SAL_NUMBER_BEG i) (< i SAL_NUMBER_END)))

(define (SalBoolType? i) 
  (and (< SAL_BOOL_BEG i) (< i SAL_BOOL_END)))

(define (SalOpType? i) 
  (and (< SAL_OP_BEG i) (< i SAL_OP_END)))

(define (SalOpWeight i) 
  (logand i #xFF))

(define (SalRelationType? i) 
  (and (< SAL_RELATION_BEG i) (< i SAL_RELATION_END)))

(define (SalAssignmentType? i) 
  (and (< SAL_ASSIGNMENT_BEG i) (< i SAL_ASSIGNMENT_END)))

(define (SalLiteralType? i) 
  (and (< SAL_LITERAL_BEG i) (< i SAL_LITERAL_END)))

(define (SalStatementType? i) 
  (and (< SAL_STATEMENT_BEG i) (< i SAL_STATEMENT_END)))

(define (SalCommandType? i) 
  (and (< SAL_COMMAND_BEG i) (< i SAL_COMMAND_END)))

(define (SalConstituentType? i) 
  (and (< SAL_CONSTITUENT_BEG i) (< i SAL_CONSTITUENT_END)))

(define (SalClausalType? i) 
  (and (< SAL_CLAUSAL_BEG i) (< i SAL_CLAUSAL_END)))

(define (SalRuleType? i) 
  (and (< SAL_RULE_BEG i) (< i SAL_RULE_END)))

(define (SalType=? typ1 typ2)
  ;; ignore the lower 8 (data) bits when comparing SalTypes
  (= (ash typ1 -8) (ash typ2 -8)))

(define (SalType->index typ)
  ;; shift SalType to 0
  (ash typ -8))

;
;; grammer rules
;

(defrule SalNumberRule (or SalInteger SalFloat SalRatio)
  #f
  #f)

(defrule SalBoolRule (or SalTrue SalFalse)
  #f
  #f)

(defrule SalAtomRule (or SalInteger SalFloat SalRatio SalKeyword
			 SalIdentifier SalTrue SalFalse)
  #f
  #f)

(defrule SalListRule (and SalLCurly (* SalEltRule) SalRCurly)
  (lambda (args errf) 
    (make-parse-unit SalListRule (cadr args) #f))
  (lambda ( unit info errf)
    (let ((lev (get-emit-info #:list info)) ; t if we are sublist
	  (sub (parse-unit-parsed unit)))
      (if (not lev) 
	  ;; we are at top level of a list.  make a null
	  ;; top-level {} call (list) to create the list.
	  (if (null? sub)
	      (list 'list)
	      (let* ((i (add-emit-info #:list #t info))
		     (l (emit sub i errf))
		     (q #f))	
		(if (eq? (get-emit-info #:list i)  #:unquote)
		    (set! q (list 'quasiquote l))
		    (set! q (list 'quote l)))
		;; if in a bindings copy the constant list to avoid user
		;; side effecting constant (quoted) data.
		(if (get-emit-info #:bindings info)
		    `(append ,q (list))
		    q)))
	  (emit sub info errf))))
  )

(defrule SalUnquoteRule (and (or SalUnquote SalSplice) SalSexprRule)
  (simple-unit-parser SalUnquoteRule)
  (lambda (unit info errf)
    (let* ((subs (parse-unit-parsed  unit))
	   (unq? (token-unit-type=? (car subs) SalUnquote))
	   (lis? (get-emit-info #:list info)))
      (if (not lis?) 
	  ( errf (make-parse-error
		  (if unq? "#$ outside of list." 
		      "#^ outside of list." )
		  (parse-unit-position (car subs)))))
      ;; emit subform with existing #:list flag removed from info
      (let ((expr (emit (second subs)
			(rem-emit-info #:list info)
			errf)))
	;; tell superior list that we have an unquote
	(set-emit-info! #:list #:unquote info)
	(if unq?
	    (list 'unquote expr)
	    (list 'unquote-splicing expr))))))

(defrule SalEltRule (or SalAtomRule SalListRule SalString SalUnquoteRule)
  #f
  #f)

(defrule SalArefRule (and SalIdentifier SalLBrace SalSexprRule SalRBrace)
  ;; NEW: only 1 index allowed: [ index ]
  (lambda (args errf)
    ;; <id> <lb> <parg> <rb> 
    (make-parse-unit SalArefRule (list (first args) (third args)) #f))
  (lambda (unit info errf)
    (cons 'list-ref (emit (parse-unit-parsed unit) info errf)))
  )

(defrule SalIfExprRule (and SalQMark SalLParen SalSexprRule SalComma
			    SalSexprRule
			    (@ SalComma SalSexprRule) SalRParen)
  (lambda (args errf)
    ;; #? ( <sexpr> , <sexpr> [, <sexpr>])
    (let ((elseclause (sixth args)))
      (make-parse-unit SalIfExprRule
		       (list (third args)
			     (fifth args)
			     (if (pair? elseclause)
				 (second elseclause)
				 #f))
		       #f)))
  (lambda (unit info errf)
    (let* ((subs (parse-unit-parsed unit))
	   (test (emit (first subs) info errf))
	   (then (emit (second subs) info errf))
	   (else (third subs)))
      (if else
	  (list 'if test then (emit else info errf))
	  (list 'if test then #f)))))

(define (remove-token-type toks typ)
  ;; remove all comma to
  (let* ((head (list #t))
	 (tail head))
    (do ((toks toks (cdr toks)))
	((null? toks) (cdr head))
      (if (token-unit-type=? (car toks) typ)
	  #f
	  (begin
	    (set-cdr! tail (list (car toks)))
	    (set! tail (cdr tail)))))))
		      
; this could be (and sexpr (@ comma sexpr))
(defrule SalPargsRule (or (and SalSexprRule (+ SalComma SalSexprRule))
			  SalSexprRule)
  (lambda (args errf)
    ;; args is 1 arg or (arg (:co arg ...))
    ;;(print (list #:pargs-> args))
    (if (not (pair? args))
	(list args)
	(if (and (pair? (cdr args))
		 (pair? (cadr args))
		 (token-unit-type=? (car (cadr args)) SalComma))
	    (cons (car args)
		  (remove-token-type (cadr args) SalComma))
	    (list args))))
  #f)

; this could be (and key sexpr (@ comma key sexpr))
(defrule SalKargsRule (or (and SalKeyparam SalSexprRule
			       (+ SalComma SalKeyparam SalSexprRule))
			  (and SalKeyparam SalSexprRule))
  (lambda (args errf)
    (cons (car args)
	  (cons (cadr args)
		(if (and (pair? (cddr args))
			 (pair? (caddr args))
			 (token-unit-type=? (car (caddr args)) SalComma))
		    (remove-token-type (caddr args) SalComma)
		    (list)))))
  #f)

(defrule SalFunargsRule (or (and SalLParen SalRParen)
			    (and SalLParen SalPargsRule SalRParen)
			    (and SalLParen SalKargsRule SalRParen)
			    (and SalLParen SalPargsRule SalComma 
				 SalKargsRule SalRParen))
  (lambda (args errf) 
    ;; args is (:lp ... :rp) where ... are pargs and/or kargs with :co
    ;; separater. omit lp/rp tokens 
    ;;(print (list :funargs-> args))
    (set! args (cdr args)) ; remove :lp
    (let ((args1 (car args)))
      (set! args (cdr args)) ; pop args
      (if (token-unit-type=? args1 SalRParen)
	  (list)
	  (if (token-unit-type=? (car args) SalComma)
	      (append args1 (cadr args))
	      args1))))
  #f)

(defrule SalFuncallRule (and SalIdentifier SalFunargsRule)
  (lambda (args errf)
    ;; args is (<id> (<arg>...))
    (make-parse-unit SalFuncallRule
		     (cons (car args) (cadr args))
		     #f))
  (lambda (unit info errf)
    (let* ((form (parse-unit-parsed unit))
	   (func (emit (car form) info errf))
	   ;;(spec (assoc func *special-forms* ))
	   (spec #f))
    (if spec
	( (second spec) form info errf)
	(cons func (emit (cdr form) info errf)))))
  )

(define (is-op? x)
  (if (token-unit? x)
      (let ((typ (parse-unit-type x)))
	(and (SalOpType? typ )
	     (SalOpWeight typ)))
      #f))

(define (inf->pre inf)
  ;; this does NOT rewrite subexpressions because parser applies rules
  ;; depth-first so subexprs are already processed
  (let ((op #f)
	(lh #f)
	(rh #f)
	(w1 #f)
	)
    (if (pair? inf)
	(do ()
	    ((null? inf) lh)
	  (set! op (car inf))	  ; look at each element of inf
	  (set! inf (cdr inf))
	  (set! w1 (is-op? op))
	  (cond ((number? w1)	  ; found op
		 (do ((w2 #f)
		      (ok #t)
		      (li (list)))
		     ((or (null? inf) (not ok))
		      (set! rh (inf->pre (reverse li)))
		      (set! lh (if lh (list op lh rh)
				   (list op rh #f))))
		   (set! w2 (is-op? (car inf)))
		   (if (and w2 (<= w2 w1))
		       (set! ok #f)
		       (let ((i (car inf)))
			 (set! inf (cdr inf))
			 (set! li (cons i li))))))
		(else
		 (set! lh op))))
	inf)))

(defrule SalOpRule (or SalPlus SalMinus SalTimes SalDivide
		       SalMod SalExpt SalEqual SalNotEqual
		       SalLess SalGreater SalLessEqual SalGreaterEqual
		       SalGeneralEqual SalNot SalAnd SalOr)
  #f
  #f)

;REPLACED:
;(defrule SalMexprRule (and SalTermRule (* SalOpRule SalTermRule))
;  (lambda (args errf)
;    (if (and (pair? (cadr args))
;	     (is-op? (car (cadr args))))
;	(let ((infix (cons (car args) (cadr args))))
;	  (inf->pre infix))
;	(car args)))
;  #f)
;
;(defrule SalTermRule (or (and SalMinus SalTermRule)
;			 (and SalNot SalTermRule)
;			 (and SalLParen SalMexprRule SalRParen)
;			 SalIfExprRule SalFuncallRule SalArefRule
;			 SalAtomRule SalListRule SalString)
;  (lambda (args errf) 
;    ;;(print (list #:term-> args))
;    ;; "tag" terms so mexpr can tell the difference between funcalls
;    ;; and mexprs
;    (make-parse-unit SalTermRule
;		     (if (pair? args)
;			 (if (token-unit-type=? (car args) SalLParen )
;			     (second args)
;			     args)
;			 args)
;		     #f))
;  (lambda (unit info errf)
;    (emit (parse-unit-parsed unit) info errf))
;  )
;
;(defrule SalSexprRule (or SalMexprRule SalIfExprRule SalFuncallRule
;			  SalArefRule SalListRule SalString
;			  SalClass SalAtomRule)
;  #f
;  #f)
;
;WITH:

(defrule SalTermRule (or (and SalMinus SalTermRule)
			 (and SalNot SalTermRule)
			 (and SalLParen SalSexprRule SalRParen)
			 SalIfExprRule SalFuncallRule SalArefRule
			 SalAtomRule SalListRule SalString)
  (lambda (args errf) 
    ;;(print (list #:term-> args))
    ;; "tag" terms so mexpr can tell the difference between funcalls
    ;; and mexprs
    (make-parse-unit SalTermRule
		     (if (pair? args)
			 (if (token-unit-type=? (car args) SalLParen )
			     (second args)
			     args)
			 args)
		     #f))
  (lambda (unit info errf)
    (emit (parse-unit-parsed unit) info errf))
  )

(defrule SalSexprRule (and SalTermRule (* SalOpRule SalTermRule))
  (lambda (args errf)
    (if (and (pair? (cadr args))
	     (is-op? (car (cadr args))))
	(let ((infix (cons (car args) (cadr args))))
	  (inf->pre infix))
	(car args)))
  #f)

; END



;
;; Statements
;

(defrule SalPrintStatementRule
  (and SalPrint SalSexprRule (* SalComma SalSexprRule))
  (lambda (args errf)
    ;; args = (<print> <sexpr> ({<,> <sexpr>}*)
    (make-parse-unit SalPrintStatementRule
		     (cons (cadr args)
			   (remove-token-type (caddr args) SalComma)
			   )
		     #f))
  (lambda (unit info errf) 
    (let* ((form (list 'sal:print))
	   (tail form))
      (do ((args (parse-unit-parsed unit) (cdr args)) )
	  ((null? args)
	   form)
	(set-cdr! tail (list (emit (car args) info errf)))
	(set! tail (cdr tail))))))

; realtime:
; (sprout (foo))
; (sprout (foo) 0)
; (sprout (foo) 0 1)
; scorefile:
; (sprout (foo) "foo.text")
; (sprout (foo) 0 "foo.text")

(defrule SalSproutStatementRule 
  (and SalSprout SalSexprRule (* SalComma SalSexprRule)
       (* SalComma SalKeyparam SalSexprRule)
       )
  (lambda (args errf)
    ;; args (<sprout> <sexpr> ({<,> <sexpr>*) ({<,> <keyw> <expr>}* )
    (make-parse-unit SalSproutStatementRule
		     (cons (cadr args)
			  (append (remove-token-type (caddr args) SalComma) 
				(remove-token-type (cadddr args) SalComma)))
		     #f))
  (lambda (unit info errf)
    (let ((subs (parse-unit-parsed unit)))
      ;; FIX this should check to make sure proper num args
      (cons 'sprout (emit subs info errf))
      )))

(defrule SalSendStatementRule
  (and SalSend (or SalString SalIdentifier)
       (@ SalComma (or (and SalPargsRule SalComma SalKargsRule) 
		       SalPargsRule
		       SalKargsRule)))
  (lambda (args errf)
    ;; (<send> <msg> #f)
    ;; (<send> <msg> ( <,> (...)))
    ;;;;(print (list #:parse-send-> args))
    (let ((rest (third args)))
      (if (not rest)
	  (set! rest (list))
	  (begin
	    (set! rest (second rest))
	    ;; (<arg> <arg2> ...)
	    ;; (<key> <arg> <key2> <arg2>)
	    ;; ((<arg> ...) <,> (<key> <arg> ...) )
	    ;; if there is a <,> its positionals AND keys
	    (if (and (not (null? (cdr rest)))
		     (token-unit-type=? (second rest) SalComma))
		;; remove <,>
		(set! rest (append (first rest) (third rest))))))
      (make-parse-unit SalSendStatementRule
		       (cons (second args) rest)
		       (parse-unit-position (car args))
		       )))
  (lambda (unit info errf) 
    ;;(format #t "~S~%" (list #:expand-send-> unit (parse-unit-parsed unit) info errf))
    (let* ((data (emit (parse-unit-parsed unit) info errf))
	   (mesg (first data)))
      ;; check args for mesg
      (expand-send (car data) (cdr data)
			  (lambda (str)
			    ( errf (make-parse-error
				    str
				    (parse-unit-position unit))))))))

(defrule SalExecStatementRule (and SalExec SalSexprRule)
  (lambda (args errf) (second args))
  #f)

(defrule SalLoadStatementRule (and SalLoad SalSexprRule)
  (simple-unit-parser SalLoadStatementRule)
  (simple-unit-emitter 'sal:load))

(defrule SalChdirStatementRule (and SalChdir SalSexprRule)
  (simple-unit-parser SalChdirStatementRule)
  (simple-unit-emitter 'sal:chdir))

(defrule SalPlotStatementRule
  (and SalPlot (or (and SalKeyparam SalSexprRule) SalSexprRule)
       (* SalComma (or (and SalKeyparam SalSexprRule)
		       SalSexprRule)))
  (lambda (args errf)
    (make-parse-unit
     SalPlotStatementRule
     (cons (second args)
		 (remove-token-type (third args)
				    SalComma))
     #f))
  (lambda (unit info errf)
    (let* ((data (emit (parse-unit-parsed unit) info errf))
	   (head (list 'plot))
	   (tail head))
      (do ()
	  ((null? data) head)
	(cond ((and (pair? (car data))
		    (keyword? (caar data)))
	       (set-cdr! tail (car data))
	       (set! tail (cddr tail)))
	      (else
	       (set-cdr! tail (list (car data)))
	       (set! tail (cdr tail))))
	(set! data (cdr data)))))
  )

(defrule SalProcessWaitRule (and SalWait SalSexprRule)
  (simple-unit-parser SalProcessWaitRule)
  (lambda (unit info errf)
    (let ((data (parse-unit-parsed unit)))
      (if (not (get-emit-info #:run info))
	  ( errf (make-parse-error "wait statement outside run block"
				   (parse-unit-position
				    (car data)))))
      (emit data info errf))))

(defrule SalFunctionReturnRule
  (and SalReturn SalSexprRule)
  (simple-unit-parser SalFunctionReturnRule)
  (lambda (unit info errf)
    (let ((data (parse-unit-parsed unit)))
      (if (not (get-emit-info #:function info))
	  ( errf (make-parse-error "return statement outside function"
				   (parse-unit-position
				    (car data)))))
      ;; tell calling function that we have a return statemen
      (set-emit-info! #:function #:return info)
      (emit data info errf))))

;;
;; WITH variable bindings (not a statement)
;;

(defrule SalBindRule (or (and SalIdentifier SalEqual SalSexprRule)
			 SalIdentifier)
  (lambda (args errf)
     (if (pair? args)
	 (make-parse-unit SalBindRule
			  (list (first args) (third args))
			  #f)
	 (make-parse-unit SalBindRule
			  (list args 
				(make-parse-unit SalFalse "#f" #f)
				)
			  #f)))
  (lambda (unit info errf) 
    (map (lambda (e) (emit e info errf))
	 (parse-unit-parsed unit))))

(defrule SalBindingsRule
  (and SalWith SalBindRule (* SalComma SalBindRule))
  (lambda (args errf)
    ;; remove WITH token
    (if (not (third args))
	(make-parse-unit SalBindingsRule
			 (list (second args))
			 #f)
	(make-parse-unit SalBindingsRule
			 (cons (second args)
			       (remove-token-type (third args)
						  SalComma))
			 #f)))
  (lambda (unit info errf) 
    ;; tell subforms that the context is a let bindings
    (let ((i (add-emit-info #:bindings #t info)))
      (map (lambda (e) (emit e i errf))
	   (parse-unit-parsed unit)))))

(define (check-for-redefinition bindings type)
  (do ((tail bindings (cdr tail)))
      ((null? tail) #f)
    ;; each binding can be a variable name or a list (variable value)
    (let ((sym (if (pair? (car tail)) (car (car tail)) (car tail))))
      (warn-if-redefinition sym type)
      )))

(define (warn-if-redefinition sym typ)
  (if (and (symbol? sym)
           (defined? sym))
      (let ((val (symbol->value sym))
            (nam (symbol->string sym))
            (str (if (eq? typ 'variable) "local variable"
                     (if (eq? typ 'parameter) "function parameter"
                         "function parameter or a local variable name")))
            )
        (if (procedure? val)
            (print-warning (string-append "Warning: the variable '" nam "' is already defined as a procedure. Using '" nam "' as a " str " may lead to unanticipated behavior or errors within the scope of the new definition.\n"))
            (print-warning (string-append "Warning: the variable '" nam "' is already defined. Using '" nam "' as a " str " may lead to unanticipated behavior or errors within the scope of the new definition.\n"))
            )))
  #f)

;;;
;;; assignment (set)
;;;

(defrule SalAssignRule
  (and (or SalArefRule SalIdentifier)
       (or SalEqual SalLessEqual SalGreaterEqual
	   SalInc SalMul SalCol SalPre SalApp)
       SalSexprRule)
  (lambda (args errf)
    (make-parse-unit SalAssignRule args #f))
  )

(defrule SalAssignmentRule
  (and SalSet SalAssignRule (* SalComma SalAssignRule))
  (lambda (args errf)
    (if (not (third args))
	(make-parse-unit SalAssignmentRule
			 (list (second args))
			 #f)
	(make-parse-unit SalAssignmentRule
			 (cons (second args)
			       (remove-token-type (third args)
						  SalComma))
			 #f)))
  (lambda (unit info errf)
    (let ((assigns (parse-unit-parsed unit)))
      ;;(print (list #:emit-assignment assigns))
      (let* ((head (list 'begin ))
	     (tail head))
	(do ((iter assigns (cdr iter)))
	    ((null? iter)
	     ;; remove surrounding begin if only one form
	     (if (null? (cddr head)) 
		 (cadr head)
		 head))
	  (let* ((asgn (parse-unit-parsed (car iter)) )
		 (vref (emit (first asgn) info errf))
		 ;; dont emit the operator just check its type
		 (oper (parse-unit-type (second asgn)))
		 (expr (emit (third asgn) info errf))
		 (form #f))
	    ;;(print (list #:assignment asgn))
	    (cond ((SalType=? oper SalEqual)
		   (set! form expr))
		  ((SalType=? oper SalInc)
		   (set! form `(+ ,vref ,expr)))
		  ((SalType=? oper SalMul)
		   (set! form `(* ,vref ,expr)))
		  ((SalType=? oper SalCol)
		   (set! form `(append ,vref (list ,expr))))
		  ((SalType=? oper SalPre)
		   (set! form `(cons ,expr ,vref)))
		  ((SalType=? oper SalApp)
		   (set! form `(append ,vref ,expr)))
		  ((SalType=? oper SalLessEqual)
		   (set! form `(min ,vref ,expr)))
		  ((SalType=? oper SalGreaterEqual)
		   (set! form `(max ,vref ,expr))))
	    ;; if we are setting a list location then have to rewrite
	    ;; as a list setter. this WON'T work for AREFS...
	    (if (and (pair? vref) (eq? (first vref) 'list-ref))
		(set! form `(set-car! (list-tail ,(second vref)
						 ,(third vref))
				      ,form))
		(set! form `(set! ,vref ,form)))
	    (set-cdr! tail (list form))
	    (set! tail (cdr tail))
	    ))))))

(defrule SalBlockRule
  (and SalBegin (@ SalBindingsRule) (* SalStatementRule) SalEnd)
  (lambda (args errf)
    ;; args: (BEGIN [(<bindings>)] (...) END)
    ;;(print (list #:block-> args))
    (let ((vars (second args))
	  (body (third args)))
      (if (not vars) #f (set! vars (first vars)))
      (make-parse-unit SalBlockRule
		       (cons vars body)
		       #f)))
  (lambda (unit info errf)
    ;; ( <bindings> . <statements>)
    (let ((bloc (parse-unit-parsed unit)))
      (let ((vars (car bloc)) ; #<bindings> or #f
	    (body (emit (cdr bloc) info errf)))
	;; at some point this could pass var decls into (emit body) to
	;; catch unknown variable errors etc.
	;;(print (list #:vars-> vars))
	(if (not vars)
	    `(begin ,@body)
            (let ((bind (emit vars info errf)))
              (check-for-redefinition bind 'variable)
              `(let* ,bind
                 ,@body))))))
  )

;; FIX: Should WHEN and UNLESS be Commands ??

(defrule SalConditionalRule 
  (or (and SalIf SalSexprRule SalThen (@ SalStatementRule)
	   (@ SalElse SalStatementRule))
      (and SalWhen SalSexprRule SalStatementRule)
      (and SalUnless SalSexprRule SalStatementRule))
  (lambda (args errf)
    ;;(print (list #:if-> args))
    (let ((oper (first args))
	  (test (second args))
	  (data (list)))
      (if (token-unit-type=? oper SalIf)
	  (let ((clause (fourth args))
		(then #f)
		(else #f))
	    (if (not clause)
		(set! then (make-parse-unit SalFalse "#f" #f))
		(set! then (first clause)))
	    (set! clause (fifth args))
	    (if (not clause)
		(set! else (make-parse-unit SalFalse "#f" #f))
		(set! else (cadr clause)))
	    (set! data (list oper test then else)))
	  (set! data (list oper test (third args))))
      (make-parse-unit SalConditionalRule data #f)))
  (lambda (unit info errf)
    (let* ((data (parse-unit-parsed unit)))
      ;;(print (list #:data-> data))
      (cond ((token-unit-type=? (car data) SalIf)
	     (emit data info errf))
	    ((token-unit-type=? (car data) SalWhen)
	     `(if ,(emit (cadr data) info errf)
		  , (emit (caddr data) info errf)))
	    (else
	     `(if (not ,(emit (cadr data) info errf))
		  , (emit (caddr data) info errf))))))
  )

;;
;; iteration
;;

(define (sal-parse-stepping args errf)
  ;; return a stepping clause used by loop and run.  each clause is
  ;; represented by one or more five element lists:
  ;; (<var> <bind> <loop> <step> <stop>)
  ;; when <var> is a variable to bind, <bind> is its binding list
  ;; value, <loop> is its assignment statement at the top of the loop,
  ;; <step> is its incrementing expression at the bottom of the loop,
  ;; and <stop> is an expression that, if true, stops the iteration.
  (let ((data (list))
	(user #f)
	(expr #f)
	(temp #f)
	(stop #f)
	(then #f)
	(bool #f)
	(tail #f)
	(luup #f)
	(size #f)
	(mode #f)
	(goal #f)
	(from #f)
	(step #f))
    (cond ((token-unit-type=? (first args) SalRepeat)
	   ;; (repeat <expr>)
	   (set! expr (emit (second args) (list) errf))
	   (set! temp (gensym "temp"))
	   (set! stop (gensym "stop"))
	   (set! data (list (list stop expr #f #f #f)
			    (list temp 0 #f
				  `(set! ,temp (+ ,temp 1))
				  `(>= ,temp ,stop)))))
	  ((token-unit-type=? (third args) SalEqual)
	   ;; (for <v> = <expr> #f)
	   ;; (for <v> = <expr> (then <expr>))
	   (set! user (emit (second args) (list) errf))
	   (set! expr (emit (fourth args) (list) errf))
	   (set! then (fifth args))
	   (if then
	       (begin
		 (set! then (emit (cadr then) (list) errf))
		 (set! bool (gensym "bool"))
		 (set! luup `(if ,bool
				 (begin (set! ,bool #f) 
					(set! ,user ,expr))
				 (set! ,user ,then)))
		 (set! data (list (list bool #t #f #f #f))))
	       (set! luup `(set! ,user ,expr)))
	   (set! data (append data (list (list user #f luup #f #f)))))
	  ((token-unit-type=? (third args) SalIn)
	   ;; (for <v> in <expr>)
	   (set! user (emit (second args) (list) errf))
	   (set! expr (emit (fourth args) (list) errf))
	   (set! tail (gensym "tail"))
	   (set! data (list (list tail expr #f 
				  `(set! ,tail (cdr ,tail))
				  `(null? ,tail))
			    (list user #f `(set! ,user (car ,tail)) 
				  #f #f))))
	  (else
	   (set! size (length args))
	   (set! user (emit (second args) (list) errf))
	   ;; the LAST element always #f or (by <expr>)
	   (set! step (list-ref args (- size 1)))
	   (if step
	       (set! step (emit (cadr step) (list) errf))
	       (set! step 1))
	   (cond ((= size 6)
		  ;; (for <var> from <expr> (LIMIT <expr>) (by <expr>))
		  ;; (for <var> from <expr> (LIMIT <expr>) #f)
		  ;; (for <var> from <expr> #f #f)
		  (set! from (emit (fourth args) (list) errf))
		  (if (fifth args)
		      (begin (set! mode (car (fifth args)))
			     (set! goal (emit (cadr (fifth args))
					      (list) errf)))
		      (begin (set! mode #f)
			     (set! goal #f))))
		 (else
		  ;; (for <var> LIMIT <expr> (by <expr>))
		  ;; (for <var> LIMIT <expr> #f)
		  (set! from 0)
		  (set! mode (third args))
		  (set! goal (emit (fourth args) (list) errf))))
	   (if (not (number? step))
	       (let ((var (gensym "step")))
		 (set! data (append data (list (list var step #f #f #f))))
		 (set! step var)))
	   (if (not (number? from))
	       (let ((var (gensym "from")))
		 (set! data (append data (list (list var from #f #f #f))))
		 (set! from var)))
	   (if goal
	       (if (not (number? goal))
		   (let ((var (gensym "goal")))
		     (set! data (append data 
					(list (list var goal #f #f #f))))
		     (set! goal var))))
	   (if (not mode)
	       (set! step `(set! ,user (+ ,user ,step)))
	       (cond ((token-unit-type=? mode SalBelow)
		      (set! stop `(>= ,user ,goal))
		      (set! step `(set! ,user (+ ,user ,step))))
		     ((token-unit-type=? mode SalTo)
		      (set! stop `(> ,user ,goal))
		      (set! step `(set! ,user (+ ,user ,step))))
		     ((token-unit-type=? mode SalAbove)
		      (set! stop `(<= ,user ,goal))
		      (set! step `(set! ,user (- ,user ,step))))
		     ((token-unit-type=? mode SalDownto)
		      (set! stop `(< ,user ,goal))
		      (set! step `(set! ,user (- ,user ,step))))))
	   (set! data (append data 
			      (list (list user from #f step stop))))))
    (make-parse-unit SalSteppingRule data #f)))

(defrule SalSteppingRule
  (or (and SalRepeat SalSexprRule)
      (and SalFor SalIdentifier SalEqual SalSexprRule 
	   (@ SalThen SalSexprRule))
      (and SalFor SalIdentifier SalIn SalSexprRule)
      ;;(and SalFor SalIdentifier SalOver SalSexprRule (@ SalBy SalSexprRule))
      (and SalFor SalIdentifier SalFrom SalSexprRule
	   (@ (or SalBelow SalTo SalAbove SalDownto) SalSexprRule)
	   (@ SalBy SalSexprRule))
      (and SalFor SalIdentifier 
	   (or SalBelow SalTo SalAbove SalDownto) SalSexprRule
	   (@ SalBy SalSexprRule)))
  sal-parse-stepping
  #f)

(defrule SalTerminationRule
  (or (and SalWhile SalSexprRule) (and SalUntil SalSexprRule))
  (simple-unit-parser SalTerminationRule)
  #f)

(define (sal-emit-iteration unit info errf)
    ;; DATA: (<loop|run> <with> (<stepping>) (<stop>) (<actions>) <finally>)
  (let* ((data (parse-unit-parsed unit))
	 (run? #f)
	 ;; forms
	 (bind (list))
	 (luup (list))
	 (step (list))
	 (stop (list))
	 (done (list))
	 (body (list))
	 (punt (list))
	 )
    ;;(format #t "sal-emit-iteration: data=~S~%" data)
    ;; tell subforms what type of iteration we are
    (cond ((token-unit-type=? (car data) SalLoop)
	   (set! info (add-emit-info #:luup #t info)))
	  (else
	   (if (not (get-emit-info #:process info))
	       ( errf (make-parse-error "process statement outside process" 
					(parse-unit-position
					 (car data))) ))
	   (set! info (add-emit-info #:run #t info))
	   (set! run? #t)))
    ;; pop loop|run tag off data
    (set! data (cdr data))
    ;; add user's with variables to binding list
    (if (first data)
	(set! bind (append bind (emit (first data) info errf))))
    ;; map over the stepping clauses adding iteration forms
    (do ((tail (second data) (cdr tail)))
	((null? tail) #f)
      ;; eah stepping clause contains a list of 5-element lists:
      ;; ( (<var> <bind> <loop> <step> <stop>) ...)
      (do ((clauses (parse-unit-parsed (car tail)) (cdr clauses)))
	  ((null? clauses) #f)
	(let ((clause (first clauses)))
	  ;; add stepping vars to binding list BUGFIX: check to make
	  ;; sure that the variable isn't duplicated
	  (if (assoc (first clause) bind)
	      ( errf (make-parse-error 
		      (string-append "found duplicated variable '" 
				     (symbol->string (car clause)) "'")
		      (parse-unit-position unit))))
	  (set! bind (append bind (list (list (first clause) 
					      (second clause)))))
	  (if (third clause)
	      (set! luup (append luup (list (third clause)))))
	  (if (fourth clause)
	      (set! step (append step (list (fourth clause)))))
	  (if (fifth clause)
	      (set! stop (append stop (list (fifth clause))))))))
    ;; loop var now holds all the 'for' assignments, now add users
    ;; actions and stepping clauses as the body of the loop
    
    ;; get user's action statements 
    (do ((tail (fourth data) (cdr tail)))
	((null? tail) #f)
      (set! body (append body (list (emit (car tail) info errf)))))
    ;; add stepping forms after all the actions.
    ;;;;s7(set! body (append body step))

    ;; if there is a while or until then the exit code has to appear
    ;; lexically after all the for variables have values and before
    ;; the user's action clauses.
    (if (pair? (third data))
	(let ((test (list))
	      (halt (if run? *process-stop* '(go #f))))
	  (do ((tail (third data) (cdr tail)))
	      ((null? tail) #f)
	    ;; data is (while|until <sexpr>)
	    (let* ((data (parse-unit-parsed (car tail)))
		   (form (emit (second data) info errf)))
	      (if (token-unit-type=? (car data) SalWhile)
		  (set! form `(not , form)))
	      (set! test (append test (list form)))))
	  ;; concat multiple tests into a single or
	  (if (null? (cdr test))
	      (set! test (car test))
	      (set! test (cons 'or test)))
	  ;; for processes setting halt to #t will cause the func to
	  ;; run 1 more time at which point it will do its exit code
	  (set! punt `((if , test , halt)))
	  ))
    
    ;; turn all stop forms into one valid lisp expression
    (if (pair? stop)
	(if (null? (cdr stop))
	    (set! stop (car stop)) ; only one
	    (set! stop (cons 'or stop))) ; or together
	(begin
	  ;;(warn "A (possibly) non-terminating iteration is being defined.")
	  (set! stop #f)))
    ;; add finally clause, defaults to #f
    (if (fifth data)
	(set! done (emit (fifth data) info errf))
	(set! done #f))
    (check-for-redefinition bind 'variable)
    (if (not run?)
	;; LOOP expansions
	(if (null? punt)
	    ;; simple cases can be implemented with a do loop
	    `(let* ,bind
	       (do () 
		   (,stop , done)
		 ,@luup ,@body ,@step))
	    ;; while/until requires a lambda expr with a call/cc exit
	    ;; for the while/until and post-exit execution of any
	    ;; finally clause
	    `((lambda ()
		(let* ,bind
		  (call-with-current-continuation
		   (lambda (go )
		     (do ()
			 (,stop #f)
		       ,@luup ,@punt ,@body ,@step)))
		  ,done
		  ;;(void)
                  ))
	      )
	    )
	;; PROCESS EXPANSIONS
	(expand-process
	 (make-loop-clause 'operator 'process
			   'bindings bind
			   'collectors (list)
			   'initially (list)
			   'end-tests (list stop)
			   'looping (append luup punt body)
			   'stepping step
			   'finally (or done (list))
			   'returning (list))
	 )
	)))

(defrule SalLoopStatementRule
  (and (or SalLoop SalRun)
       (@ SalBindingsRule)
       (* SalSteppingRule )
       (* SalTerminationRule)
       (+ SalStatementRule)
       (@ SalFinally SalStatementRule)
       SalEnd)
  (lambda (args errf)
    (let ((type (first args))
	  (vars (second args))
	  (step (third args))
	  (stop (fourth args))
	  (body (fifth args))
	  (done (sixth args)))
      ;; #f or (<bindings)
      (if vars (set! vars (car vars)))
      (if done (set! done (cadr done)))
      (make-parse-unit SalLoopStatementRule
		       (list type vars step stop body done)
		       (parse-unit-position type))))
  (lambda (unit info errf)
    (sal-emit-iteration unit info errf ))
  )

;;;

(defrule SalFunDeclRule
  (and SalIdentifier
       (or (and SalLParen SalRParen)
	   (and SalLParen 
;		SalIdentifier (* SalComma SalIdentifier)
		SalBindRule (* SalComma SalBindRule)
		SalRParen))
       SalStatementRule)
  (lambda (args errf)
    ;;(print (list #:funcdecl-> args))
    (let ((name (first args))
	  (pars (second args))
	  (body (third args)))
      (if (= (length pars) 2)
	  ;; pars: ( <(>  <)> )
	  (set! pars (list))
	  ;; pars: ( <(> <sym> (<,> <sym> ...) <)> )
	  (set! pars (cons (second pars)
			   (remove-token-type (third pars) SalComma))))
      (make-parse-unit SalFunDeclRule (list name pars body) #f)) ))

(defrule SalDefineStatementRule
  (and SalDefine
       (or (and SalVariable SalBindRule (* SalComma SalBindRule))
	   (and SalFunction SalFunDeclRule)
	   (and SalProcess SalFunDeclRule)))
  (lambda (args errf)
    ;; args: (<define> (<TYPE> ...))
;    (print (list #:define-args-> args))
    ;; flush <define> and set args to (<TYPE> ...)
    (set! args (second args)) ; 
    (let ((type (first args)))
      (if (token-unit-type=? type SalVariable)
	  ;; flatten args to (<type> <bind> ...)
	  (set! args
		(cons type (cons (second args)
				 (remove-token-type (third args)
						    SalComma)))))
      (make-parse-unit SalDefineStatementRule args #f)))
  (lambda (unit info errf)
    (let* ((data (parse-unit-parsed unit))
	   (type (first data))
	   (head (list 'begin))
	   (tail head))
      ;; data: (<variable> <bind> ....) OR (<function> <fundecl>)
      (cond ((token-unit-type=? type SalVariable)
	     ;; rest of data is list of bindings
	     (do ((rest (cdr data) (cdr rest)))
		 ((null? rest) #f)
	       (let* ((bind (emit (car rest) info errf))
		      (var (car bind))
		      (str (format #f "Variable: ~A = " var)))
	       (set-cdr! tail (list (cons 'define bind)
				    `(sal:print ,str ,var)))
	       (set! tail (cddr tail)))))
	    (else ;; function or process. second element is
		  ;; <SalFunDeclRule>: (<name> <params> <body>)
	     (let* ((data (parse-unit-parsed (second data)))
		    (name (emit (first data) info errf))
		    (pars (emit (second data) info errf))
		    ;; copy pars for possible sideeffect
		    (decl (cons name (append pars (list))))
		    (body (third data))
		    (mesg #f)
		    (form #f)
		    )
               ;; (format #t "~S~%" (list #:procdata-> name pars body))
	       ;; decl is (funcname {arg}*). if last arg contains
	       ;; "..." then its an &rest arg: replace last cdr of
	       ;; decl with arg minus the "..."
	       (if (not (null? (cdr decl)))
		   (let* ((tail (list-tail decl (- (length decl) 2)))
			  (name (symbol->string (car (cadr tail))))
			  (size (string-length name)))
		     (if (and (> size 3)
			      (string=? (substring name (- size 3)) 
					"..."))
			 (set-cdr! tail
				   (string->symbol
				    (substring name 0 (- size 3)))))))
	       (cond ((token-unit-type=? type SalFunction)
		      (set! info (add-emit-info #:function #t info))
		      (set! form (emit body info errf))
		      ;(print (list #:after-emitinfo-> info))
		      (if (eq? #:return (get-emit-info #:function info))
			  (set! form
				`(call-with-current-continuation
				  (lambda (return) ,form))))
		      (set! form (list 'define* decl form))
		      (set! mesg "Function: "))
		     ((token-unit-type=? type SalProcess)
		      (set! info (add-emit-info #:process #t info))
		      (set! form (emit body info errf))
		      (set! form (list 'define* decl form))
		      (set! mesg "Process: ")))
	       (do ((args (string-append mesg (symbol->string name) " ("))
		    (tail pars (cdr tail)))
		   ((null? tail)
		    (set! mesg `(print-output ,(string-append args ")\n"))))
		 (set! args (string-append args
					   (symbol->string (car (car tail)))))
		 (if (not (null? (cdr tail)))
		     (set! args (string-append args ", "))))
	       ;; message printed before definition to help track
	       ;; compiler warnings
	       (set-cdr! head (list mesg form))
	       )))
      head))
  )

(defrule SalSoundFileStatementRule
  (and SalSoundFile 
       (or SalString SalIdentifier)
       (or (and SalLParen SalRParen)
           (and SalLParen SalKeyparam SalSexprRule
                (* SalComma SalKeyparam SalSexprRule) SalRParen))
       (@ SalBindingsRule) 
       (* SalStatementRule)
       SalEnd)
  (lambda (args errf)
    ;;(format #t "~S~%" (list #:soundfile-> args))
    (let ((file (second args) )
          (opts (third args)  )
          (vars (fourth args) )
          (body (fifth args)))
      ;; opts: ( <(> <)> )
      ;; opts: ( <(> <key> <expr> ( {<,> <key> <expr>}* ) <)> )
      (if (null? (cddr opts)) ; empty opts: ( <(> <)> )
          (set! opts (list))
          (set! opts (cons (second opts) 
                           (cons (third opts)
                                 (remove-token-type (fourth opts)
                                                    SalComma)))))
      (if vars (set! vars (first vars)))
      (make-parse-unit SalSoundFileStatementRule
                       (list file opts vars body)
                     #f)))
  (lambda (unit info errf)
    (let* ((args (parse-unit-parsed unit))
           (file (emit (first args) info errf) )
           (opts (emit (second args) info errf))
           (vars (third args) )
           (body (emit (fourth args) info errf))
           (func #f))
      (if (string? file)
          (let ((typ (pathname-type file)))
            (if (string? typ)
                (set! opts (append opts 
                                   (default-header-type typ)
                                   (default-data-format typ))))))
      ;; add file name to with-sound args
      (set! opts (cons #:output (cons file opts)))
      ;; if local varables wrap body forms inside let*
      (if vars
          (set! body
                (list
                 `(let* ,(emit (parse-unit-parsed vars) info errf)
                   ,@body))))
      ;; create with-sound-helper thunk
      (set! func `(lambda () 
                    ,@body
                    (format #t "Soundfile: ~S~%Channels: ~S~%Srate: ~S~%"
                            (mus-file-name *output*)
                            (mus-channels *output*)
                            (mus-srate))))
      `(with-sound-helper ,func ,@opts)))
  )

;;;
;;; FomusFile
;;;

(defrule SalFomusFileStatementRule
  (and SalFomusFile 
       (or SalString SalIdentifier)
       (or (and SalLParen SalRParen)
           (and SalLParen SalKeyparam SalSexprRule
                (* SalComma SalKeyparam SalSexprRule) SalRParen))
       (@ SalBindingsRule) 
       (* SalStatementRule)
       SalEnd)
  (lambda (args errf)
    ;;(format #t "~S~%" (list #:fomusfile-> args))
    (let ((file (second args) )
          (opts (third args)  )
          (vars (fourth args) )
          (body (fifth args)))
      ;; opts: ( <(> <)> )
      ;; opts: ( <(> <key> <expr> ( {<,> <key> <expr>}* ) <)> )
      (if (null? (cddr opts)) ; empty opts: ( <(> <)> )
          (set! opts (list))
          (set! opts (cons (second opts) 
                           (cons (third opts)
                                 (remove-token-type (fourth opts)
                                                    SalComma)))))
      (if vars (set! vars (first vars)))
      (make-parse-unit SalFomusFileStatementRule
                       (list file opts vars body)
                     #f)))
  (lambda (unit info errf)
    (let* ((args (parse-unit-parsed unit))
           (file (emit (first args) info errf))
           (opts (emit (second args) info errf))
           (vars (third args) )
           (body (emit (fourth args) info errf)) )
      (if vars
          `(let* ,(emit (parse-unit-parsed vars) info errf)
             (fms:open-score ,file ,@opts)
             ,@ body
             (ffi_fms_close_score))
          `(begin
             (fms:open-score ,file ,@opts)
             ,@body          
             (ffi_fms_close_score)))))
  )

;;;
;;;
;;;

(defrule SalStatementRule
  (or SalDefineStatementRule
      SalBlockRule 
      SalPrintStatementRule
      SalSproutStatementRule
      SalSendStatementRule
      SalAssignmentRule
      SalConditionalRule
      SalLoopStatementRule
      SalProcessWaitRule
      SalFunctionReturnRule
      SalPlotStatementRule
      SalLoadStatementRule
      SalChdirStatementRule
      SalExecStatementRule
      SalSoundFileStatementRule
      SalFomusFileStatementRule
      )
  #f
  #f)

(defrule SalStatementSequenceRule (+ SalStatementRule)
  (lambda (args errf)
    (make-parse-unit SalStatementSequenceRule
		     args (parse-unit-position (first args))))
  (lambda (unit info errf)
    (cons 'begin (emit (parse-unit-parsed unit) info errf))))
    
(define (sal str tokens expand multi)
  (if (pair? tokens)
      (let* ((rule (if multi SalStatementSequenceRule SalStatementRule))
             (scm (parse rule tokens #f 0 #f #f)))
        (cond ((parse-error? scm)
               (print-sal-error str scm #f)
               (void))
              (expand
               (format #t "~S~%" scm)
               (void))
              (else
               (eval scm (interaction-environment))))) ;
      (void)))

;;=========================================================================
;;
;; Sal2
;;
;; begin ... end
;; if (<expr>)  ... [else ...] end
;; function <name> (...) ... end
;; loop ... end
;; file {string} (...) ... end
;; run ... end
;; set ...
;; variable ...
;;=========================================================================

(defrule Sal2VariableStatementRule
  (and Sal2Variable SalBindRule (* SalComma SalBindRule))
  (lambda (args errf)
    ;; args: (<variable> (...))
    ;;(format #t "~S~%" (list #:variable-> args))
    (make-parse-unit Sal2VariableStatementRule
                     (remove-token-type (cons (second args)
                                              (third args))
                                        SalComma)
                     #f))
  (lambda (unit info errf)
    (let* ((head (list 'begin))
	   (tail head))
      (do ((data (parse-unit-parsed unit) (cdr data))
           (bind #f)
           (var #f))
          ((null? data)
           (set-cdr! tail (list var))
           head)
        (set! bind (emit (car data) info errf))
        (set! var (car bind))
        (set-cdr! tail (list (cons 'define bind)))
        (set! tail (cdr tail)))))
  )

(defrule Sal2BeginStatementRule
  (and SalBegin (@ SalBindingsRule) (+ Sal2StatementRule) SalEnd)
  (lambda (args errf)
    ;; args: (BEGIN [(<bindings>)] (...) END)
    ;;(format #t "~S~%" (list #:block-> args))
    (let ((vars (second args))
	  (body (third args)))
      (if vars (set! vars (first vars)))
      (make-parse-unit Sal2BeginStatementRule
		       (cons vars body)
		       #f)))
  (lambda (unit info errf)
    ;; ( <bindings> . <statements>)
    (let* ((bloc (parse-unit-parsed unit))
           (vars (car bloc)))
      (cond ((not vars)
             `(begin ,@ (emit (cdr bloc) info errf)))
            (else
             `(let* ,(emit vars info errf)
                ,@ (emit (cdr bloc)
                         (add-emit-info :let #t info)
                         errf)
                   )))))
  )

(defrule Sal2IfStatementRule 
  (and Sal2If SalLParen SalSexprRule SalRParen
       (+ Sal2StatementRule)
       (@ SalElse (+ Sal2StatementRule))
       SalEnd)
  (lambda (args errf)
    ;; ARGS: <IF> <(> (<expr>...) <)> 
    ;;(format #t "~S~%" (list :if-> args))
    (let ((test (third args))
          (then (fifth args))
          ( else (sixth args)))
      (if else (set! else (cadr else)))
      (make-parse-unit Sal2IfStatementRule
                       (list test then else) #f)))
  (lambda (unit info errf)
    (let* ((data (parse-unit-parsed unit))
           (test (emit (first data) info errf))
           (then (emit (second data) info errf))
           ( else (and (third data)
                       (emit (third data) info errf))))
      (if (null? (cdr then))
          (set! then (car then))
          (set! then (cons 'begin then)))
      (if else
          (if (null? (cdr else))
              (set! else (car else))
              (set! else (cons 'begin else))))
      (list 'if test then else)))
  )

(defrule Sal2LoopStatementRule
  (and SalLoop
       (@ SalBindingsRule)
       (* SalSteppingRule )
       (* SalTerminationRule)
       (+ Sal2StatementRule)
       (@ SalFinally Sal2StatementRule)
       SalEnd)
  (lambda (args errf)
    (let ((type (first args))
	  (vars (second args))
	  (step (third args))
	  (stop (fourth args))
	  (body (fifth args))
	  (done (sixth args)))
      ;; #f or (<bindings)
      (if vars (set! vars (car vars)))
      (if done (set! done (cadr done)))
      (make-parse-unit Sal2LoopStatementRule
		       (list type vars step stop body done)
		       (parse-unit-position type))))
  (lambda (unit info errf)
    (sal-emit-iteration unit info errf ))
  )

(defrule Sal2FormalsRule
  (or (and SalLParen SalRParen)
      (and SalLParen SalOptKey SalBindRule (* SalComma SalBindRule) SalRParen)
      (and SalLParen SalIdentifier (* SalComma SalIdentifier) 
           SalOptKey SalBindRule (* SalComma SalBindRule) SalRParen)
      (and SalLParen SalIdentifier (* SalComma SalIdentifier) SalRParen)
      )

  (lambda (args errf)
    ;;(format #t "~S~%" (list :formals-> args))
    (let ((pars (list)) )
      ;; there are always at LEAST two args. if second is <)>
      ;; then we have no lambda args
      (cond ((token-unit-type=? (second args) SalRParen)
             (set! pars (list)) )
            ((token-unit-type=? (second args) SalOptKey) ; all optkey params
             (set! pars (cons (third args)
                              (remove-token-type (fourth args) SalComma)))
             ;; pars is two element list ( () (opts...))
             (set! pars (list (list) (emit pars (list) errf)))) 
            ((token-unit-type=? (fourth args) SalOptKey) ; mixed args
             ;; mix of required and optkey
             (let ((reqs (cons (second args)
                               (remove-token-type (third args) SalComma)))
                   (opts (cons (fifth args)
                               (remove-token-type (sixth args) SalComma))))
               ;; pars is a two element list ((reqs..) (opts...))
               (set! pars (list (emit reqs (list) errf)
                                (emit opts (list) errf)))
               ))
            (else ; only required args
             (let ((reqs (cons (second args)
                               (remove-token-type (third args) SalComma))))
               ;; pars is list of symbols
               (set! pars (emit reqs (list) errf))
               )))
      ;; the formals unit holds the emitted lambda bindings
      (make-parse-unit Sal2FormalsRule pars #f)
      ))
  #f
  )

(defrule Sal2FunctionStatementRule
  (and Sal2Function
       SalIdentifier
       Sal2FormalsRule
       (@ SalBindingsRule)
       (+ Sal2StatementRule)
       SalEnd
       )
  (lambda (args errf)
    ;; <FUNCTION> <name> <formals> #f (<statement>*) END
    ;;(format #t "~S~%" (list #:function-> args))
    (let ((name (second args))
	  (pars (third args))
          (vars (fourth args))
	  (body (fifth args))
          )
      ;; vars = #f | (<bindings>)
      (if vars (set! vars (car vars)))
      (make-parse-unit Sal2FunctionStatementRule
                       (list name pars vars body)
                       #f)) )
  (lambda (unit info errf)
    (let* ((data (parse-unit-parsed unit))
           (name (emit (first data) info errf))
           (pars (parse-unit-parsed (second data))) ; pars is list of formals (units)
           (vars (third data))
           (body (emit (fourth data) info errf)))
      (if vars
          (let ((bind (emit vars info errf)))
            (check-for-redefinition bind 'variable)
            (set! body `((let* , bind ,@body)))))
      (emit-function name pars body)
      ;; use define if no args or all are required
      )))

(define (emit-function name formals body)
  ;; formals is either (), (a b c) or ((required...) (optkey...))
  ;; wrap the appropriate function around body given the formals
  (if (or (null? formals) (symbol? (car formals)))
      ;; use define if no args or all required args
      (begin
        (check-for-redefinition formals 'parameter)
        `(define ,(cons name formals) ,@body)
        )
      (let ((reqs (first formals))
            (opts (second formals)))
        (check-for-redefinition reqs 'parameter)
        (check-for-redefinition opts 'parameter)
        ;; use define* if all optkey args
        (if (null? reqs)
            `(define* ,(cons name opts) ,@body)
            ;; else required AND optkey, use define and with-optkeys
            (let ((restarg (gensym "restarg")))
              ;; this breaks...
              ;;`(define (,name ,@required . , restarg) (with-optkeys (,restarg ,@optkeys) ,@body))
              `(define (,name ,@reqs . , restarg)
                 ,(append (list 'with-optkeys (cons restarg opts)) body))
              )))))

(defrule Sal2ProcessWaitRule (and Sal2Wait SalSexprRule)
  (simple-unit-parser Sal2ProcessWaitRule)
  (lambda (unit info errf)
    (let ((data (parse-unit-parsed unit)))
      (if (not (get-emit-info #:run info))
	  ( errf (make-parse-error "wait statement outside process block"
				   (parse-unit-position
				    (car data)))))
      (emit data info errf))))

(defrule Sal2ProcessStatementRule
  (and Sal2Process
       SalIdentifier
       Sal2FormalsRule 
       (@ SalBindingsRule)
       (* SalSteppingRule )
       (* SalTerminationRule)
       (+ Sal2StatementRule)  
       ;;(+ (or SalProcessWaitRule Sal2StatementRule)) 
       (@ SalFinally Sal2StatementRule)
       SalEnd)
  (lambda (args errf)
    (let ((type (first args))
          (name (second args))
          (pars (third args))
	  (vars (fourth args))
	  (step (fifth args))
	  (stop (sixth args))
	  (body (seventh args))
	  (done (eighth args)))
      ;; #f or (<bindings>)
      (if vars (set! vars (car vars)))
      (if done (set! done (cadr done)))
      (make-parse-unit Sal2ProcessStatementRule
		       (list type name pars vars step stop body done)
		       (parse-unit-position type))))
  (lambda (unit info errf)
    ;; remove name and formals from unit, emit loop body and wrap in
    ;; function.
    (let* ((data (parse-unit-parsed unit))
           (type (first data))
           (name (emit (second data) info errf))
           (pars (parse-unit-parsed (third data)))
           (body #f))
      ;; pop name and pars from (run...) expansion
      (parse-unit-parsed-set! unit (cons type (cdddr data)))
      (set! info (add-emit-info #:process #t info))
      (set! body (sal-emit-iteration unit info errf ))
      (emit-function name pars (list body))
    ))
  )


(defrule Sal2FileStatementRule
  (and Sal2File 
       (or SalString SalIdentifier)
       (or (and SalLParen SalRParen)
           (and SalLParen SalKeyparam SalSexprRule
                (* SalComma SalKeyparam SalSexprRule) SalRParen))
       (@ SalBindingsRule) 
       (* Sal2StatementRule)
       SalEnd)
  (lambda (args errf)
    ;;(format #t "~S~%" (list #:file-> args))
    (let ((file (second args) )
          (opts (third args)  )
          (vars (fourth args) )
          (body (fifth args)))
      ;; opts: ( <(> <)> )
      ;; opts: ( <(> <key> <expr> ( {<,> <key> <expr>}* ) <)> )
      (if (null? (cddr opts)) ; empty opts: ( <(> <)> )
          (set! opts (list))
          (set! opts (cons (second opts) 
                           (cons (third opts)
                                 (remove-token-type (fourth opts)
                                                    SalComma)))))
      (if vars (set! vars (first vars)))
      (make-parse-unit Sal2FileStatementRule
                       (list file opts vars body)
                     #f)))
  (lambda (unit info errf)
    (let* ((args (parse-unit-parsed unit))
           (file (emit (first args) info errf))
           (opts (emit (second args) info errf))
           (vars (if (third args) (emit (parse-unit-parsed (third args)) info errf) (list)))
           (body (emit (fourth args) info errf))
           (data (gensym "data")) ; data returned by open-output-file
           (flag (gensym "flag")) ; success flag
           )
      (check-for-redefinition vars 'variable)
      `(let ((,data ,0)
             (,flag #f))
         (dynamic-wind
             (lambda ()
               (set! ,data (open-file ,file ,@opts)))
             (lambda ()
               (let* ,vars
                 ,@body
                 (set! ,flag #t)
                 (void)))
             (lambda () (close-file ,data ,flag))
           ))))
  )

(defrule Sal2StatementRule
  (or Sal2BeginStatementRule
      Sal2IfStatementRule
      Sal2VariableStatementRule
      Sal2FunctionStatementRule
      Sal2LoopStatementRule
      Sal2ProcessStatementRule
      Sal2FileStatementRule
      SalAssignmentRule
      Sal2ProcessWaitRule    ;; but only allowed inside process
      SalSexprRule
      )
  #f
  #f)

(defrule Sal2StatementSequenceRule (+ Sal2StatementRule)
  (lambda (args errf)
    (make-parse-unit Sal2StatementSequenceRule
		     args (parse-unit-position (first args))))
  (lambda (unit info errf)
    (cons 'begin (emit (parse-unit-parsed unit) info errf))))

(define (sal2 str tokens expand multi)
  (if (pair? tokens)
      (let ((code #f)
            (rule (if multi Sal2StatementSequenceRule Sal2StatementRule))
            )
        (set! code (parse rule tokens #f 0 #f #f))
        (cond ((parse-error? code)
               (print-sal-error str code #f) ; signal error
               +s7-error+)
              (expand ;; macroexpanding
               (ffi_print_values (format #f "~S" code ))
               +s7-error+ ;; signal no value printing
               )
              (else ;; evaluation
               (eval code (interaction-environment))))) ;;(interaction-environment)
      +s7-error+))

;;;
;;; Sal Runtime Support
;;;

(define (print-sal-error str err err?)
  (let* ((len (string-length str))
	 (pos (parse-error-position err))
	 (beg (- pos 1))
	 (end 0)
	 )
    ;; isolate line containing error position
    (do ((f #f))
	((or f (= beg -1)) #f)
      (if (char=? (string-ref str beg) #\newline)
	  (set! f #t)
	  (set! beg (- beg 1))))
    ;; beg now 1+ nearest leftward #\Return or at 0
    (set! beg (+ beg 1))
    ;; find end of error line
    (set! end pos)
    (do ((f #f))
	((or f (= end len)) #f)
      (if (char=? (string-ref str end) #\newline)
	  (set! f #t)
	  (set! end (+ end 1))))
    (let ((msg (string-append (parse-error-string err)
                              (make-string 1 #\newline)
                              (substring str beg end) 
                              (make-string 1 #\newline)
                              (let* ((siz (- pos beg))
                                     (pad (make-string (+ siz 2) #\space)))
                                (string-set! pad siz #\^)
                                (string-set! pad (+ siz 1) #\newline)
                                pad))))
      (if (not err?)
          (begin (print-error (string-append ">>> Error: " msg))
                 (values))
          (error msg)))))
      
(define (sal:print . args)
  (ffi_cm_print args))

(define (sal:chdir path)
  (chdir path)
  (sal:print "Directory: \"" (pwd) "\"")
  (values))

(define (sal:load file)
;;  (if (equal? "sal" (pathname-type file))
;;      (load-sal-file (full-pathname file))
;;      (ffi_load file))
  (ffi_load file)
  (print-output (string-append "Loaded " file "\n" ))
  )

;;(define (load-sal-file file)
;;  (let ((toks #f)
;;        (args #f))
;;    (dynamic-wind (lambda () #f)
;;        (lambda ()
;;          (set! toks (ffi_sal_allocate_tokens)) ; alloc C array for tokens
;;          (set! args (ffi_sal_tokenize_file file toks #f))
;;          (if (pair? args)
;;              (sal (car args) (cdr args))))
;;        (lambda () (if toks (ffi_sal_free_tokens toks))))))

(define (sal-enums enum spec c?)
  (let ((num #f)
	(nam #f)
	(lsh #f))
    (cond ((pair? enum)
	   (set! nam (car enum))
	   (set! lsh (cadr enum)))
	  (else 
	   (set! nam enum)
	   (set! lsh 0)))
    (if c?
	(format #t "~%  enum ~A {~%    // generated by (sal-enums )~%" nam)
	(format #t "~%(begin~%  ;; generated by (sal-enums)~%"))
    (do ((tail spec (cdr tail))
	 (this #f)
	 (name #f)
	 (data #f)
	 (enum #f))
	((null? tail)
	 #f)
      (set! this (car tail))
      (if (pair? this)
	  (begin (set! name (car this))
		 (cond ((number? (cadr this))
			(set! num (cadr this)))
		       ((eq? (cadr this) '+)
			(set! num (+ num 1)))
		       ((eq? (cadr this) '*)
			(set! num num))
		       (else (error "second element ~A not num * or +"
				    (cadr this))))
		 (if (not (null? (cddr this)))
		     (set! data (caddr this))
		     (set! data 0)))
	  (begin
	    (set! name this)
	    (set! num (+ num 1))
	    (set! data 0)))
      (set! enum (+ (ash num lsh) data))
      (if (not c?)
	  (format #t "  (define ~A #x~X)~%" name enum)
	  (begin 
	    (format #t "    ~A = 0x~X" name enum)
	    (if (not (null? (cdr tail))) (format #t ","))
	    (format #t "~%"))))
    (if c? (format #t "  };~%")
	(format #t "  )~%"))
    )
  #t
  )


#|

(define SalTypes
  `((SalUntyped 0)
    (SAL_TOKEN_BEG *) ; * means dont increment number
    (SAL_DELIM_BEG *)
    SalComma
    SalLParen
    SalRParen
    SalLCurly
    SalRCurly
    SalLBrace
    SalRBrace
    SAL_DELIM_END
    ;; Data
    (SAL_DATA_BEG *)
    SalString
    SAL_NUMBER_BEG
    SalInteger
    SalRatio
    SalFloat
    SAL_NUMBER_END
    SalKeyword
    SAL_BOOL_BEG
    ;; Special (hash) Notations
    (SAL_HASH_BEG *)
    SalTrue
    SalFalse
    SAL_BOOL_END
    SalQMark
    SalUnquote
    SalSplice
    SAL_HASH_END
    (SAL_DATA_END *)
    ;; Named Parameters
    SalKeyparam
    ;; Classes
    SalClass
    ;; Operators... (data: op_weight)
    SAL_OP_BEG
    (SalPlus + 5)
    (SalMinus + 5)
    (SalTimes + 6)
    (SalDivide + 6)
    (SalMod + 5)
    (SalExpt + 7)
    (SalAnd + 2)
    (SalOr  + 1)
    (SalNot + 3)
    ;; ...relations
    SAL_RELATION_BEG
    (SalLess + 4)
    (SalGreater + 4)
    (SalNotEqual + 4)
    (SalGeneralEqual + 4)
    SAL_ASSIGNMENT_BEG
    (SalEqual + 4)
    (SalLessEqual  + 4)
    (SalGreaterEqual  + 4)
    SAL_RELATION_END
    (SAL_OP_END *)
    ;; ...assignment (starts at SalEqual)
    SalInc
    SalMul
    SalCol
    SalPre
    SalApp
    SAL_ASSIGNMENT_END
    ;; Literals... (data: command=110, statement=010 clausal=001)
    (SAL_LITERAL_BEG *)
    (SAL_STATEMENT_BEG *)
    (SAL_COMMAND_BEG *)
    ;; commands (statement starting word)
    (SalBegin  + 16)  ; 16 = SalOpen
    SalChdir
    SalDefine
    SalExec
    SalIf
    SalLoad
    (SalLoop  + 16)
    SalPlot
    SalPrint
    (SalRun  + 16)
    SalSend
    SalSet
    (SalSoundFile + 16)
    (SalFomusFile + 16)
    SalSprout

    Sal2Variable
    (Sal2Function + 16)
    (Sal2Process + 16)
    (Sal2If + 16)
    (Sal2File + 16)
    Sal2Wait

    SAL_COMMAND_END
    (SAL_CONSTITUENT_BEG *)
    ;; ...statement constituent parts
    SalElse
    (SalEnd    + 17) ; 17 = blockClose
    SalReturn
    SalThen
    SalUnless
    SalUntil
    SalWait
    SalWhen
    SalWhile
    SalWith
    SalOptKey
    SAL_CONSTITUENT_END
    (SAL_STATEMENT_END *)
    (SAL_CLAUSAL_BEG *)
    ;; ...clausals
    SalAbove
    SalBelow
    SalBy
    SalDownto
    SalFinally
    SalFor
    SalFrom
    SalIn
    SalOver
    SalRepeat
    SalTo
    SAL_CLAUSAL_END
    (SAL_LITERAL_END *)
    ;; ...define types
    (SAL_DEFINE_BEG *)
    SalFunction
    SalProcess
    SalVariable
    SAL_DEFINE_END
    ;; Identifiers (variables, functions, obj.slot)
    (SAL_IDENTIFIER_BEG *)
    SalIdentifier
    SalSlotRef
    SAL_IDENTIFIER_END
    (SAL_TOKEN_END *)
    ;; RULES
    (SAL_RULE_BEG *)
    SalNumberRule
    SalBoolRule
    SalAtomRule
    SalListRule
    SalEltRule
    SalArefRule
    SalIfExprRule
    SalUnquoteRule
    SalFuncallRule
    SalFunargsRule
    SalPargsRule
    SalKargsRule
    SalOpRule
    SalMexprRule
    SalTermRule
    SalSexprRule
    SalBindingsRule
    SalBindRule

    SalAssignmentRule
    SalAssignRule
    SalAssignerRule
    SalSetRule
    SalFunctionReturnRule
    SalProcessWaitRule
    SalBlockRule
    SalConditionalRule
    SalLoopStatementRule
    SalRunStatementRule
    SalSteppingRule
    SalTerminationRule
    SalPrintStatementRule
    SalExecStatementRule
    SalSendStatementRule
    SalSoundFileStatementRule
    SalFomusFileStatementRule
    SalSproutStatementRule
    SalLoadStatementRule
    SalChdirStatementRule
    SalPlotStatementRule
    SalDefineStatementRule
    SalStatementRule
    SalStatementSequenceRule
    SalDeclarationRule
    SalVarDeclRule
    SalFunDeclRule
    SalProcDeclRule
    SalProcessBodyRule

    Sal2FormalsRule
    Sal2BeginStatementRule
    Sal2IfStatementRule
    Sal2FunctionStatementRule
    Sal2VariableStatementRule
    Sal2WithStatementRule
    Sal2LoopStatementRule
    Sal2ProcessStatementRule
    Sal2ProcessWaitRule
    Sal2FileStatementRule
    Sal2StatementRule
    Sal2StatementSequenceRule

    SAL_RULE_END
    (SAL_TYPE_END *)
 )
)

;; data bits enum (bits 0-7 are operator weights)
(sal-enums 'SalTypeData '((SalBlockOpen 16) SalBlockClose) #f)
(sal-enums 'SalTypeData '((SalBlockOpen 16) SalBlockClose) #t)
;; sal parser types
(sal-enums '(SalTypes 8) SalTypes #f)
(sal-enums '(SalTypes 8) SalTypes #t)
|#

