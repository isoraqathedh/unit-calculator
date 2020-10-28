(in-package #:info.isoraqathedh.unit-calculator)

(defvar *stack* (make-list 0)
  "Stack for the calculator.")

(defun display-stack (&optional (stack *stack*))
  (with-output-to-string (s)
    (format s "~&Stack:")
    (if (null stack)
        (format s "~&<empty>")
        (loop for i in stack
              for counter from 1
              do (format s "~&~3d: ~a" counter i)))))

(define-condition calculator-error (error) ())

(define-condition not-enough-elements (calculator-error)
  ((has :reader has
        :initarg :has)
   (required :reader required
             :initarg :required))
  (:report (lambda (c s)
             (format s "Not enough elements on stack: ~d/~d"
                     (has c) (required c)))))

(define-condition incompatible-arguments (calculator-error)
  ((args :reader args
         :initarg :args)
   (op :reader op
       :initarg :op))
  (:report (lambda (c s)
             (format s "Incompatible arguments for operation ~s on ~s"
                     (op c) (args c)))))

(defun ensure-stack-depth (depth &optional (stack *stack*))
  (unless (<= depth (length stack))
    (error 'not-enough-elements :has (length stack) :required depth)))

(defun perform-operation (func argument-count &option (stack *stack*))
  (let ((result (apply func (subseq stack 0 argument-count))))
    (loop repeat argument-count do (pop stack))
    (push result stack)))

;;; Operations

;;; Conversion

;;; Addition
(defgeneric c+ (arg-1 arg-2)
  (:method ((arg-1 unit) (arg-2 unit))
    (or (same-unit-p arg-1 arg-2 :factor t)
        (error 'incompatible-arguments :op #'c+ :args (list arg-1 arg-2))))
  (:method ((arg-1 number) (arg-2 unit))
    (+ arg-1 (handler-case (convert-unit arg-2 nil)
               (simple-error (error 'incompatible-arguments :op #'c+ :args (list arg-1 arg-2))))))
  (:method ((arg-1 unit) (arg-2 number))
    (+ (convert-unit arg-1 nil) arg-2))
  (:method ((arg-1 number) (arg-2 number))
    (+ arg-1 arg-2)))
