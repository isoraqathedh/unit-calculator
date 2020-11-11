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

;;; Operations
(defun perform-operation* (func argument-count stack)
  (let ((result (apply func (subseq stack 0 argument-count))))
    (loop repeat argument-count
          do (pop stack))
    (push result stack)))

(defun perform-operation (func argument-count)
  (setf *stack* (perform-operation* func argument-count *stack*)))

;;; Something
(defun enter (thing)
  (push thing *stack*))

(defun unit-reduce-function (op-symbol)
  (lambda (x y)
    (reduce-unit (list op-symbol x y))))
