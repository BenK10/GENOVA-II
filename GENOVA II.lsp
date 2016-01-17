;GENOVA II

;simple genetic algorithm in LISP
;instead of "scripts", just call external LISP functions at runtime. Simple, elegant, portable.
;first version: fixed population and individual sizes, all "genes" are bits

;globals
(defvar *params-file*) ;TODO passed from cmd line

;set from params file
(defvar *population-size*)
(defvar *genome-length*)
(defvar *max-generations*)
(defvar *selection-function*)
(defvar *crossover-function*)
(defvar *mutation-function*)
(defvar *fitness-function*)

;initialize parameters
(defun init-params ()
  (with-open-file (in-file *params-file*)
       (loop while in-file do
	 (let ((param-name (read in-file nil))
	       (param-value (read in-file nil)))
	    (cond ((equalp "population-size" (princ-to-string param-name))
			 (setf *population-size* param-value))
		 ((equalp "genome-length" (princ-to-string param-name))
		         (setf *genome-length* param-value))
		 ((equalp "max-generations" (princ-to-string param-name))
			 (setf *max-generations* param-value))
		 ((equalp "selection-function" (princ-to-string param-name))
		         (setf *selection-function* param-value))
		 ((equalp "crossover-function" (princ-to-string param-name))
        		 (setf *crossover-function* param-value))
		 ((equalp "mutation-function" (princ-to-string param-name))
		         (setf *mutation-function* param-value))
		 ((equalp "fitness-function" (princ-to-string param-name))
		  (setf *fitness-function* param-value))
		 ((or (equalp param-name nil) (equalp param-value nil))
		  (return)))))))

;genome class
(defclass individual () 
  ((fitness 
    :initform 0)
	(genome)))

(defmethod initialize-instance :after ((indi individual) &key genome-length)
  (setf (slot-value indi 'genome) (make-array genome-length :element-type 'bit)))

;randomizes individual's genome
(defmethod randomize-genome ((indi individual))
  (let ((v (slot-value indi 'genome)))
  (loop for i from 0 to (- (array-dimension v 0) 1) do
       (setf (aref v i) (random 2 )))))

;create population vectors
(defun populate (population-size genome-length) 
  (let ((pop (make-array population-size :element-type 'individual)))
  (loop for i from 0 to (- (array-dimension pop 0) 1) do
	(setf (aref pop i ) (make-instance 'individual :genome-length genome-length)))
    pop))


;randomizes population's genomes
(defun initialize-population (population)
  (loop for i from 0 to (- (array-dimension population 0) 1) do
       (randomize-genome (aref population i))))

;call a function by supplied string
(defun call-by-string (function-name)
  (funcall (symbol-function (find-symbol (string-upcase function-name)))))

