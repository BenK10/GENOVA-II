;GENOVA II

;simple genetic algorithm in LISP
;first version: fixed population and individual sizes, all "genes" are bits

;globals
(defvar *params-file*) ;TODO passed from cmd line
(defvar *ga-functions-file*) ;user-defined GA functions

;set from params file
(defvar *population-size*)
(defvar *genome-length*)
(defvar *max-generations*)
(defvar *selection-function*)
(defvar *crossover-function*)
(defvar *mutation-function*)
(defvar *mutation-chance*)
(defvar *fitness-function*)
(defvar *terminating-fitness*)

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
		 ((equalp "mutation-chance" (princ-to-string param-name))
		  (setf *mutation-chance* param-value))
		 ((equalp "fitness-function" (princ-to-string param-name))
		  (setf *fitness-function* param-value))
		 ((equalp "terminating-fitness" (princ-to-string param-name))
		  (setf *terminating-fitness* param-value))
		 ((equalp "ga-functions-file" (princ-to-string param-name))
		  (setf *ga-functions-file* param-value))
		 ((or (equalp param-name nil) (equalp param-value nil))
		  (return)))))))

;genome class
(defclass individual () 
  ((fitness 
    :initform 0
    :reader fitness)
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
	(setf (aref pop i) (make-instance 'individual :genome-length genome-length)))
    pop))


;randomizes population's genomes
(defun initialize-population (population)
  (loop for i from 0 to (- (array-dimension population 0) 1) do
       (randomize-genome (aref population i))))

;call a function by supplied string
(defmacro call-by-string (function-name &rest rest)
  `(funcall (symbol-function (find-symbol (string-upcase ,function-name))) ,@rest))

;make pool of candidates. Pool is indices of candidates; candidates may appear multiple times
(defun make-selection-pool (population)
  (call-by-string *selection-function* population))

;produce the next generation
(defun procreate (selection-pool population)
  (let ((descendants (populate *population-size* *genome-length*)))
    (loop for i from 0 to (- (array-dimension descendants 0) 1) do
	 (let* ((idx-a  (aref selection-pool (random (array-dimension selection-pool 0))))
		(idx-b  (aref selection-pool (random (array-dimension selection-pool 0))))
		(parent-a (slot-value (aref population idx-a) 'genome))
		(parent-b (slot-value (aref population idx-b) 'genome)))
	   (setf (slot-value (aref descendants i) 'genome)
		 (call-by-string *crossover-function* parent-a parent-b))))
    descendants))

;mutate random individuals
(defun mutate (population)
  (let* ((population-mutation-chance (* *mutation-chance* *population-size*))
	 (expected-mutants (if (< population-mutation-chance 100)
			       1
			       (floor (/ population-mutation-chance 100)))))
    (if (< (random 101) population-mutation-chance)
	(loop for i from 0 to expected-mutants do
	(let* ((rand-idx (random (array-dimension population 0)))
	       (mutant (slot-value (aref population rand-idx) 'genome)))
	(setf (slot-value (aref population rand-idx) 'genome)
	      (call-by-string *mutation-function* mutant)))))))

;score individuals' fitnesses
(defun rate-fitness (population)
  (loop for i from 0 to (- (array-dimension population 0) 1) do
       (setf (slot-value (aref population i) 'fitness)
	     (call-by-string *fitness-function* (slot-value (aref population i) 'genome)))))

