;GENOVA II

;simple genetic algorithm in LISP
;instead of "scripts", just call external LISP functions at runtime. Simple, elegant, portable.
;first version: fixed population and individual sizes, all "genes" are bits

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
