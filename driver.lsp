					;driver for GENOVA II
;setup
(load *core-engine*) 
(load *ga-functions-file*)
(init-params) 

(defvar population (populate *population-size* *genome-length*))
(defun most-fit ()
  (with-slots (fitness) (aref population 0) fitness))

;randomize and rate progenitors
(initialize-population population)
(rate-fitness population)

;evolve
(loop for i from 0 to *max-generations* while (< (most-fit) *terminating-fitness*) do
     (setf population (procreate (make-selection-pool population) population))
     (mutate population)
     (rate-fitness population)
     (sort population #'> :key #'fitness))

     ;uncomment below and delete last parenthesis above to print genomes 
     ;(format t "generation ~a" i)
     ;(loop for individual across population do
	;  (print (slot-value individual 'genome))))
     

(format t "most fit individual: ~%")
(print (slot-value (aref population 0) 'genome))
