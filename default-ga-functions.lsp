;default genetic algorithm functions

;fitness-proportionate selection
(defun select (population)
  (let  ((population-fitness (loop for individual across population sum (with-slots (fitness) individual fitness)))
         (selection-pool (make-array population-fitness :fill-pointer 0 )))
		 (loop for i from 0 to (- (array-dimension population 0) 1) do
		   (dotimes (with-slots (fitness) individual fitness) 
		     (vector-push selection-pool i)))
	selection-pool))
			 
;single-point crossover
(defun crossover (parent-a parent-b)
  (let ((cross-point (random *genome-length*))
        (child (make-array *genome-length* :element-type 'bit)))
		(loop for i from 0 to cross-point do
		  (setf (aref child i) (aref parent-a i)))
		(loop for i from (+ cross-point 1) to (- *genome-length* 1) do
		  (setf (aref child i) (aref parent-b i)))
		child))
		
;mutation: flip random bit
(defun mutate (genome)
 (let ((idx (random *genome-length*))
        (bit (aref genome idx)))
    (setf (aref genome idx) (* (- bit 1) -1))))

;silly fitness: count 1 bits
(defun fitness (genome)
  (count-if #'(lambda (x) (= x 1)) genome)	
	