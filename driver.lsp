					;driver for GENOVA II
;setup
(load "core.fasl")
(init-params)
(load *ga-functions-file*)
(defvar population populate *population-size* *genome-length*))
(defun most-fit ()
  (with-slots (fitness) (aref population 0) fitness)

;randomize progenitors
(initialize-population population)

;evolve
(loop for i from 0 to *max-generations* while (< most-fit *terminating-fitness*) do
      ((setf population (procreate (make-selection-pool population)))
      (mutate population)
      (set-fitness population)
      (sort population #'> :key #'fitness)))

(format t "most fit individual: ~%")
(slot-value (aref population 0) 'genome)
