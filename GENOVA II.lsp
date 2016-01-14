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

;example use: (make-instance 'individual :genome-length 10)  


;population vectors
(setf popA (make-array population-size :element-type 'individual))
(setf popB (make-array population-size :element-type 'individual))

;globals (supplied to command line)
;population size
;genome size
;max generations
;functions - selection, crossover, mutation, fitness
	
;;TODO:
;;default "construction" using make-array of individuals. use macro?
;;eg, with macro can say make-array <size> element-type: 'individual <genome size>
;;call it make-population
;;also need randomize-population (use map to assign randoms to population individuals)
;;selection, crossover, mutation, population vectors

;;globals: command-line supplied functions (called the same way as built-ins),

;;how to export as standalone executable? How to pass and parse command line args?

;;put globals and main program in another file?
	
;;etc
;---------
;call function passed as string
;(funcall (symbol-function (find-symbol (string-upcase "foo"))))
;can also use APPLY instead of FUNCALL
;"foo" can be replaced by a string variable