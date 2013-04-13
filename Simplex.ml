module type SIMPLEX
sig

  type elt
  type linear_equation
  type constraint

	val pivot : matrix -> matrix

	val constraint_from_string : string -> constraint
	
<<<<<<< HEAD:simplex.ml
end
=======
	val linear_from_string : string -> constraint

end

>>>>>>> de40903d572f839c7f1375bd8dbcaf9fa8e49723:Simplex.ml
