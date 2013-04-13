module type simplex
sig

	type elt
	type linear_equation
	type constraint

	val pivot : matrix -> matrix
	val constraint_from_string : string -> constraint
	val linear_from_string : string -> constraint
	
end