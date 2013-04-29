all: simplex

# These must be in the right order--no forward refs
FILES = Order.ml Helpers.ml Elts.ml MatrixI.ml Matrix.ml Simplex.ml Interface.ml

simplex: $(FILES)
	ocamlc -g -o simplex $(FILES)

clean: 
	rm -f simplex *.cmi *.cmo
