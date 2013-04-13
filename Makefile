all: simplex

# These must be in the right order--no forward refs
FILES = Order.ml Elts.ml Matrix.ml Simplex.ml

simplex: $(FILES)
	ocamlc -g -o simplex $(FILES)

clean: 
	rm -f simplex *.cmi *.cmo
