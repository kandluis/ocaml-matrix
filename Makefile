all: moogle

# These must be in the right order--no forward refs
FILES = elts.ml matrix.ml simplex.ml

moogle: $(FILES)
	ocamlc -g -o moogle $(FILES)

clean: 
	rm -f simplex *.cmi *.cmo
