all: miniml expr evaluation expr_tests eval_tests
tests: expr_tests eval_tests

miniml: miniml.ml 
	ocamlbuild -use-ocamlfind miniml.byte	

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr_tests: expr_tests.ml
	ocamlbuild -use-ocamlfind expr_tests.byte

eval_tests: eval_tests.ml
	ocamlbuild -use-ocamlfind eval_tests.byte

clean:
	rm -rf _build *.byte