test: test-huffman
clean:
	rm -f *.s *.min-caml *.ocamlopt *.gcc *.gcc64 *.cmi *.cmx *.o *.out

test-huffman: huffman.min-caml huffman.ocamlopt huffman.gcc huffman.gcc64
	time ./huffman.min-caml > huffman.min-caml.out < huffman.int
	time ./huffman.ocamlopt > huffman.ocamlopt.out < huffman.int
	time ./huffman.gcc > huffman.gcc.out < huffman.int
	time ./huffman.gcc64 > huffman.gcc64.out < huffman.int
	diff huffman.min-caml.out huffman.ocamlopt.out
	diff huffman.min-caml.out huffman.gcc.out
	diff huffman.min-caml.out huffman.gcc64.out

huffman.ocamlopt: huffman.ml
	ocamlopt -pp ../min-rt/preprocess.sh -unsafe -inline 100 huffman.ml -o huffman.ocamlopt

huffman.min-caml: huffman.ml ../min-caml
	../min-caml -inline 100 huffman
	gcc -m32 -g -O2 -Wall huffman.s ../libmincaml.S ../stub.c -lm -o huffman.min-caml

huffman.gcc: huffman.c
	gcc -m32 -O3 -Wall huffman.c -o huffman.gcc

huffman.gcc64: huffman.c
	gcc -m64 -O3 -Wall huffman.c -o huffman.gcc64
