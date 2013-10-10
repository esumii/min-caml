# added by Sumii

SLDS = ball contest cup dra lattice mange mir orange piero1 piero2 planet \
shuttle test test2 test3 tileball tron tsu tsu2 tsu3

.PRECIOUS: %.cmp %.diff.gcc-GCC.ppm %.diff.ocamlopt-gcc.ppm %.mincaml.ppm %.gcc.ppm %.GCC.ppm %.ocamlopt.ppm %.ocamlc.ppm

test: $(SLDS:%=%.cmp) $(SLDS:%=%.diff.ocamlopt-gcc.ppm) $(SLDS:%=%.diff.gcc-GCC.ppm)

clean:
	rm -f *.cmp *.ppm *.ppm.tmp ppmdiff raytrace RayTrace *.sld_ *.cmo *.cmx *.cmi *.o min-rt_b min-rt min-rt.s min-rt.min-caml

%.cmp: %.ocamlc.ppm %.mincaml.ppm # avoid 80-bit float on x86
	diff $^ > $@
%.diff.gcc-GCC.ppm: ppmdiff %.gcc.ppm %.GCC.ppm
	./$^ $@
%.diff.ocamlopt-gcc.ppm: ppmdiff %.ocamlopt.ppm %.gcc.ppm
	./$^ $@
ppmdiff: ppm.ml diff.ml
	ocamlopt $^ -o $@

%.mincaml.ppm: %.sld min-rt.min-caml
	time ./min-rt.min-caml < $< > $@.tmp
	mv $@.tmp $@
min-rt.min-caml: min-rt.s globals.s ../libmincaml.S ../stub.c
	gcc -m32 -g -O2 -Wall $^ -lm -o min-rt.min-caml
min-rt.s: min-rt.ml ../min-caml
	../min-caml -inline 100 min-rt

%.gcc.ppm: %.sld raytrace
	echo 768 > $<_
	cat $< >> $<_
	time ./raytrace < $<_ > $@.tmp
	mv $@.tmp $@
raytrace: raytrace.c
	gcc -m32 -Wall -O3 raytrace.c -lm -o raytrace

%.GCC.ppm: %.sld RayTrace
	time ./RayTrace -s -w 768 -h 768 < $< > $@.tmp
	mv $@.tmp $@
RayTrace: RayTrace.cxx
	g++ -m32 -Wall -O3 RayTrace.cxx -lm -o RayTrace

%.ocamlopt.ppm: %.sld min-rt
	time ./min-rt < $< > $@.tmp
	mv $@.tmp $@
%.ocamlc.ppm: %.sld min-rt_b
	./min-rt_b < $< > $@.tmp # to avoid 80-bit float on x86
	mv $@.tmp $@

######################################################################

# original by Oiwa follows (modified a little by Sumii)

native: min-rt
bytecode: min-rt_b

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	ocamlc -g -c -pp ./preprocess.sh $<

.ml.cmx:
	ocamlopt -unsafe -inline 100 -c -pp ./preprocess.sh $<

.mli.cmi:
	ocamlc -g -c $<

min-rt.cmx: globals.cmx miniMLRuntime.cmx
min-rt.cmo: globals.cmo miniMLRuntime.cmi
globals.cmx: miniMLRuntime.cmx
globals.cmo: miniMLRuntime.cmi

min-rt_b: min-rt.cmo globals.cmo miniMLRuntime.cmo
	ocamlc -g -o min-rt_b miniMLRuntime.cmo globals.cmo min-rt.cmo 

min-rt: min-rt.cmx globals.cmx miniMLRuntime.cmx
	ocamlopt -unsafe -inline 100 -o min-rt miniMLRuntime.cmx globals.cmx min-rt.cmx 

miniMLRuntime.cmi: miniMLRuntime.mli
	ocamlc -c miniMLRuntime.mli

miniMLRuntime.cmx: miniMLRuntime.ml miniMLRuntime.cmi
	ocamlopt -unsafe -inline 100 -c miniMLRuntime.ml

miniMLRuntime.cmo: miniMLRuntime.ml miniMLRuntime.cmi
	ocamlc -g -c miniMLRuntime.ml
