SOURCES = core.ml mapq.mli mapq.ml UID.ml globular.ml polygraph.ml formal.ml toplevel.ml
RESULT = polyr
LIBS = unix
ANNOTATE = true
OCAMLFLAGS = -warn-error +5
DOC_FILES += $(filter %.ml, $(SOURCES))
OCAMLDOCFLAGS = -sort -stars -short-functors
OCAMLDOC = ocamldoc.opt
OCAMLDOCFLAGS = -charset utf-8

all: dnc
	./polyr test.polyr -b

include OCamlMakefile
