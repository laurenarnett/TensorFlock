PROJECT ?= TensorFlock 
PROJECT_TOP ?= toplevel 
OCAML_SENTINAL ?= .ocaml-sentinal
OPAM_FILE ?= opam
SRC_DIR ?= ./src/

$(OCAML_SENTINAL): $(OPAM_FILE)
	opam pin add --no-action $(PROJECT) .
	opam install --deps-only $(PROJECT)
	touch $@

$(SRC_DIR)$(PROJECT_TOP).native: $(OCAML_SENTINAL) clean $(PROJECT_TOP).ml 
	ocamlbuild $@

test: $(OCAML_SENTINAL)
	echo "This is a test that will pass"

clean: 
	ocamlbuild -clean

.PHONY: test clean
