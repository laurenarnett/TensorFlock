PROJECT ?= TensorFlock 
SRC_DIR ?= src/
PROJECT_TOP ?= toplevel 
PROJECT_EXTENSION ?= src/toplevel.native
OCAML_SENTINAL ?= .ocaml-sentinal
OPAM_FILE ?= opam

$(OCAML_SENTINAL): $(OPAM_FILE)
	opam pin add --no-action $(PROJECT) . -y
	opam install --deps-only $(PROJECT)
	touch $@

$(PROJECT_EXTENSION): $(OCAML_SENTINAL) clean $(SRC_DIR)* 
	ocamlbuild $@

test: $(OCAML_SENTINAL)
	echo "This is a test that will pass"

clean: 
	ocamlbuild -clean

.PHONY: test clean
