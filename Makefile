PROJECT ?= TensorFlock 
SRC_DIR ?= src/
PROJECT_TOP ?= toplevel 
PROJECT_EXTENSION ?= src/toplevel.native
OCAML_SENTINAL ?= .ocaml-sentinal
PROJECT_PARSER ?= parser
OPAM_FILE ?= opam

$(OCAML_SENTINAL): $(OPAM_FILE)
	opam pin add --no-action $(PROJECT) . -y
	opam install --deps-only $(PROJECT)
	touch $@

$(PROJECT_EXTENSION): $(OCAML_SENTINAL) clean $(SRC_DIR)* 
	ocamlbuild $@

state: $(OCAML_SENTINAL) clean
	ocamlyacc -v $(SRC_DIR)$(PROJECT_PARSER).mly

test: $(PROJECT_EXTENSION) $(OCAML_SENTINAL)
	bash ./test_runner.sh

clean: 
	ocamlbuild -clean
ifneq ($(wildcard $(SRC_DIR)$(PROJECT_PARSER).ml),)
	rm $(SRC_DIR)$(PROJECT_PARSER).ml
endif
ifneq ($(wildcard $(SRC_DIR)$(PROJECT_PARSER).mli),)
	rm $(SRC_DIR)$(PROJECT_PARSER).mli
endif
ifneq ($(wildcard $(SRC_DIR)$(PROJECT_PARSER).output),)
	rm $(SRC_DIR)$(PROJECT_PARSER).output
endif

.PHONY: state test clean
